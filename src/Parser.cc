
/*
  the grammar so far

  term := primary
        | primary binop primary   # a binop

  primary := primitive
           | primitive primary    # a call
           | unop primitive       # a unop

  primitive := identifier         # a variable
             | 'nil'
             | 'Nil'
             | integer
             | 'Int'
             | 'true'
             | 'false'
             | 'Bool'
             | '\\' identifier (: term)? => term  # a procedure literal.
             | 'if' term 'then' term 'else' term  # a conditional expression.
             | 'while' term' 'do' term            # an interation.

  identifier := [a-zA-Z_][a-zA-Z0-9_]*
  integer    := [0-9]+

  binop := operator
  unop  := operator

  operator := [+-/*%<>:=&@!~|$^]+


  the kernel so far includes
    integer operators + - unop(-) * / %
    type    operator  ->

*/

#include <iostream>
using std::cout;
#include <string>
using std::string;
using std::stoi;
#include <stack>
using std::stack;
#include <vector>
using std::vector;
#include <set>
using std::set;
#include <memory>
using std::unique_ptr;
using std::move;
#include <utility>
using std::optional;
using std::pair;
using std::get;

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
using llvm::Type;

#include "Parser.hh"
#include "Ast.hh"
#include "Lexer.hh"
#include "BinopTable.hh"
#include "Location.hh"
#include "Error.hh"


Parser::Parser(const SymbolTable* const top, const BinopTable* const binops, const UnopTable* const Unops)
  : binops(binops), unops(unops)
{
  /*
    we need to utilize a stack in order to
    express the nesting of scopes naturally.
  */
  scopes.push(top);
  reset();
}

void Parser::reset()
{
  while (marks.size() > 0)
    marks.pop();
  tokbuf.clear();
  txtbuf.clear();
  locbuf.clear();
  curidx = 0;
}

int Parser::mark()
{
  marks.push(curidx);
  return curidx;
}

void Parser::release()
{
  curidx = marks.top();
  marks.pop();
}

bool Parser::speculating()
{
  return marks.size() > 0;
}

void Parser::gettok(int i)
{
  if (curidx + i > tokbuf.size()) // do we need more tokens than we have?
  {
    int n = (curidx + i) - tokbuf.size(); // how many tokens do we need?

    for (int j = 0; j < n; j++)
    {
      // without string here,
      // gdb cannot print strings...
      auto    tok = lexer.yylex();
      string* str = lexer.yytxt();
      auto    loc = lexer.yyloc();
      tokbuf.push_back(tok);
      txtbuf.push_back(*str);
      locbuf.push_back(loc);
    }
  }
}

void Parser::nextok()
{
  curidx += 1;

  if (curidx == tokbuf.size() && !speculating()) {
    reset();
  }

  gettok(1);
}

Token Parser::curtok()
{
  return tokbuf[curidx];
}

string Parser::curtxt()
{
  return txtbuf[curidx];
}

Location Parser::curloc()
{
  return locbuf[curidx];
}

bool Parser::is_unop(const string& op)
{
  auto unop = unops.find(op);
  if (unop != unops.end())
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool Parser::is_binop(const string& op)
{
  auto binop = binops.find(op);
  if (binop)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool Parser::is_primary(Token t)
{
  switch(t)
  {
    case Token::Nil: case Token::Int:
    case Token::True: case Token::False:
    case Token::Id: case Token::If:
    case Token::Backslash: case Token::Operator:
    case Token::LParen: case Token::TypeNil:
    case Token::TypeInt: case Token::TypeBool:
    case Token::While:
      return true;
    default:
      return false;
  }
}

bool Parser::is_ender(Token t)
{
  switch(t)
  {
    case Token::RParen: case Token::End:
    case Token::NewLn: case Token::EqRarrow:
    case Token::Then: case Token::Else:
    case Token::Do: case Token::Comma:
      return true;
    default:
      return false;
  }
}

bool Parser::speculate(Token t)
{
  if (t == curtok())
  {
    nextok();
    return true;
  }
  else
  {
    return false;

  }
}

optional<unique_ptr<Ast>> Parser::parse(const string& text)
{
  optional<unique_ptr<Ast>> result;
  reset();

  lexer.set_buffer(text);

  gettok(1);

  if (curtok() == Token::End)
  {
    return optional<unique_ptr<Ast>>(EmptyNode());
  }

  /*
    #! use-before-definition
    we could make one of the invariants
    of terms which make use of a name before
    it is defined: being speculatively parseable.
    this at least ensures that things which look
    like operators, but may not be defined, are
    still where we expect them, as with identifiers,
    but we still want to reject truly malformed terms.
  */
  mark();
  optional<ParserError> err_or = speculate_term();
  release();

  /*
  minor note: it is sort of weird that
  we are using optionals in the reverse
  of what they are usually, but given
  we are constructing an error type or not,
  i feel it still algins with the intended
  usecase, "we are either given an error,
  or we are given nothing, i.e. success."
  */
  if (err_or)
  {
    cout << buildErrStr(*err_or, text);
    result = optional<unique_ptr<Ast>>();
  }
  else
    result = optional<unique_ptr<Ast>>(move(parse_term()));

  return result;
}

unique_ptr<Ast> Parser::parse_term()
{
  unique_ptr<Ast> lhs, rhs;

  /*
  term := primary
        | primary binop primary

  primary := atom
           | atom atom
           | unop atom

  atom := identifier
        | identifier := term
        | Nil
        | nil
        | Int
        | [0-9]+
        | Bool
        | true
        | false
        | \ identifier (: term)? => term
        | if term then term else term
        |
  */

  /*
    we
    require that the first term of
    any expression be a primary expression,
    this includes, variables,
    literal-values, literal-types,
    unary operations, parenthesized expressions,
    and language constructs (if, while, etc...)
    these are components of computation, and
    we combine them by way of function application,
    with binary operators directing values into
    functions in such a way as to allow more
    human readable descriptions of computation
    (obviously we can observe that other languages
     have extensible binary operations and Polymorphism
     and overloading, in fact we are working with one right
     now)
  */
  Location&& lhsloc = curloc();
  if (is_primary(curtok()))
  {
    lhs = parse_primary();

    /*

      should the next token be an operator, then
      we need to distinguish between which kind of
      operator it is, and where we could see it.
      with unary operators, we want them to bind
      tightly to the immediate next term, but
      we do not want them to parse a full term
      afterwards.

      this is to avoid the reverse case of parsing
      [3 - 4] as [3 (-4)]
      and yeah, you are reading that correctly,
      it's a call expression...
      and hence, does not allow the programmer
      to even input an expression representing
      [3 - 4]

      this means we cannot parse
      a -b c -d
      as
      a (-b) c (-d)
      we must parse it as
      (a - b) (c - d)
      so to apply unary operations within
      a call expression, one must wrap
      the operation within parenthesis.


      additionally we should recommend that
      unary operators avoid symbolically
      intersecting with binary operators.
      if they do, be aware the grammar rules will select
      the binary form of the operator every time.

      a - b
      is never
      a (-b)
      it is always
      a - b
      this is also valid
      a - -b



    */
    if (lhs)
    {
      /*
        there may be more expressions past the first
        primary term.
      */
      if (curtok() == Token::Operator && is_binop(curtxt()))
      {
        lhs = parse_infix(move(lhs), 0);
      }
      /*
        we could also validly see
        the end of the expression.
      */
      else if (is_ender(curtok()))
      {
        ;
      }
      else
      {
        /*
           error: unknown token following valid
                  primary term.
                  we expect to see the end of the
                  expression past the first valid term,
                  should we not see an operator which
                  could bind multiple terms together,
                  or multiple primary terms following
                  which are bound together within a
                  call expression. past those two
                  grouping constructs, there is no
                  way to combine primary terms together.
                  so it follows that a term which
                  contains a single valid primary, and is
                  followed by neither binop, another
                  primary term, a unop term, or an
                  end of term token, is followed by
                  an invalid token.
        */

      }
    }
    else
    {
      /*
       if we didn't see some lhs,
       but we predicted that we would,
       it's an error term.
      */
    }
  }
  else if (is_ender(curtok()))
  {
    lhs = make_unique(EmptyNode(lhsloc));
  }
  else
  {
    // error: unknown token, not primary, or an ending token.
  }
  return move(lhs);
}

unique_ptr<Ast> Parser::parse_primary()
{
  unique_ptr<Ast> lhs, rhs;
  auto lhsloc = curloc();
  lhs = parse_primitive();

  /*
  if we parsed some primary expression,
  then we must expect that we could be
  required to parse more of said exression,
  and so we check, should the next token
  be another primary term, then we must be
  parsing some call node.
  which means we must
  construct a call Ast node containing these
  two primary expressions, however, we need to
  be cognizant of the associativity of the
  call expression itself.
  do we parse [a b c d] as:
   (((a b) c) d)
  or
   (a (b (c d)))
  i.e. do call terms associative left or right?
  the answer is they associate to the left,
  we construct the deepest node as (a b)
  the next node as (a b) c, then the outermost,
  root node as ((a b) c) d.

  we should recommend that
  unary operators avoid symbolically
  intersecting with binary operators.
  if they do, be aware the grammar rules will select
  the binary form of the operator every time.

  this is to avoid the reverse case of parsing
  [3 - 4] as [3 (-4)]
  and yeah, you are reading that correctly,
  it's a call expression...
  and hence, does not allow the programmer
  to even input an expression representing
  [3 - 4]
  */

  while ((curtok() != Token::Operator && is_primary(curtok()))
      || (curtok() == Token::Operator && !is_binop(curtxt())))
  {
    rhs = parse_primitive();
    Location&& rhsloc = curloc();
    Location callloc = Location(lhsloc.first_line,
                                lhsloc.first_column,
                                rhsloc.first_line,
                                rhsloc.first_column);

    lhs = make_unique(CallNode(move(lhs), move(rhs), callloc));
  }

  return move(lhs);
}

unique_ptr<Ast> Parser::parse_primitive()
{
  unique_ptr<Ast> lhs;
  Location&& lhsloc = curloc();
  switch(curtok())
  {

    /* a variable by itself, or a bind expression.
      notice how, unless the bind expression is
      contained within some lexical structure,
      the bind will gather the rest of the terms
      after the ":=" into a single term which is
      evaluated before the variable is bound to
      the value. (parens would allow the programmer
      to end the bind expression, and perhaps even
      write statements like (x := y) (a := b)
      which would attempt to operate upon the
      result of the bind. (which if we defined the result
      to be the variable itself, said term could be well formed,
      however it is currently always the value 'nil'.)
      which means this expression would reduce to
      (nil) (nil) at best, before we try to call
      the value 'nil', a senseless operation.
      defining it as the lhs of the bind, would
      allow compound bindings to be well formed,
      something the designers of c liked.
      something like x := y := z := 100
      it would be up to us what the above means,
      is each the value 100? or is each the
      other, meaning we need to travel through
      the bindings to reach the 100?)

    */
    case Token::Id: {
      string&& id = curtxt();
      nextok(); // eat Id

      if (curtok() == Token::ColonEquals)
      {
        nextok(); // eat ':='

        // parse_term is responsible for consuming
        // the correct amount of tokens
        unique_ptr<Ast> rhs = parse_term();
        Location&& rhsloc = curloc();
        Location bindloc = Location(lhsloc.first_line,
                                    lhsloc.first_column,
                                    rhsloc.first_line,
                                    rhsloc.first_column);
        lhs = make_unique(BindNode(id, move(rhs), bindloc));
      }
      else
      {
        lhs = make_unique(VariableNode(id, lhsloc));
      }
      break;
    }

    /*
    the next grouping of these cases
    all share something in common, each
    of which is a single token term. these
    are special in that they can be typed
    here and now, even without looking at any
    other text in the program. in this sense
    their types are Atomic. (or Primitive if you like)
    these types are what we are talking about when
    we say "constituent parts of data structures
    and algorithms". they are the building blocks
    which we use to construct programs. and each
    of which can be parsed by extending the definition
    of the parser right here (and in the lexer of course).
    any single token term can be parsed by this switch
    statement. and any multiple token term can
    be parsed by dispatching from here, given that
    every term dispatching from here uniquely
    directs us to a function. if we needed to
    disambiguate between subterms here based
    on tokens after the first token, then
    that would require a more sophisticated set
    of parsing subroutines.
    */

    case Token::TypeNil:
    {
      lhs = unique_ptr<Ast>(new EntityNode(AtomicType(PrimitiveType::Nil), lhsloc));
      nextok();
      break;
    }

    case Token::TypeInt:
    {
      lhs = unique_ptr<Ast>(new EntityNode(AtomicType(PrimitiveType::Int), lhsloc));
      nextok();
      break;
    }

    case Token::TypeBool:
    {
      lhs = unique_ptr<Ast>(new EntityNode(AtomicType(PrimitiveType::Bool), lhsloc));
      nextok();
      break;
    }

    case Token::Nil:
    {
      lhs = unique_ptr<Ast>(new EntityNode((void*)nullptr, lhsloc));
      nextok();
      break;
    }

    case Token::Int:
    {
      int value = stoi(curtxt());
      lhs = unique_ptr<Ast>(new EntityNode(value, lhsloc));
      nextok();
      break;
    }

    case Token::True:
    {
      lhs = unique_ptr<Ast>(new EntityNode(true, lhsloc));
      nextok();
      break;
    }

    case Token::False:
    {
      lhs = unique_ptr<Ast>(new EntityNode(false, lhsloc));
      nextok();
      break;
    }

    /*
    back to multiple token literals.

    an operator in primary position
    is always considered to be a unary operation
    by the parser.
    */
    case Token::Operator:
    {
      string&& op = curtxt();
      nextok();
      // unops bind to their immediate next
      // term only.
      auto rhs    = parse_primitive();
      auto rhsloc = curloc();
      Location unoploc(lhsloc.first_line,
                       lhsloc.first_column,
                       rhsloc.first_line,
                       rhsloc.first_column);

      lhs = unique_ptr<Ast>(new UnopNode(op, move(rhs), unoploc));
      break;
    }

    /*
    could be parsing a tuple
    or a parenthesized term.
    */
    case Token::LParen:
    {
      nextok();

      // is this an empty tuple?
      if (curtok() == Token::RParen)
      {
        nextok();
        lhs = unique_ptr<Ast>(new TupleNode());
        break;
      }

      // parse first subterm
      lhs = parse_term();

      if (curtok() == Token::RParen)
      {
        nextok();
        // good parenthesized term
      }
      else if (curtok() == Token::Comma)
      {
        // this is a tuple declaration.
        vector<unique_ptr<Ast>> tuple_members;

        do {
          nextok();

          tuple_members.push_back(move(lhs));

          lhs = parse_term();

        } while (curtok() == Token::Comma);

        if (curtok() == Token::RParen)
        {
          nextok();
          // good tuple term
          lhs = unique_ptr<Ast>(new TupleNode(tuple_members));
        }
        else
        {
          // error: missing closing paren
        }
      }
      else
      {
          // error: missing closing paren;
      }
      break;
    }

    case Token::While:
    {
      lhs = parse_while();
      break;
    }

    case Token::If:
    {
      lhs = parse_if();
      break;
    }

    case Token::Backslash:
    {
      lhs = parse_procedure();
      break;
    }

    /*
      my thinking is that when the programmer
      enters nothing, that is a known case, and
      so we should handle it conceptually.
      (as in, our conception of what this
      programming language is).

      (while <something> do <empty>)
      Empty is sometimes useful.
      mostly for syntactic conveinence, but
      from the point-of-view of the theorist,
      everything past turing completeness is
      syntactic conveinence, so i don't hold
      much stock in that counter-argument.
      the conveinence is more akin to expressing intention
      in different ways to me. sometimes it is
      usefull to say the same thing in different ways,
      as everyone understands things differently.
      so we simply choose to draw the line a little
      further up from the bare minimum, just enough
      that we can still get a grasp on everything
      from a theoretical perspective, but loose
      enough to give some freedom of expression.
    */
    case Token::End: case Token::Then:
    case Token::Else: case Token::RParen:
    case Token::EqRarrow: {
      /*
        we don't want to consume the ending token
        because the enclosing scope deals with it.
      */
      break;
    }

    default:
      throw "unexpected token in primitive position.";
  }
  return lhs;
}

unique_ptr<Ast> Parser::parse_if()
{
  Location&& lhsloc = curloc();
  unique_ptr<Ast> cond, test, first, second;
  if (curtok() == Token::If)
  {
    nextok();

    test = parse_term();

    if (curtok() == Token::Then)
    {
      nextok();

      first = parse_term();

      if (curtok() == Token::Else)
      {
        nextok();

        second = parse_term();
        Location&& rhsloc = curloc(), condloc(lhsloc.first_line,
                                              lhsloc.first_column,
                                              rhsloc.first_line,
                                              rhsloc.first_column);
        cond = unique_ptr<Ast>(new CondNode(move(test), move(first), move(second), condloc));
      }
      else
      {
        // error: missing else
      }
    }
    else
    {
      // error: missing then
    }
  }
  return cond;
}

unique_ptr<Ast> Parser::parse_while()
{
  unique_ptr<Ast> loop, test, body;
  Location&& lhsloc = curloc();

  if (curtok() == Token::While)
  {
    nextok();
    test = parse_term();

    if (curtok() == Token::Do) {
      nextok();
      body = parse_term();

      Location&& rhsloc = curloc(), looploc(lhsloc.first_line,
                                            lhsloc.first_column,
                                            rhsloc.last_line,
                                            rhsloc.last_column);

      loop = unique_ptr<Ast>(new WhileNode(move(test), move(body), looploc));
    }
  }
  return loop;
}

unique_ptr<Ast> Parser::parse_procedure()
{
  bool poly = false;
  string id;
  vector<pair<string, unique_ptr<Ast>>> args;
  unique_ptr<Ast> proc, type, body;
  Location&& lhsloc = curloc();

  auto parse_arg = [&poly, &id, &type](){
    if (curtok() != Token::Id)
    {
      id = curtxt();
      nextok();

      if (curtok() == Token::Colon)
      {
        nextok();
        type = parse_term();
        poly = false;
      }
      else
      {
        // if the type annotation is missing, we assume polymorphic type.
        type = unique_ptr<Ast>(new EntityNode(PrimitiveType::Poly, Location()));
        poly = true;
      }

      return make_pair(id, move(type));
    }
    throw "unexpected bad arg after speculation.";
  }

  if (curtok() != Token::Backslash)
    throw "unexpected missing backslash after speculation.";

  nextok();

  if (curtok() == Token::Id)
  {
    // parse a single argument
    args.push_back(parse_arg());
  }
  else if (curtok() == Token::LParen)
  {
    // parse an argument list.
    nextok();

    args.push_back(parse_arg());

    if (curtok() == Token::Comma)
    {
      do {
        nextok();
        args.push_back(parse_arg());
      } while (curtok() == Token::Comma);
    }
  }
  else
  {
    throw "unexpected missing argument after speculation.";
  }

  if (curtok() != Token::EqRarrow)
    throw "unexpected missing \"=>\" after speculation.";

  nextok();

  // while we parse the body of this procedure,
  // the procedures scope is the new enclosing
  // scope/top-of-the-scope-stack.
  // this line does double duty,
  // 1) it constructs the new scope with a reference to
  //      the old enclosing scope as it's enclosing scope.
  // 2) it pushes this new scope onto the scope stack in
  //      order to make it the new enclosing scope.
  scopes.push(new SymbolTable(scopes.top()));

  body = parse_term();

  Location&& rhsloc = curloc();
  Location procloc(lhsloc.first_line,
                   lhsloc.first_column,
                   rhsloc.first_line,
                   rhsloc.first_column);

  proc = make_unique<Ast>(EntityNode(ProcedureLiteral(args, move(body)), procloc, *(scopes.top())));
  scopes.pop();

  return proc;
}


unique_ptr<Ast> Parser::parse_infix(unique_ptr<Ast> lhs, int precedence)
{
  /*
  this is an implementation of an operator precedence parser.
  the pseudocode for which:

  parse_expression_1(lhs, min_precedence)
    lookahead := peek next token
    while lookahead is a binary operator whose precedence is >= min_precedence
          op := lookahead
          advance to next token
          rhs := parse_primary ()
          lookahead := peek next token
          while lookahead is a binary operator whose precedence is greater
                   than op's, or a right-associative operator
                   whose precedence is equal to op's
              rhs := parse_expression_1 (rhs, lookahead's precedence)
              lookahead := peek next token
          lhs := the result of applying op with operands lhs and rhs
      return lhs

  this algorithm parses as many tokens as it
  can until it reaches the highest node in
  the tree
  (this is purpose of the gaurded recursion,
   iterating the recursion collapses more than
   one 'peak' as it were, in the expression
   before we can validly construct the node.
   "sure this was a local high, but what if
    there is another higher peak afterwards?")
  (highest meaning starting from the left, the highest
  precedence operator, or any right associative operator
  of equal precedence)) and it constructs the first
  node with that operator and the latest lhs and rhs
  and then parse until we reach the
  next deepest node in the expression, once the
  end of the expression is reached, or there is
  a very low precedence operator far to the right
  of the beginning of the expression, then the
  recursive function collapses as many rhs
  nodes until we reach the end, or we parse the
  low precedence operator into it's correct tree
  and then start another pass into the next
  deepest expression, iterating this collapses
  many peaks into the rhs, and many identical
  precedence operations into the lhs.
  */
  optional<pair<int, Assoc>> lopPrec;

  while ((lopPrec = binops.find(curtxt())) && get<int>(*lopPrec) >= precedence)
  {
    string&& optxt = curtxt();
    auto       op = lopPrec;

    nextok();

    Location&& lhsloc = curloc();
    auto rhs = parse_primary();
    Location&& rhsloc = curloc();

    Location rhstermloc(lhsloc.first_line,
                        lhsloc.first_column,
                        rhsloc.first_line,
                        rhsloc.first_column);

    /*
       op
    lhs  rhs

        op
    lhs     op
         rhs   rhs'

    */
    while ((lopPrec = binops.find(curtxt()))
           &&   (get<int>(*lopPrec) > get<int>(*op)
             || (get<int>(*lopPrec) == get<int>(*op) && get<Assoc>(*lopPrec) == Assoc::Right)))
    {
      rhs = move(parse_infix(move(rhs), get<int>(*lopPrec)));
    }

    /*
       op
    lhs  rhs

          op
       op    rhs'
    lhs  rhs
    */
    Location binoploc(lhs->loc.first_line,
                      lhs->loc.first_column,
                      rhstermloc.first_line,
                      rhstermloc.first_column);
    lhs = unique_ptr<Ast>(new BinopNode(optxt, move(lhs), move(rhs), binoploc));
  }
  return lhs;
}


optional<ParserError> Parser::speculate_term()
{
  optional<ParserError> result;

  if (is_primary(curtok()))
  {
    result = speculate_primary();

    if (result)
    {
      while (curtok() == Token::Operator && is_binop(curtxt()))
      {
        /*
        an infix/affix expression,
        modulo precedence, is:
          primary (operator primary)+
        */
          nextok(); // eat the binop
          result = speculate_primary();
      }
    }
  }
  else if (is_ender(curtok()))
  {
    // an empty expression is represented by the empty term
    result = optional<ParserError>();
  }
  else
  {
     result = optional<ParserError>({curloc(), "unknown token in primary position"});
  }
  return result;
}

optional<ParserError> Parser::speculate_primary()
{
  optional<ParserError> result;
  while ((curtok() != Token::Operator && is_primary(curtok()))
      || (curtok() == Token::Operator && !is_binop(curtxt())))
  {
    result = speculate_primitive();
  }
  return result;
}

optional<ParserError> Parser::speculate_primitive()
{
  optional<ParserError> result;
  if      (speculate(Token::Nil));
  else if (speculate(Token::TypeNil));
  else if (speculate(Token::Int));
  else if (speculate(Token::TypeInt));
  else if (speculate(Token::True));
  else if (speculate(Token::False));
  else if (speculate(Token::TypeBool));
  else if (speculate(Token::Id)) {
    if (speculate(Token::ColonEquals)) {
      result = speculate_term();
    }
  }
  else if (speculate(Token::LParen)) {
    /*
      remember that we are using the None
      shape of the optional type to signal
      success, and the implicit boolean
      conversion of the None shape is false.
    */
    result = speculate_term();
    if (!result)
    {
      // we saw a valid term
      if (speculate(Token::RParen));
      else if (speculate(Token::Comma))
      {
        // this is a tuple, which could contain
        // any number of terms.
        do {
          result = speculate_term();
        } while (!result && speculate(Token::Comma));

        if (speculate(Token::RParen))
          ;
        else {
          result  = optional<ParserError>({curloc(), "missing closing ) after valid tuple"});
        }
      }
      else
      {
        // we saw one valid term, but then it wasn't
        // followed by an RParen or a Comma.
        result = optional<ParserError>({curloc(), "missing closing ) or , after valid term"});
      }
    }
    else
    {
      // we didn't see a valid term, the reason is in result;
    }
  }
  else if (speculate(Token::Operator)) {
    result = speculate_primary();
  }
  else if (speculate(Token::While)) {
    result = speculate_while();
  }
  else if (curtok() == Token::If) {
    result = speculate_if();
  }
  else if (curtok() == Token::Backslash) {
    result = speculate_procedure();
  }
  else if (is_ender(curtok()));
  else {
    // error: no valid primitive term to parse.
    result = optional<ParserError>({curloc(), "unknown primitive"});
  }

  return result;
}

optional<ParserError> Parser::speculate_while()
{
  optional<ParserError> result;
  if (speculate(Token::While)) {
    result = speculate_term();
    if (!result && speculate(Token::Do)) {
      result = speculate_term();
    }
    else
    {
      result = optional<ParserError>({curloc(), "missing 'do'"});
    }
  }
  else
  {
    result = optional<ParserError>({curloc(), "missing 'while'"});
  }
  return result;
}

optional<ParserError> Parser::speculate_if()
{
  optional<ParserError> result;
  if (speculate(Token::If)) {
      result = speculate_term();
      if (!result && speculate(Token::Then)) {
          result = speculate_term();
          if (!result && speculate(Token::Else)) {
            result = speculate_term();
          }
          else
          {
            result = optional<ParserError>({curloc(), "missing 'else'"});
          }
      }
      else
      {
        result = optional<ParserError>({curloc(), "missing 'then'"});
      }
  }
  else
  {
    result = optional<ParserError>({curloc(), "missing 'if'"});
  }
  return result;
}

optional<ParserError> Parser::speculate_procedure()
{
  optional<ParserError> result;

  auto speculate_arg = [&result]()
  {
    if (speculate(Token::Id)) {
      // type annotations are optional.
      if (speculate(Token::Colon)) {
        result = speculate_term();
      }
    }
    else
    {
      result = optional<ParserError>({curloc(), "expected the id of the argument"});
    }
  }

  // start parsing the procedure literal.
  if (speculate(Token::Backslash)) {
    // parse the argument, or argument list
    if (curtok() == Token::Id) {
        speculate_arg();
    }
    else if (speculate (Token::LParen))
    {
      do {
        speculate_arg();
      } while (speculate(Token::Comma));

      if (speculate(Token::RParen))
        ;
      else
        result = optional<ParserError>({curloc(), "missing closing ) after argument list"});
    }
    else
    {
      result = optional<ParserError>({curloc(), "missing argument or argument list following \\"});
    }

    // parse the body
    if (speculate(Token::EqRarrow))
    {
      result = speculate_term();
    }
    else
    {
      result = optional<ParserError>({curloc(), "missing \"=>\" after argument(s)"});
    }

  }

  return result;
}





















/* ------------------------------------------------- */
