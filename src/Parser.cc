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

#include "Parser.hh"
#include "Ast.hh"
#include "Lexer.hh"
#include "OperatorTable.hh"
#include "Kernel.hh"

Parser::Parser()
{
  init_binops(binops);
  init_unops(unops);
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

void Parser::fillTokens(int i)
{
  if (curidx + i > tokbuf.size()) // do we need more tokens than we have?
  {
    int n = (curidx + i) - tokbuf.size(); // how many tokens do we need?

    for (int j = 0; j < n; j++)
    {
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

  fillTokens(1);
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

bool Parser::is_typeop(const string& op)
{
  auto typeop = typeops.find(op);
  if (typeop)
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
  if (is_type_primitive(t))
    return true;

  switch(t)
  {
    case Token::Nil: case Token::Int:
    case Token::True: case Token::False:
    case Token::Id: case Token::If:
    case Token::Backslash: case Token::Operator:
    case Token::LParen:
      return true;
    default:
      return false;
  }
}

bool Parser::is_type_primitive(Token t)
{
  switch(t)
  {
    case Token::TypeNil: case Token::TypeInt:
    case Token::TypeBool:
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

  fillTokens(1);

  mark();
  bool good = speculate_term();
  release();

  if (good)
    result = optional<unique_ptr<Ast>>(move(parse_term()));
  else
    result = optional<unique_ptr<Ast>>();

  return result;
}

unique_ptr<Ast> Parser::parse_term()
{
  unique_ptr<Ast> lhs, rhs;

  /*
    starting from anywhere we could
    expect to see some term, we
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
    (we hope).
  */
  Location&& lhsloc = curloc();
  if (is_primary(curtok()))
  {
    lhs = parse_primary();
  }

  /*
    if we parsed some primary expression,
    then we must expect that we could be
    required to parse more of some exression,
    and so we check, should the next token
    be another primary term, then we must be
    parsing some call node. which means we must
    construct a call Ast node containing these
    two primary expressions, however, we need to
    be cognizant of the associativity of the
    call expression itself.
    do we parse [a b c d] as:
      (((a b) c) d)
    or
      (a (b (c d)))
    i.e. do call terms associative left or right ?.
    the answer is they associate to the left,
    we construct the deepest node as (a b)
    the next node as (a b) c, then the outermost,
    root node as ((a b) c) d.
  */
  if (lhs) {
    if ((curtok() == Token::Operator && is_unop(curtxt()))
     || (curtok() != Token::Operator && is_primary(curtok())))
    {
      do
      {
        rhs = parse_primary();
        Location&& rhsloc = curloc();
        Location callloc = Location(lhsloc.first_line,
                                    lhsloc.first_column,
                                    rhsloc.first_line,
                                    rhsloc.first_column);

        lhs = unique_ptr<Ast>(new Call(move(lhs), move(rhs), callloc));
      } while ((curtok() == Token::Operator && is_unop(curtxt()))
            || (curtok() != Token::Operator && is_primary(curtok())));
    }
    /*
    if the term immediately after the primary
    term is an operator, then the assumption is
    that is a binary operation. and so we dispatch
    to the operator precedence parser.
    */
    else if (curtok() == Token::Operator) {
      lhs = parse_infix(move(lhs), 0);
    }
  }

  return move(lhs);
}

unique_ptr<Ast> Parser::parse_primary()
{
  unique_ptr<Ast> lhs;
  Location&& lhsloc = curloc();
  switch(curtok())
  {
    case TypeNil:
    case TypeInt:
    case TypeBool:
      lhs = parse_type_primitive();
      break;
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
      something like x := y := z := 100)

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
        lhs = unique_ptr<Ast>(new Bind(id, move(rhs), bindloc));
      }
      else
      {
        lhs = unique_ptr<Ast>(new Variable(id, lhsloc));
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

    case Token::Nil:
    {
      Type* niltype = ctx->getVoidTy();
      lhs = unique_ptr<Ast>(new Entity(niltype, '\0', lhsloc));
      nextok();
      break;
    }

    case Token::Int:
    {
      int value     = stoi(curtxt());
      Type* inttype = ctx->getInt32Ty();
      lhs = unique_ptr<Ast>(new Entity(inttype, value, lhsloc));
      nextok();
      break;
    }

    case Token::True:
    {
      Type* booltype = ctx->getInt1Ty();
      lhs = unique_ptr<Ast>(new Entity(booltype, true, lhsloc));
      nextok();
      break;
    }

    case Token::False:
    {
      Type* booltype = ctx->getInt1Ty();
      lhs = unique_ptr<Ast>(new Entity(booltype, false, lhsloc));
      nextok();
      break;
    }

    /*
    an operator in primary position
    is always considered to be a unary operation
    by the parser.
    */
    case Token::Operator:
    {
      string&& op = curtxt();
      nextok();

      auto rhs    = parse_term();
      auto rhsloc = curloc();
      Location unoploc(lhsloc.first_line,
                       lhsloc.first_column,
                       rhsloc.first_line,
                       rhsloc.first_column);

      lhs = unique_ptr<Ast>(new Unop(op, move(rhs), unoploc));
      break;
    }

    case Token::LParen:
    {
      nextok();

      auto lhs = parse_term();

      if (curtok() == Token::RParen)
      {
        nextok();
        // good term
      }
      else
      {
        // error: missing closing right parenthesis.
      }
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
      lhs = unique_ptr<Ast>(new Empty(lhsloc));
      break;
    }

    default:
      throw "unexpected token in primary position.";
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
        cond = unique_ptr<Ast>(new Cond(move(test), move(first), move(second), condloc));
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

unique_ptr<Ast> Parser::parse_procedure()
{
  bool poly = false;
  unique_ptr<Ast> proc, type, body;
  Location&& lhsloc = curloc();
  if (curtok() == Token::Backslash)
  {
    nextok();

    if (curtok() == Token::Id)
    {
      string&& id = curtxt();
      nextok();

      // parse the optional type annotation
      if (curtok() == Token::Colon)
      {
        nextok();
        type = parse_term();
      }
      else
      {
        type = unique_ptr<Ast>(new Entity(EntityTypeTag::Poly, Location()));
        poly = true;
      }

      if (curtok() == Token::EqRarrow)
      {
        nextok();
        body = parse_term();
        Location&& rhsloc = curloc();
        Location procloc(lhsloc.first_line,
                         lhsloc.first_column,
                         rhsloc.first_line,
                         rhsloc.first_column);

        proc = unique_ptr<Ast>(new Entity(Procedure(id, move(type), move(body)), poly, procloc));
      }
      else
      {
        // error: expecting '=>' to predict function body
      }

    }
    else
    {
      // error: expecting Id immediately after '\\'
    }
  }
  return proc;
}

unique_ptr<Ast> Parser::parse_type_primitive()
{
  unique_ptr<Ast> lhs;
  Location loc = curloc();
  switch(curtok())
  {
    case Token::TypeNil:
    {
      Type* niltype = ctx->getVoidTy();
      lhs = unique_ptr<Ast>(new Entity(niltype, lhsloc));
      nextok();
      break;
    }

    case Token::TypeInt:
    {
      Type* inttype = ctx->getInt32Ty();
      lhs = unique_ptr<Ast>(new Entity(inttype, lhsloc));
      nextok();
      break;
    }

    case Token::TypeBool:
    {
      Type* booltype = ctx->getInt1Ty();
      lhs = unique_ptr<Ast>(new Entity(booltype, lhsloc));
      nextok();
      break;
    }

    default:
      throw "unknown type primitive\n";

  }
  return lhs;
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
    lhs = unique_ptr<Ast>(new Binop(optxt, move(lhs), move(rhs), binoploc));
  }
  return lhs;
}


bool Parser::speculate_term()
{
  bool result = true;

  if (is_primary(curtok()))
  {
    result = speculate_primary();
  }
  else
  {
    // error: expression must begin
    //  with a primary term.
  }

  if (result)
  {

    if (curtok() != Token::Operator && is_primary(curtok()))
    {
      do
      {
        result = speculate_primary();
        if (!result)
        {
          // error: unable to parse primary term.
          break;
        }
      } while (curtok() != Token::Operator && is_primary(curtok()));
    }
    else if (curtok() == Token::Operator && is_unop(curtxt()))
    {
      nextok();
      result = speculate_term();
    }
    else if (curtok() == Token::Operator && is_typeop(curtxt()))
    {
      do {
        nextok();
        result = speculate_type_primitive();
        if (!result) {
          // error: type operators only valid on type primitives
          break;
        }
      } while (curtok() == Token::Operator && is_typeop(curtxt()));
    }
    else if (curtok() == Token::Operator && is_binop(curtxt()))
    {
      /*
      an infix/affix expression,
      modulo precedence, is:
        term (operator term)+
      */
      do {
        nextok(); // eat the binop
        result = speculate_primary();
        if (!result)
        {
          // error: unable to parse primary term.
          break;
        }
      } while (curtok() == Token::Operator && is_binop(curtxt()));
    }
  }

  return result;
}

bool Parser::speculate_primary()
{
  bool result = true;
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
    if (speculate_term()) {
      result = speculate(Token::RParen);
    }
  }
  else if (speculate(Token::Operator)) {
    result = speculate_primary();
  }
  else if (curtok() == Token::If) {
    result = speculate_if();
  }
  else if (curtok() == Token::Backslash) {
    result = speculate_procedure();
  }
  else if (is_ender(curtok()));
  else {
    // error: no valid primary term to parse.
    result = false;
  }

  return true;
}

bool Parser::speculate_if()
{
  bool result = false;
  if (speculate(Token::If)) {
    if (speculate_term()) {
      if (speculate(Token::Then)) {
        if (speculate_term()) {
          if (speculate(Token::Else)) {
            if (speculate_term()) {
              result = true;
            }
          }
        }
      }
    }
  }
  return result;
}

bool Parser::speculate_procedure()
{
  bool result = false;
  if (speculate(Token::Backslash)) {
    if (speculate(Token::Id)) {
      if (speculate(Token::Colon)) {
        if (speculate_term())
        ;
      }

      if (speculate(Token::EqRarrow)) {
        if (speculate_term()) {
          result = true;
        }
      }
    }
  }
  return result;
}





















/* ------------------------------------------------- */
