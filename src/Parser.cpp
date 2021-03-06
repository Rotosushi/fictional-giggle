
/*
  the grammar so far

  term := affix
        | affix ';' term

  affix := primary
         | primary operator primary
         | primary "<-" affix

  primary := primitive
           | primitive primitive

  primitive := type
             | atom
             | unop atom

  atom := identifier
        | identifier := term
        | nil
        | [0-9]+
        | true
        | false
        | \ identifier (: term)? => term
        | 'if' term 'then' term 'else' term
        | 'while' term 'do' term
        | '(' term ')'

  type := "Nil"
        | "Int"
        | "Bool"
        | "Poly"
        | "Ref" type
        | type -> type

  identifier := [a-zA-Z_][a-zA-Z0-9_]*
  integer    := [0-9]+

  binop := operator
  unop  := operator

  operator := [*+-/%<>:=&@!~|$^]+


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
#include <list>
using std::list;
#include <set>
using std::set;
#include <memory>
using std::shared_ptr;
using std::make_shared;
using std::move;
#include <utility>
using std::optional;
using std::pair;
using std::get;

#include "Ast.hpp"
#include "Empty.hpp"
#include "Variable.hpp"
#include "Application.hpp"
#include "Binop.hpp"
#include "Unop.hpp"
#include "Entity.hpp"
#include "Bind.hpp"
#include "Iteration.hpp"
#include "Conditional.hpp"
#include "Sequence.hpp"
#include "Assignment.hpp"
#include "Reference.hpp"
#include "PinkException.hpp"
#include "ParserError.hpp"
#include "ParserJudgement.hpp"
#include "BinopPrecedenceTable.hpp"
#include "BinopEliminators.hpp"
#include "UnopEliminators.hpp"
#include "SymbolTable.hpp"
#include "Parser.hpp"


Parser::Parser(shared_ptr<SymbolTable> tpscp,
       shared_ptr<BinopPrecedenceTable> prsdncs,
       shared_ptr<BinopSet> bnps,
       shared_ptr<UnopSet>  unps)
  : precedences(prsdncs), binops(bnps), unops(unps)
{
  /*
    we need to utilize a stack in order to
    express the nesting of scopes naturally.
  */
  scopes.push(tpscp);
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

    for (int j = 0; j < n; j++) // fill in that many more tokens
    {
      // without the type string here,
      // gdb cannot print strings...
      // what a strange requirement,
      // constidering the string header
      // is included in the source code
      // of this executable, or at the
      // very least dynamically linked in,
      // in which case a link to the header
      // still exists in the binary!
      // does C treat each source file
      // completely independant of eachother
      // until exactly linkage/dynamic-linkage time?
      auto   tok = lexer.yylex();
      string str = lexer.yytxt();
      auto   loc = lexer.yyloc();
      tokbuf.push_back(tok);
      txtbuf.push_back(str);
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
  optional<shared_ptr<UnopEliminatorSet>> unop = unops->FindUnop(op);
  if (unop)
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
  optional<shared_ptr<BinopEliminatorSet>> binop = binops->FindBinop(op);
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
    case Token::Semicolon:
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

ParserJudgement Parser::parse(const string& text)
{
  reset();

  lexer.set_buffer(text);

  gettok(1);

  if (curtok() == Token::End)
  {
    return ParserJudgement(shared_ptr<Ast>(new Entity(unique_ptr<Object>(new Empty()), curloc())));
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
  optional<ParserError> err_or = speculate_expression();
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
    return ParserJudgement(*err_or);
  }
  else
  {
    return ParserJudgement(parse_expression());
  }
}

/*

expression := term
            | term ';' expression

term := primary
      | primary operator primary
      | primary '<-' term

primary := primitive
         | primitive primitive

primitive := type
           | atom
           | unop atom

atom := identifier
      | identifier := term
      | nil
      | [0-9]+
      | true
      | false
      | \ identifier (: term)? => term
      | 'if' term 'then' term 'else' term
      | 'while' term 'do' term
      | '(' term ')'

type := Nil
      | Int
      | Bool
      | Poly
      | type -> type
*/
shared_ptr<Ast> Parser::parse_expression()
{
  shared_ptr<Ast> lhs, rhs;
  Location lhsloc = curloc();
  lhs = parse_term();

  if (lhs)
  {
    if (curtok() == Token::Semicolon)
    {
      // we saw a ';' after the first term,
      // this means we want to parse an entire term
      // and then place the two terms into a sequence
      // Node.
      nextok(); // eat ';'
      rhs = parse_expression();
      Location rhsloc = curloc();
      Location seqloc = {lhsloc.first_line, lhsloc.first_column, rhsloc.last_line, rhsloc.last_column};
      lhs = shared_ptr<Ast>(new Sequence(lhs, rhs, seqloc));
    }
  }

  return lhs;
}

shared_ptr<Ast> Parser::parse_term()
{
  shared_ptr<Ast> lhs, rhs;
  /*
    we require that the first term of
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
      afterwards (as this leads to unintuitive
      evaluation trees.)

      from the beginning we wanted expressions like
      a b c - f g h
      to parse as
      (a b c) - (f g h)

      now i know this is necessary
      to avoid the reverse case of parsing
      [3 - 4] as [3 (-4)]
      and yeah, you are reading that correctly,
      it's a call expression...
      and hence, does not allow the programmer
      to even input an expression representing
      [3 - 4]
      all this means we cannot parse
      a -b c -d
      as
      a (-b) c (-d)
      we must parse it as
      (a - b) (c - d)
      so to apply unary operations within
      a call expression, one must wrap
      the operation within parenthesis.


      as a direct result of the above
      discussion we should recommend that
      unary operators avoid symbolically
      intersecting with binary operators.
      if they do, be aware the grammar rules place
      prefrence on the binary form of the operator.
      this means that if the operator is both a unop
      and a binop, the expression
      a op b will always be the operation taking
      arguments a and b,
      and never the application of a taking as argument
      the unop applied to b.

      TLDR:
      [a - b]
      is never
      [a (-b)]
      it is always
      [a - b]
      this is also valid
      [-a - -b]
      as is
      [-a - b]
      this means that when the programmer
      wants to apply an operation directly
      to the argument to a procedure, they
      -must- wrap that operation in parens
      in order for the entire expression
      to parse as a call expression, otherwise
      we switch to parsing a binary operation,
      however, this is only if the operator
       itself is a binop and a unop. should the
       operator only be defined as a unop then
       the grammar will parse it as an application.
    */
    if (lhs != nullptr)
    {
      /*
        there may be more expression past the first
        primary term.
      */

      if (curtok() == Token::Operator && is_binop(curtxt()))
      {
        lhs = parse_infix(lhs, 0);
      }
      else if (curtok() == Token::Larrow)
      {
        /*
         term '<-' term
        */
        nextok(); // eat '<-'
        rhs = parse_term();
        Location&& rhsloc = curloc();
        Location assgnloc(lhsloc.first_line, lhsloc.first_column, rhsloc.last_line, rhsloc.last_column);
        lhs = shared_ptr<Ast>(new Assignment(lhs, rhs, assgnloc));
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
    lhs = shared_ptr<Ast>(new Entity(unique_ptr<Object>(new Empty()), curloc()));
  }
  else
  {
    // error: unknown token, not primary, or an ending token.
  }
  return lhs;
}

shared_ptr<Ast> Parser::parse_primary()
{
  shared_ptr<Ast> lhs, rhs;
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

    lhs = shared_ptr<Ast>(new Application(lhs, rhs, callloc));
  }
  return lhs;
}

shared_ptr<Ast> Parser::parse_primitive()
{
  shared_ptr<Ast> lhs;
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
    case Token::Id:
    {
      string id = curtxt();
      nextok(); // eat Id

      if (curtok() == Token::ColonEquals)
      {
        nextok(); // eat ':='

        // parse_term is responsible for consuming
        // the correct amount of tokens
        shared_ptr<Ast> rhs = parse_term();
        Location&& rhsloc = curloc();
        Location bindloc = Location(lhsloc.first_line,
                                    lhsloc.first_column,
                                    rhsloc.first_line,
                                    rhsloc.first_column);
        lhs = shared_ptr<Ast>(new Bind(id, rhs, bindloc));
      }
      else
      {
        // the variable we parsed has already
        // been consumed at this point.
        lhs = shared_ptr<Ast>(new Variable(id, lhsloc));
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
    we say "constituent parts of data structures".
    they are the building blocks
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
    of parsing subroutines. (the dispatch from
    here would need to occur on the shared predictor,
    and the function called would be in charge of
    disambiguation.)
    */

    case Token::Nil:
    {
      lhs = shared_ptr<Ast>(new Entity((void*)nullptr, lhsloc));
      nextok(); // eat "nil"
      break;
    }

    case Token::Int:
    {
      int value = stoi(curtxt());
      lhs = shared_ptr<Ast>(new Entity(value, lhsloc));
      nextok(); // eat int
      break;
    }

    case Token::True:
    {
      lhs = shared_ptr<Ast>(new Entity(true, lhsloc));
      nextok(); // eat 'true'
      break;
    }

    case Token::False:
    {
      lhs = shared_ptr<Ast>(new Entity(false, lhsloc));
      nextok(); // eat 'false'
      break;
    }

    /*
    back to multiple token literals.

    an operator in primary position
    is always considered to be a unary operation
    by the parser. (what is primary position?
    well, primary tokens are! so, here, is the
    primary position.)
    */
    case Token::Operator:
    {
      string&& op = curtxt();
      nextok();
      // unops bind to their immediate next
      // term only. (just like the token ':='
      // for bind terms)
      auto rhs    = parse_primitive();
      auto rhsloc = curloc();
      Location unoploc(lhsloc.first_line,
                       lhsloc.first_column,
                       rhsloc.first_line,
                       rhsloc.first_column);

      lhs = shared_ptr<Ast>(new Unop(op, move(rhs), unoploc));
      break;
    }

    /*
      a type is explicitly considered
      within the grammar now, so we no
      longer consider '->' to be subsumed
      by operator parsing.
      it is now a fully fledged grapheme.
      additionally, we note that all
      type expressions are started by a
      type literal, and composed from there.

    */
    case Token::TypeNil:
    case Token::TypeInt:
    case Token::TypeBool:
    {
      lhs = shared_ptr<Ast>(new Entity(parse_type(), curloc()));
      break;
    }

    /*
    having types be first class is probably
    not a static language feature.
    */

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
        // the empty tuple '()' is equivalent to the value 'nil'
        lhs = shared_ptr<Ast>(new Entity((void*)nullptr, lhsloc));
        break;
      }

      // parse first subterm
      lhs = parse_term();

      if (curtok() == Token::RParen)
      {
        nextok();
        // good parenthesized term
      }
      /*else if (curtok() == Token::Comma)
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
      }*/
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


      Empty is sometimes useful.
      (while <something> do <empty>)
      mostly for syntactic conveinence, but
      from the point-of-view of some hardcore theorists
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
      throw PinkException("unexpected token in primitive position.", __FILE__, __LINE__);
  }
  return lhs;
}

shared_ptr<Type> Parser::parse_type()
{
  Location&& lhsloc = curloc();
  shared_ptr<Type> type, rhs;

  auto parse_type_atom = [this, lhsloc]()
  {
    shared_ptr<Type> type, rhs;
    if (curtok() == Token::TypeRef)
    {
      nextok(); // eat "Ref"
      rhs = parse_type();
      Location&& rhsloc = curloc();
      Location refloc = {lhsloc.first_line, lhsloc.first_column, rhsloc.first_line, rhsloc.first_column};
      type = shared_ptr<Type>(new RefType(rhs, refloc));
    }
    else if (curtok() == Token::TypeNil)
    {
      type = shared_ptr<Type>(new MonoType(AtomicType::Nil, curloc()));
      nextok();
    }
    else if (curtok() == Token::TypeInt)
    {
      type = shared_ptr<Type>(new MonoType(AtomicType::Int, curloc()));
      nextok();
    }
    else if (curtok() == Token::TypeBool)
    {
      type = shared_ptr<Type>(new MonoType(AtomicType::Bool, curloc()));
      nextok();
    }
    else if (curtok() == Token::TypePoly)
    {
      type = shared_ptr<Type>(new MonoType(AtomicType::Poly, curloc()));
      nextok();
    }
    else if (curtok() == Token::LParen)
    {
      nextok();
      type = parse_type();

      if (curtok() != Token::RParen)
        throw PinkException("Unexpected missing closing parenthesis.", __FILE__, __LINE__);
    }
    else
    {
      /*
        bad type atom
      */
      throw PinkException("Unexpected Bad Type Atom", __FILE__, __LINE__);
    }

    return type;
  };



  type = parse_type_atom();

  if (type && curtok() == Token::Rarrow)
  {
    nextok(); // eat '->'
    /*
      type := "Nil"
            | "Int"
            | "Bool"
            | "Ref" type
            | type '->' type
            //| '(' type (',' type)+ ')'

      the recursive call enacts the
      right associative nature of the
      '->' type operator.
    */

    shared_ptr<Type> rhstype = parse_type();
    Location&& rhsloc = curloc();
    Location typeloc(lhsloc.first_line, lhsloc.first_column, rhsloc.last_line, rhsloc.last_column);
    type = shared_ptr<Type>(new ProcType(type, rhstype, typeloc));
  }

  return type;
}

shared_ptr<Ast> Parser::parse_if()
{
  Location&& lhsloc = curloc();
  shared_ptr<Ast> cond, test, first, second;
  if (curtok() == Token::If)
  {
    nextok();

    test = parse_expression();

    if (curtok() == Token::Then)
    {
      nextok();

      first = parse_expression();

      if (curtok() == Token::Else)
      {
        nextok();

        second = parse_expression();
        Location&& rhsloc = curloc(), condloc(lhsloc.first_line,
                                              lhsloc.first_column,
                                              rhsloc.first_line,
                                              rhsloc.first_column);
        cond = shared_ptr<Ast>(new Conditional(test, first, second, condloc));
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

shared_ptr<Ast> Parser::parse_while()
{
  shared_ptr<Ast> loop, test, body;
  Location&& lhsloc = curloc();

  if (curtok() == Token::While)
  {
    nextok();
    test = parse_expression();

    if (curtok() == Token::Do) {
      nextok();
      body = parse_expression();

      Location&& rhsloc = curloc(), looploc(lhsloc.first_line,
                                            lhsloc.first_column,
                                            rhsloc.last_line,
                                            rhsloc.last_column);

      loop = shared_ptr<Ast>(new Iteration(test, body, looploc));
    }
  }
  return loop;
}

/*
saving this for later when we try and build
multiple argument procedure definitions out
of single argument procedure structures only.
(we still want to parse lists of arguments,
for the conveinence of the programmer.)

  procedure := '\' id (: type)? (',' id (: type)?)* '=>' term
shared_ptr<Ast> Parser::parse_procedure()
{
bool poly = false;
shared_ptr<Type> poly_type = shared_ptr<Type>(new MonoType(AtomicType::Poly, Location()));
vector<pair<string, shared_ptr<Type>>> args;
shared_ptr<Ast> proc, body;
Location&& lhsloc = curloc();

auto parse_arg = [this, &poly, &poly_type]() -> pair<string, shared_ptr<Type>>
{
  if (curtok() != Token::Id)
    throw PinkException("unexpected missing argument after speculation.", __FILE__, __LINE__);

  shared_ptr<Type> arg_type;
  string arg_name = curtxt();
  nextok();

  if (curtok() == Token::Colon)
  {
    nextok();
    arg_type = parse_type();
    poly = arg_type->is_polymorphic();
  }
  else
  {
    arg_type = poly_type;
    poly = true;
  }

  return pair<string, shared_ptr<Type>>(arg_name, arg_type);
};

if (curtok() != Token::Backslash)
  throw PinkException("unexpected missing backslash after speculation.", __FILE__, __LINE__);

nextok();

if (curtok() == Token::Id)
{
  args.push_back(parse_arg());
  while (curtok() == Token::Comma)
  {
    nextok();
    args.push_back(parse_arg());
  }

  if (curtok() != Token::EqRarrow)
    throw PinkException("unexpected missing \"=>\" after speculation.", __FILE__, __LINE__);

  nextok();

  // while we parse the body of this procedure,
  // the procedures scope is the new enclosing
  // scope/top-of-the-scope-stack.
  // this line does double duty,
  // 1) it constructs the new scope with a reference to
  //      the old enclosing scope as it's enclosing scope.
  // 2) it pushes this new scope onto the scope stack in
  //      order to make it the new enclosing scope.
  //      just in case procedure definitions occur within.
  scopes.push(shared_ptr<SymbolTable>(new SymbolTable(scopes.top().get())));

  body = parse_term();

  Location&& rhsloc = curloc();
  Location procloc(lhsloc.first_line,
                   lhsloc.first_column,
                   rhsloc.first_line,
                   rhsloc.first_column);

  if (!poly)
    proc = shared_ptr<Ast>(new Entity(unique_ptr<Lambda>(new Lambda(args, scopes.top(), body)), procloc));
  else
  {
    for (auto&& arg : args)
      get<shared_ptr<Type>>(arg) = poly_type;
    proc = shared_ptr<Ast>(new Entity(unique_ptr<PolyLambda>(new PolyLambda(args, scopes.top(), body)), procloc));
  }

  scopes.pop();

}
else
{
  throw PinkException("unexpected missing argument after speculation.", __FILE__, __LINE__);
}


return proc;
*/

shared_ptr<Ast> Parser::parse_procedure()
{

  bool poly = false;
  string id;
  shared_ptr<Type> type;
  shared_ptr<Ast> proc, body;
  Location&& lhsloc = curloc();

  if (curtok() != Token::Backslash)
    throw PinkException("unexpected missing backslash after speculation.", __FILE__, __LINE__);

  nextok();

  if (curtok() == Token::Id)
  {
    id = curtxt();
    nextok();

    if (curtok() == Token::Colon)
    {
      nextok();
      type = parse_type();
      poly = type->is_polymorphic();
    }
    else
    {
      // if the type annotation is missing, we assume polymorphic type.
      type = shared_ptr<Type>(new MonoType(AtomicType::Poly, Location()));
      poly = true;
    }

    if (curtok() != Token::EqRarrow)
      throw PinkException("unexpected missing \"=>\" after speculation.", __FILE__, __LINE__);

    nextok();

    // while we parse the body of this procedure,
    // the procedures scope is the new enclosing
    // scope/top-of-the-scope-stack.
    // this line does double duty,
    // 1) it constructs the new scope with a reference to
    //      the old enclosing scope as it's enclosing scope.
    // 2) it pushes this new scope onto the scope stack in
    //      order to make it the new enclosing scope.
    //      just in case procedure definitions occur within.
    scopes.push(shared_ptr<SymbolTable>(new SymbolTable(scopes.top().get())));

    body = parse_term();

    Location&& rhsloc = curloc();
    Location procloc(lhsloc.first_line,
                     lhsloc.first_column,
                     rhsloc.first_line,
                     rhsloc.first_column);

    if (!poly)
      proc = shared_ptr<Ast>(new Entity(unique_ptr<Lambda>(new Lambda(id, type, scopes.top(), body)), procloc));
    else
      proc = shared_ptr<Ast>(new Entity(unique_ptr<PolyLambda>(new PolyLambda(id, type, scopes.top(), body)), procloc));
    scopes.pop();

  }
  else
  {
    throw PinkException("unexpected missing argument after speculation.", __FILE__, __LINE__);
  }


  return proc;
}


shared_ptr<Ast> Parser::parse_infix(shared_ptr<Ast> lhs, int precedence)
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
    ((some time later: actually, thinking through
    this case some more, either the next node has
    higher precedence or it doesn't, and once we
    process that node, either we are looping again
    because of the outer loop, or we are inside another
    copy of the outer loop. in either case we are
    making the inner judgement multiple times without
    directly having to loop that choice directly.))
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
  optional<pair<int, Associativity>> lookahead;

  /*
    this first test is of the shape of
    this optional type, and only iff
    the shape is filled does the second
    case get executed, which avoids a
    possible runtime error.
  */
  while ((lookahead = precedences->FindPrecAndAssoc(curtxt())) && get<0>(*lookahead) >= precedence)
  {
    string&& optxt = curtxt();
    optional<pair<int, Associativity>> curop(lookahead);

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
    lhs     op'
         rhs   rhs'

    */
    while ((lookahead = precedences->FindPrecAndAssoc(curtxt()))
           &&   (get<0>(*lookahead) > get<0>(*curop)
             || (get<0>(*lookahead) == get<0>(*curop) && get<1>(*lookahead) == Associativity::Right)))
    {
      rhs = parse_infix(move(rhs), get<0>(*lookahead));
    }

    /*
       op
    lhs  rhs

          op'
       op    rhs'
    lhs  rhs
    */
    Location binoploc(lhs->location.first_line,
                      lhs->location.first_column,
                      rhstermloc.first_line,
                      rhstermloc.first_column);
    lhs = shared_ptr<Ast>(new Binop(optxt, lhs, rhs, binoploc));
  }
  return lhs;
}

/* ------------------------------------------------------------------------ */

optional<ParserError> Parser::speculate_expression()
{
  optional<ParserError> result;

  result = speculate_term();

  if (!result)
  {
    if (speculate(Token::Semicolon))
    {
      result = speculate_expression();
    }
  }

  return result;
}


optional<ParserError> Parser::speculate_term()
{
  optional<ParserError> result;

  if (is_primary(curtok()))
  {
    result = speculate_primary();

    /*
    recall that the none shape of this optional type means success.
    */
    if (!result)
    {
      if (curtok() == Token::Operator && is_binop(curtxt()))
      {
        do
        {
          /*
          an infix/affix expression,
          modulo precedence, is:
            primary (operator primary)+
          */
            nextok(); // eat the binop
            result = speculate_primary();
        } while (curtok() == Token::Operator && is_binop(curtxt()));
      }
      else if (curtok() == Token::Larrow)
      {
        nextok();
        result = speculate_term();
      }
      else if (is_ender(curtok()))
      {
        ;
      }
      else
      {
        result = optional<ParserError>({"unknown token in primary position: [" + curtxt() + "]", curloc()});
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
     result = optional<ParserError>({"unknown token in primary position: [" + curtxt() + "]", curloc()});
  }
  return result;
}

optional<ParserError> Parser::speculate_primary()
{
  optional<ParserError> result;
  while ((curtok() != Token::Operator && is_primary(curtok()))
      || (curtok() == Token::Operator && is_unop(curtxt())))
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
  else if (speculate(Token::Id))
  {
    if (speculate(Token::ColonEquals))
    {
      result = speculate_term();
    }
  }
  else if (speculate(Token::LParen))
  {
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
      if (speculate(Token::RParen))
        ;
      else
      {
        // we saw one valid term, but then it wasn't
        // followed by an RParen or a Comma.
        result = optional<ParserError>({"missing closing ) or , after valid term", curloc()});
      }
    }
    else
    {
      // we didn't see a valid term, the reason is in result;
    }
  }
  else if (speculate(Token::Operator))
  {
    result = speculate_primary();
  }
  else if (curtok() == Token::TypeNil
        || curtok() == Token::TypeInt
        || curtok() == Token::TypeBool)
  {
    result = speculate_type();
  }
  else if (speculate(Token::While))
  {
    result = speculate_while();
  }
  else if (curtok() == Token::If)
  {
    result = speculate_if();
  }
  else if (curtok() == Token::Backslash)
  {
    result = speculate_procedure();
  }
  else if (is_ender(curtok()))
  ;
  else
  {
    // error: no valid primitive term to parse.
    result = optional<ParserError>({"unknown primitive grapheme: [" + curtxt() + "]", curloc()});
  }

  return result;
}

optional<ParserError> Parser::speculate_type()
{
  optional<ParserError> result;

  do {
    // the body of this loop needs to parse
    // a type atom, which are then composed by
    // operations.
    if (speculate(Token::TypeRef))
    {
      result = speculate_type();
    }
    else if (speculate(Token::TypeNil));
    else if (speculate(Token::TypeInt));
    else if (speculate(Token::TypeBool));
    else {
      result = optional<ParserError>({"expected to parse a type primitive here.", curloc()});
    }
    // this condition needs to check against
    // every possible type composition operator
  } while (!result && speculate(Token::Rarrow));

  return result;
}

optional<ParserError> Parser::speculate_while()
{
  optional<ParserError> result;
  if (speculate(Token::While))
  {
    result = speculate_term();
    if (!result && speculate(Token::Do))
    {
      result = speculate_term();
    }
    else
    {
      result = optional<ParserError>({"missing 'do'", curloc()});
    }
  }
  else
  {
    result = optional<ParserError>({"missing 'while'", curloc()});
  }
  return result;
}

optional<ParserError> Parser::speculate_if()
{
  optional<ParserError> result;
  if (speculate(Token::If))
  {
      result = speculate_term();
      if (!result && speculate(Token::Then))
      {
          result = speculate_term();
          if (!result && speculate(Token::Else))
          {
            result = speculate_term();
          }
          else
          {
            result = optional<ParserError>({"missing 'else'", curloc()});
          }
      }
      else
      {
        result = optional<ParserError>({"missing 'then'", curloc()});
      }
  }
  return result;
}


optional<ParserError> Parser::speculate_procedure()
{
  optional<ParserError> result;

    // start parsing the procedure literal.
    if (speculate(Token::Backslash))
    {
      // parse the argument, or argument list
      if (speculate(Token::Id))
      {
          // type annotations are optional.
          if (speculate(Token::Colon))
          {
            result = speculate_term();
          }
      }
      else
      {
        result = optional<ParserError>({"missing argument following '\\'", curloc()});
      }

      // parse the body
      if (!result && speculate(Token::EqRarrow))
      {
        result = speculate_term();
      }
      else if (!result)
      {
        result = optional<ParserError>({"missing \"=>\" after argument", curloc()});
      }
      else
        ; // in this case we want to preserve
          // the previous error message

    }

    return result;
}

optional<ParserError> Parser::speculate_arg()
{
  if (speculate(Token::Id))
    if (speculate(Token::Colon))
    {
      return speculate_type();
    }
    else
    {
      // we saw an arg!
      return optional<ParserError>();
    }

  else
  {
    return optional<ParserError>(ParserError("missing argument id", curloc()));
  }
}





















/* ------------------------------------------------- */
