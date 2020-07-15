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
  marks.pop();
}

bool Parser::speculating()
{
  return marks.size() > 0;
}

void Parser::fillTokens(int i)
{
  if (curidx + i > tokbuf.size()) { // do we need more tokens than we have?
    int n = (curidx + i) - tokbuf.size(); // how many tokens do we need?

    for (int j = 0; j < n; j++) {
      tokbuf.push_back(lexer.yylex());
      txtbuf.push_back(lexer.yytext());
      locbuf.push_back(lexer.yylloc());
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
  if (unop != unops.end()) {
    return true;
  } else {
    return false;
  }
}

bool Parser::is_binop(const string& op)
{
  auto binop = binops.find(op);
  if (binop) {
    return true;
  } else {
    return false;
  }
}

bool Parser::is_primary(Token t)
{
  switch(t) {
    case Token::Nil: case Token::TypeNil:
    case Token::Int: case Token::TypeInt:
    case Token::True: case Token::False:
    case Token::TypeBool: case Token::If:
    case Token::Id: case Token::Backslash:
    case Token::Operator: case Token::LParen:
      return true;
    default:
      return false;
  }
}

bool Parser::is_ender(Token t)
{
  switch(t) {
    case Token::RParen: case Token::End:
    case Token::NewLn: case Token::EqRarrow:
    case Token::Then: case Token::Else:
    case Token::If:
      return true;
    default:
      return false;
  }
}

bool Parser::speculate(Token t)
{
  if (t == curtok()) {
    nextok();
    return true;
  } else {
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

  Location&& lhsloc = curloc();
  if (is_primary(curtok())) {
    lhs = parse_primary();
  }

  if (lhs) {
    if (is_primary(curtok())) {
      do {
        rhs = parse_primary();
        Location&& rhsloc = curloc();
        Location callloc = Location(lhsloc.first_line,
                                    lhsloc.first_column,
                                    rhsloc.first_line,
                                    rhsloc.first_column);

        lhs = unique_ptr<Ast>(new Call(move(lhs), move(rhs), callloc));
      } while (is_primary(curtok()));
    }
    else if (is_binop(curtxt())) {
      lhs = parse_infix(move(lhs), 0);
    }
  }

  return move(lhs);
}

unique_ptr<Ast> Parser::parse_primary()
{
  unique_ptr<Ast> lhs;
  Location&& lhsloc = curloc();
  switch(curtok()) {
    /* a variable by itself, or a bind expression. */
    case Token::Id: {
      string&& id = curtxt();
      nextok(); // eat Id

      if (curtok() == Token::ColonEquals) {
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
      } else {
        lhs = unique_ptr<Ast>(new Variable(id, lhsloc));
      }
      break;
    }

    case Token::Nil: {
      auto niltype = new MonoType(PrimitiveType::Nil);
      lhs = unique_ptr<Ast>(new Entity(*niltype, '\0', lhsloc));
      nextok();
      break;
    }

    case Token::TypeNil: {
      auto niltype = new MonoType(PrimitiveType::Nil);
      lhs = unique_ptr<Ast>(new Entity(*niltype, lhsloc));
      nextok();
      break;
    }

    case Token::Int: {
      int value    = stoi(curtxt());
      auto inttype = new MonoType(PrimitiveType::Int);
      lhs = unique_ptr<Ast>(new Entity(*inttype, value, lhsloc));
      nextok();
      break;
    }

    case Token::TypeInt: {
      auto inttype = new MonoType(PrimitiveType::Int);
      lhs = unique_ptr<Ast>(new Entity(*inttype, lhsloc));
      nextok();
      break;
    }

    case Token::True: {
      auto booltype = new MonoType(PrimitiveType::Bool);
      lhs = unique_ptr<Ast>(new Entity(*booltype, true, lhsloc));
      nextok();
      break;
    }

    case Token::False: {
      auto booltype = new MonoType(PrimitiveType::Bool);
      lhs = unique_ptr<Ast>(new Entity(*booltype, false, lhsloc));
      nextok();
      break;
    }

    case Token::TypeBool: {
      auto booltype = new MonoType(PrimitiveType::Bool);
      lhs = unique_ptr<Ast>(new Entity(*booltype, lhsloc));
      nextok();
      break;
    }

    case Token::Operator: {
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

    case Token::LParen: {
      nextok();

      auto term = parse_term();

      if (curtok() == Token::RParen) {
        nextok();
        lhs = move(term);
      } else {
        // error: missing closing right parenthesis.
      }
    }

    case Token::If: {
      lhs = parse_if();
      break;
    }

    case Token::Backslash: {
      lhs = parse_procedure();
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
  if (curtok() == Token::If) {
    nextok();

    test = parse_term();

    if (curtok() == Token::Then) {
      nextok();

      first = parse_term();

      if (curtok() == Token::Else) {
        nextok();

        second = parse_term();
        Location&& rhsloc = curloc(), condloc(lhsloc.first_line,
                                              lhsloc.first_column,
                                              rhsloc.first_line,
                                              rhsloc.first_column);
        cond = unique_ptr<Ast>(new Cond(move(test), move(first), move(second), condloc));
      }
      else {
        // error: missing else
      }
    }
    else {
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
  if (curtok() == Token::Backslash) {
    nextok();

    if (curtok() == Token::Id) {
      string&& id = curtxt();
      nextok();

      if (curtok() == Token::Colon) {
        nextok();
        type = parse_term();
      }
      else {
        auto polytype = new PolyType();
        type = unique_ptr<Ast>(new Entity(*polytype, Location()));
        poly = true;
      }

      if (curtok() == Token::EqRarrow) {
        nextok();
        body = parse_term();
        Location&& rhsloc = curloc();
        Location procloc(lhsloc.first_line,
                         lhsloc.first_column,
                         rhsloc.first_line,
                         rhsloc.first_column);

        auto undeftype = new UndefType();
        proc = unique_ptr<Ast>(new Entity(*undeftype, Procedure(id, move(type), move(body)), move(poly), procloc));
      }
      else {
        // error: expecting '=>' to predict function body
      }

    }
    else {
      // error: expecting Id immediately after '\\'
    }
  }
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
  */
  optional<pair<int, Assoc>> lopPrec;
  while ((lopPrec = binops.find(curtxt())) && get<int>(*lopPrec) >= precedence) {
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

    while ((lopPrec = binops.find(curtxt()))
           &&   (get<int>(*lopPrec) > get<int>(*op)
             || (get<int>(*lopPrec) == get<int>(*op) && get<Assoc>(*lopPrec) == Assoc::Right)))
    {
      rhs = move(parse_infix(move(rhs), get<int>(*lopPrec)));
    }

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

  if (is_primary(curtok())) {
    result = speculate_primary();
  }

  if (result) {
    if (is_primary(curtok())) {
      do {
        result = speculate_primary();
        if (!result) break;
      } while (is_primary(curtok()));
    }
    else if (is_binop(curtxt())) {
      do {
        nextok(); // eat the binop
        result = speculate_primary();
        if (!result) break;
      } while (is_binop(curtxt()));
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
  else if (curtok() == Token::If) {
    result = speculate_if();
  }
  else if (curtok() == Token::Backslash) {
    result = speculate_procedure();
  }
  else if (is_ender(curtok()));
  else result = false;

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
