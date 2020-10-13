
#pragma once
#include <string>
using std::string;
#include <stack>
using std::stack;
#include <vector>
using std::vector;
#include <set>
using std::set;
#include <memory>
using std::shared_ptr;
#include <utility>
using std::optional;

#include "Ast.hpp"
#include "Lexer.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"


enum class Token {
  Error,
  End,
  More,
  NewLn,
  Nil,
  TypeNil,
  Int,
  TypeInt,
  True,
  False,
  TypeBool,
  While,
  Do,
  If,
  Then,
  Else,
  Id,
  LParen,
  RParen,
  Comma,
  Colon,
  ColonEquals,
  Backslash,
  Rarrow,
  EqRarrow,
  Operator,
};

class ParserJudgement
{
public:
  bool success;
  union U {
    ParserError err;
    shared_ptr<Ast> term;

    U() : err("Default ParserJudgement", Location()) {}
    U(const ParserError& err) : err(err) {}
    U(const shared_ptr<Ast>& term) : term(term) {}
  } u;

  ParserJudgement() {}
  ParserJudgement(const ParserError& err) : success(false), u(err) {}
  ParserJudgement(const shared_ptr<Ast>& term) : success(true), u(term) {}

  bool succeeded() { return success; }
  operator bool()  { return success; }
};


class Parser {
  // in order to build scopes in a nested fashion
  // we utilize a stack. this is to support the
  // usage of scopes in typeing and evaluation.
  stack<SymbolTable*>   scopes
  OperatorTable*        ops;
  Lexer                 lexer;
  stack<int, vector<int>> marks;
  vector<Token>    tokbuf;
  vector<string>   txtbuf;
  vector<Location> locbuf;
  int              curidx;

public:
  Parser(const SymbolTable* const top, OperatorTable* ops);

  ParserJudgement parse(const string& text);

private:
  void reset();

  int  mark();
  void release();
  bool speculating();

  void     gettok(int n);
  void     nextok();
  Token    curtok();
  string   curtxt();
  Location curloc();

  bool is_unop(const string& op);
  bool is_binop(const string& op);

  bool is_primary(Token t);
  bool is_type_primitive(Token t);
  bool is_ender(Token t);

  shared_ptr<Ast> parse_term();
  shared_ptr<Ast> parse_primary();
  shared_ptr<Ast> parse_primitive();
  shared_ptr<Ast> parse_type();
  shared_ptr<Ast> parse_if();
  shared_ptr<Ast> parse_while();
  shared_ptr<Ast> parse_procedure();
  shared_ptr<Ast> parse_infix(shared_ptr<Ast> lhs, int precedence);

  bool speculate(Token t);
  optional<ParserError> speculate_term();
  optional<ParserError> speculate_primary();
  optional<ParserError> speculate_primitive();
  optional<ParserError> speculate_type();
  optional<ParserError> speculate_if();
  optional<ParserError> speculate_while();
  optional<ParserError> speculate_procedure();
};
