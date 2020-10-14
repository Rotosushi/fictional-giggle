
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
#include "ParserJudgement.hpp"

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


class Parser {
  // in order to build scopes in a nested fashion
  // we utilize a stack. this is to support the
  // usage of scopes in typeing and evaluation.
  stack<SymbolTable*>   scopes;
  OperatorTable*        ops;
  Lexer                 lexer;
  stack<int, vector<int>> marks;
  vector<Token>    tokbuf;
  vector<string>   txtbuf;
  vector<Location> locbuf;
  int              curidx;

public:
  Parser(SymbolTable* top, OperatorTable* ops);

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

  /*
  do we want the actual parse algorithm to
  work on a return type which captures the
  success/failure if we are already manually
  separating the failure into failure of the
  speculation, and the actual parsing expects
  to be given something which already speculates
  i.e. is a valid term in the grammar.
  */
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
