
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
using std::unique_ptr;
#include <utility>
using std::optional;

#include "llvm-10/llvm/IR/LLVMContext.h"

#include "Ast.hh"
#include "Lexer.hh"
#include "BinopTable.hh"
#include "Kernel.hh"
#include "Location.hh"
#include "Error.hh"


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
  EqRarrow,
  Operator,
};



class Lexer;

class Parser {
  // in order to build scopes in a nested fashion
  // we utilize a stack. this is identical to the
  // usage of scopes in typeing and evaluation.
  stack<SymbolTable*> scopes
  BinopTable*         binops;
  UnopTable*          unops;
  Lexer               lexer;
  stack<int, vector<int>> marks;
  vector<Token>    tokbuf;
  vector<string>   txtbuf;
  vector<Location> locbuf;
  int              curidx;

public:
  Parser(const SymbolTable* const top, const BinopTable* const binops, const UnopTable* const Unops);

  optional<unique_ptr<Ast>> parse(const string& text);

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

  unique_ptr<Ast> parse_term();
  unique_ptr<Ast> parse_primary();
  unique_ptr<Ast> parse_primitive();
  unique_ptr<Ast> parse_if();
  unique_ptr<Ast> parse_while();
  unique_ptr<Ast> parse_procedure();
  unique_ptr<Ast> parse_infix(unique_ptr<Ast> lhs, int precedence);

  bool speculate(Token t);
  optional<ParserError> speculate_term();
  optional<ParserError> speculate_primary();
  optional<ParserError> speculate_primitive();
  optional<ParserError> speculate_if();
  optional<ParserError> speculate_while();
  optional<ParserError> speculate_procedure();
};
