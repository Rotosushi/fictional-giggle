
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

#include "llvm-10/llvm/IR/LLVMContext.h"

#include "Ast.hh"
#include "Lexer.hh"
#include "OperatorTable.hh"
#include "Kernel.hh"


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
  If,
  Then,
  Else,
  Id,
  LParen,
  RParen,
  Colon,
  ColonEquals,
  Backslash,
  EqRarrow,
  Operator,
};

class Lexer;

class Parser {
  LLVMContext* ctx;
  stack<int, vector<int>> marks;
  vector<Token>    tokbuf;
  vector<string>   txtbuf;
  vector<Location> locbuf;
  Lexer            lexer;
  OperatorTable    typeops;
  OperatorTable    binops;
  set<string>      unops;
  int              curidx;

public:
  Parser(LLVMContext* c) : ctx(c) {}

  optional<unique_ptr<Ast>> parse(const string& text);

private:
  void reset();

  int  mark();
  void release();
  bool speculating();

  void     fillTokens(int n);
  void     nextok();
  Token    curtok();
  string   curtxt();
  Location curloc();

  bool is_unop(const string& op);
  bool is_binop(const string& op);
  bool is_typeop(const string& op);

  bool is_primary(Token t);
  bool is_type_primitive(Token t);
  bool is_ender(Token t);

  unique_ptr<Ast> parse_term();
  unique_ptr<Ast> parse_primary();
  unique_ptr<Ast> parse_if();
  unique_ptr<Ast> parse_procedure();
  unique_ptr<Ast> parse_type_primitive();
  unique_ptr<Ast> parse_type_annotation();
  unique_ptr<Ast> parse_infix(unique_ptr<Ast> lhs, int precedence);

  bool speculate(Token t);
  bool speculate_term();
  bool speculate_primary();
  bool speculate_if();
  bool speculate_procedure();
  bool speculate_type_primitive();
};
