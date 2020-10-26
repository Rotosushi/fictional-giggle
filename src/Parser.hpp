
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
#include "Token.hpp"
#include "Location.hpp"
#include "Lexer.hpp"
#include "Environment.hpp"
#include "ParserJudgement.hpp"



class Parser
{
  // in order to build scopes in a nested fashion
  // we utilize a stack. this is to support the
  // usage of scopes in typeing and evaluation.
  stack<shared_ptr<SymbolTable>>     scopes;
  Environment             env;
  Lexer                   lexer;
  stack<int, vector<int>> marks;
  vector<Token>           tokbuf;
  vector<string>          txtbuf;
  vector<Location>        locbuf;
  int                     curidx;

public:
  Parser(Environment env);

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

  the one consideration is that this return signature
  forces our hand into making these procedures
  throw upon reaching an error. (which i suppose is
  fine, given that throwing here signals an implementation
  defect rather than an actual syntax error.)
  */
  shared_ptr<Ast> parse_term();
  shared_ptr<Ast> parse_primary();
  shared_ptr<Ast> parse_primitive();
  shared_ptr<Type> parse_type_annotation();
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
