
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

#include "Token.hpp"
#include "Location.hpp"
#include "Lexer.hpp"
#include "BinopPrecedenceTable.hpp"
#include "BinopEliminators.hpp"
#include "UnopEliminators.hpp"
#include "SymbolTable.hpp"
#include "ParserJudgement.hpp"

class Ast;


class Parser
{
  // in order to build scopes in a nested fashion
  // we utilize a stack. this is to support the
  // usage of scopes in typeing and evaluation.
  stack<shared_ptr<SymbolTable>>     scopes;
  shared_ptr<BinopPrecedenceTable> precedences;
  shared_ptr<BinopSet>    binops;
  shared_ptr<UnopSet>     unops;
  Lexer                   lexer;
  stack<int, vector<int>> marks;
  vector<Token>           tokbuf;
  vector<string>          txtbuf;
  vector<Location>        locbuf;
  int                     curidx;

public:
  Parser(shared_ptr<SymbolTable> tpscp,
         shared_ptr<BinopPrecedenceTable> prsdncs,
         shared_ptr<BinopSet> bnps,
         shared_ptr<UnopSet>  unps);

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
  /*

  expression := term
              | term ';' expression

  term := primary
        | primary operator primary

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
        | '(' term (',' term)+ ')'

  type := Nil
        | Int
        | Bool
        | Poly
        | type -> type
  */
  shared_ptr<Ast> parse_expression(); // new
  shared_ptr<Ast> parse_term(); // modify
  shared_ptr<Ast> parse_affix(); // new
  shared_ptr<Ast> parse_primary();
  shared_ptr<Ast> parse_primitive();
  shared_ptr<Type> parse_type();
  shared_ptr<Ast> parse_if();
  shared_ptr<Ast> parse_while();
  shared_ptr<Ast> parse_procedure();
  shared_ptr<Ast> parse_infix(shared_ptr<Ast> lhs, int precedence);

  bool speculate(Token t);
  optional<ParserError> speculate_expression();
  optional<ParserError> speculate_term();
  optional<ParserError> speculate_affix();
  optional<ParserError> speculate_primary();
  optional<ParserError> speculate_primitive();
  optional<ParserError> speculate_type();
  optional<ParserError> speculate_if();
  optional<ParserError> speculate_while();
  optional<ParserError> speculate_procedure();
  optional<ParserError> speculate_arg();
};
