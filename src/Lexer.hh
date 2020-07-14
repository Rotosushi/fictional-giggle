
#pragma once
#include <string>
using std::string;

#include "Ast.hh"
#include "Parser.hh"

class Lexer {
  string buf;
  string::iterator end;
  string::iterator cursor;
  string::iterator marker;
  string::iterator token;
  Location loc;

public:
  void set_buffer(const string& str);
  string    yytext();
  Location& yylloc();
  Token     yylex();

private:
  void update_location(int length);
};
