
#pragma once
#include <string>
using std::string;

#include "Location.hpp"
#include "Token.hpp"


class Lexer {
  string buf;
  string::iterator end;
  string::iterator cursor;
  string::iterator marker;
  string::iterator token;
  Location loc;

public:
  void set_buffer(const string& str);
  void reset();

  Token     yylex();
  string    yytxt();
  Location& yyloc();

private:
  void update_location();
};
