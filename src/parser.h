#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

struct StrLoc;

typedef enum Token {
  END,
  NIL,
  ID,
  COLON,
  COLONEQUALS,
  BSLASH,
  RARROW,
  REQARROW,
  SEMICOLON,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
} Token;

typedef struct Parser {
  int*     markstack;
  Token*   tokbuf;
  char**   texbuf;
  StrLoc*  locbuf;
  int idx;
  int mkstsz;
  int bufsz;
} Parser;

Parser* createParser();
void destroyParser(Parser* p);
Ast* parse(Parser* p, yyscan_t scanner, struct StrLoc* loc);

#endif
