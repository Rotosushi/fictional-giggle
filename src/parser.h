#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"
#include "precedencetable.h"

struct StrLoc;
struct Scanner;

typedef enum Token {
  ERR,
  END,
  MORE,
  NIL,
  NIL_TYPE,
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
  PrecedenceTable* precTable;
  StringSet* binopSet;
  StringSet* unopSet;
} Parser;

Parser* createParser();
void    destroyParser(Parser* p);
Ast*    parse(Parser* p, struct Scanner* scanner, struct StrLoc* llocp);

#endif
