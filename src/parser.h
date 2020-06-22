#ifndef PARSER_H
#define PARSER_H

#include <stdbool.h>

#include "lexer.h"
#include "ast.h"
#include "precedencetable.h"
#include "stringset.h"

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
  int bufsz;
  int mkstsz;
  PrecedenceTable* pTable;
  StringSet* binopSet;
  StringSet* unopSet;
  bool end;
} Parser;

Parser* createParser();
void    destroyParser(Parser* p);
Ast*    parse(Parser* p, struct Scanner* scanner);
bool    end_of_input(Parser* p);

#endif
