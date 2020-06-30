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
  NEWLN,
  MORE,
  BINOP,
  UNOP,
  NIL,
  NIL_TYPE,
  ID,
  COLON,
  COLONEQUALS,
  BSLASH,
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
  char**   txtbuf;
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
void    printParsedTokens(Parser* p);
char*   tokenToString(Token t);

#endif
