
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "parser.h"

/*
FILE* yyin;
FILE* yyout;
char* yytext;
int   yyleng;
int   yylineno;
int   yy_flex_debug;
StrLoc* llocp;
*/
/*
term:
      id
    | nil
    | lambda
    | call
    | bind
    | '(' term ')'
    //| int
    //| nil_type
    //| int_type
    //| binop
    //| unop
*/

Parser* createParser()
{
  Parser* result = (Parser*)malloc(sizeof(Parser));
  result->markstack = NULL;
  result->tokbuf = NULL;
  result->texbuf = NULL;
  result->locbuf = NULL;
  result->idx = 0;
  result->mkstsz = 0;
  result->bufsz = 0;
  return result;
}

void destroyParser(Parser* p)
{
  free(p->markstack);
  free(p->tokbuf);
  free(p->locbuf);
  for (int i = 1; i < p->bufsz; ++i)
    free(p->texbuf[i]);
  free(p->texbuf);
  p->idx = 0;
  p->mkstsz = 0;
  p->bufsz = 0;
  free(p);
}


int     mark(Parser* p)
{
  /*
    push the current index onto the
    mark 'stack'

    ms -> [i0, i1, .., in]
    =>
    ms -> [i0, i1, .., in, in+1]
  */
  if (p->markstack == NULL) {
    p->markstack = (int*)malloc(sizeof(int));
    p->mkstsz = 1;
  } else {
    p->mkstsz += 1;
    p->markstack = (int*)realloc(p->markstack, sizeof(int) * p->mkstsz);
  }
  p->markstack[p->mkstsz-1] = p->idx;

  return p->idx;
}

void release(Parser* p)
{
  /*
    pop the top mark off the
    mark 'stack'

    ms -> [i0, i1, .., in-1, in]
    =>
    ms -> [i0, i1, .., in-1]
  */
  if (p->markstack == NULL) {
    return;
  } else {
    int mark = p->markstack[p->mkstsz-1];
    p->mkstsz -= 1;
    p->markstack = (int*)realloc(p->markstack, sizeof(int) * p->mkstsz);
    p->idx = mark;
  }
}

Token curtok(Parser* p)
{
  if (p->tokbuf == NULL)
    return END;

  return p->tokbuf[p->idx];
}

char* curtext(Parser* p)
{
  if (p->texbuf == NULL)
    return NULL;

  return p->texbuf[p->idx];
}

StrLoc* curloc(Parser* p)
{
  if (p->locbuf == NULL)
    return NULL;

  return &p->locbuf[p->idx];
}

bool speculating(Parser* p)
{
  return p->mkstsz > 0;
}

void getTokens(Parser* p, Scanner* s, StrLoc* loc, int i)
{
  /*
    add (i) tokens to the buffer of tokens
    currently being processed.
  */
  if (p->idx + i > p->bufsz) {
    int n = (p->idx + i) - p->bufsz;
    p->tokbuf = (Token*)realloc(p->tokbuf, p->bufsz + n);
    p->texbuf = (char**)realloc(p->texbuf, p->bufsz + n);
    p->locbuf = (StrLoc*)realloc(p->locbuf, p->bufsz + n);
    for (int i = 0; i < n; ++i) {
      Token t = yylex(p, s);
    }
  }

}

void next(Parser* p, Scanner* s, StrLoc* loc)
{
  p->idx += 1;

  if (p->idx == p->bufsz && !speculating(p)) {
    p->idx = 0;
    for (int i = 0; i < p->bufsz; i++) {
      free(p->texbuf[i]);
    }

    free(p->tokbuf);
    free(p->texbuf);
    free(p->locbuf);
  }

  getTokens(p, s, 1);
}

bool speculate(Parser* parser, Scanner* scanner, StrLoc* llocp, Token token)
{
  if (token == curtok(parser))
}

Ast* parse_term(Parser* parser, Scanner* scanner, StrLoc* loc);

Ast* parse(Parser* parser, Scanner* scanner, StrLoc* loc)
{
  Ast* result = NULL;

  result = parse_term(parser, scanner, loc);

  return result;
}

Ast* parse_constant(Parser* parser, Scanner* scanner, StrLoc* loc);
Ast* parse_lambda(Parser* parser, Scanner* scanner, StrLoc* loc);
Ast* parse_id(Parser* parser, Scanner* scanner, StrLoc* loc);
Ast* parse_call(Parser* parser, Scanner* scanner, StrLoc* loc);
Ast* parse_bind(Parser* parser, Scanner* scanner, StrLoc* loc);
Ast* parse_parens(Parser* parser, Scanner* scanner, StrLoc* loc);

Ast* parse_term(Parser* parser, Scanner* scanner, StrLoc* loc)
{


}
