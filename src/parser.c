
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "parser.h"

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
  if (p->idx + i > p->bufsz) {       // do we need more tokens than we have?
    int n = (p->idx + i) - p->bufsz; // how many more do we need exactly?
    // add (n) more slots to each buffer.
    p->tokbuf = (Token*)realloc(p->tokbuf, p->bufsz + n);
    p->texbuf = (char**)realloc(p->texbuf, p->bufsz + n);
    p->locbuf = (StrLoc*)realloc(p->locbuf, p->bufsz + n);
    for (int i = 0; i < n; ++i) {
       /*
          grab an actual new token from the input FILE

          if the lexer is out of input, but hasn't
          yet seen the EOF, then it returns MORE,
          the only thing we need to do in this case
          is call fill on the scanner, which gets
          more input from it's FILE ptr and buffers
          it. why specifically request 'push' semantics
          from re2c? well, 'fill' is a great semantic
          unit to handle needing more input from the
          FILE to look for more declarations
          in order to support out of order declarations.
          obvously this is the other half of that process,
          and allowing the typechecker to fail with the
          known case of an undeclared ident used, and then
          retypechecking the statement at some point after
          we process more declarations (hopefully after
          seeing the declaration of the unknwon name)
          is the first half of that process.
       */
       Token t = ERR;

       while ((t = yylex(p, s, loc)) == MORE) {
         fill(s);
       }

       p->tokbuf[i] = t;
       p->texbuf[i] = yytext(scanner);
       p->locbuf[i] = *loc;
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
  if (token == curtok(parser)) {
    next(parser, scanner, llocp);
    return true;
  } else
    return false;
}

Ast* parse_term(Parser* p, Scanner* s, StrLoc* l);
bool speculate_term(Parser* p, Scanner* s, StrLoc* l);

Ast* parse(Parser* parser, Scanner* scanner, StrLoc* loc)
{
  Ast* result = NULL;

  mark(parser);
  bool term = speculate_term(parser, scanner, loc);
  release(parser);

  if (term)
    result = parse_term(parser, scanner, loc);

  return result;
}

Ast* parse_constant(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_lambda(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_id(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_call(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_bind(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_parens(Parser* p, Scanner* s, StrLoc* l);

Ast* parse_term(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  term :=
          nil
        | Nil
        | id
        | \ id (: type)? => term
        | term term
        | term binop term
        | unop term
        | '(' term ')'

        entity     -> id, nil, Nil, (\ id (: type)? => term)

        call       -> term term

        binop-expr -> term binop term

        unop-expr  -> unop term

        parens     -> '(' term ')'

  */
  Ast* result = NULL;
  Token ct = curtok(p);
  if (ct == NIL || ct == NIL_TYPE) {
    result = parse_constant(p, s, l);
  }
  else if (ct == ID) {
    result = parse_id(p, s, l);
  }
  else if (ct == BSLASH) {
    result = parse_lambda(p, s, l);
  }
  else if (ct == LPAREN) {
    result = parse_parens(p, s, l);
  }

  ct = curtok(p);

  if (result) {
    if (is_binop(ct)) {
      result = parse_binop(p, s, l, result);
    }
    else {
      /*
        what predicts a call expression?
        well, an entity is what can be called
        or passed as argument. as well as a
        unop or a parenthized term.
      */
    }
  }

  return result;
}

bool speculate_constant(Parser* p, Scanner* s, StrLoc* l);
bool speculate_lambda(Parser* p, Scanner* s, StrLoc* l);
bool speculate_entity(Parser* p, Scanner* s, StrLoc* l);
bool speculate_binop(Parser* p, Scanner* s, StrLoc* l);
bool speculate_unop(Parser* p, Scanner* s, StrLoc* l);

bool speculate_term(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  term :=
          nil
        | Nil
        | id
        | \ id (: type)? => term
        | term term
        | term binop term
        | unop term
        | '(' term ')'

        entity     -> id, nil, Nil, (\ id (: type)? => term)

        call       -> term term

        binop-expr -> term binop term

        unop-expr  -> unop term

        parens     -> '(' term ')'

  */

  bool result = true;
  // it = the token sequence being parsed
  if (speculate_entity(p, s, l)) {   // is it an entity?

  }

  else if (speculate_unop(p, s, l)) {  // is it a unop expression?
    if (speculate_term(p, s, l));
    else {
      /* error: term after the unop is malformed */
      result = false;
    }
  }

  else if (speculate(p, s, l, LPAREN)) {
    if (speculate_term(p, s, l)) {
      if (speculate(p, s, l, RPAREN));
      else {
        /*  error: no postfix RPAREN after a well formed term */
        result = false;
      }
    }
    else {
      /* error: the parenthized term is malformed */
      result = false;
    }
  }

  else {
    // error: not an entity, a unop term or a LPAREN
    result = false;
  }

  /*
    we either have or have not parsed a single entity,
    unop term, or parenthesized term. but a few of
    the expressions are represented by a continuation
    of terms after the first. the two major cases
    are calls and binops. both take two terms as
    arguments. meaning that we must be able to parse
    to terms before we can consider this term well-formed.
  */

  if (result) {
    if (speculate_binop(p, s, l)) {  // is it an expression?
      if (speculate_term(p, s, l))); // is the rhs parsable?
      else {
        /* error: rhs of binop expr is malformed */
        result = false;
      }
    }
    else if (speculate_term(p, s, l)) { // is it a call?
        /*
          try and parse any number of terms in sequence

          pretty sure that the recursion here takes care of
          the 'any number' part of the above statement

          does this recur forever?
          it would if we didn't check for the well-formedness
          of the term above. then the terminating condition of
          END, RPAREN, RBRACE, wouldn't terminate and we would always
          call speculate_term above.
        */
    }
    else ;  // it's an entity by itself.
  }

  return result;
}

bool speculate_constant(Parser* p, Scanner* s, StrLoc* l)
{
  bool result = true;
  if      (speculate(p, s, l, NIL));
  else if (speculate(p, s, l, NIL_TYPE));
  else result = false;
  return result;
}


bool speculate_lambda(Parser* p, Scanner* s, StrLoc* l)
{
  bool result = true;
  if (speculate(p, s, l, BSLASH)) {
    if (speculate(p, s, l, ID)) {
      if (speculate(p, s, l, COLON)) {
        if (speculate_term(p, s, l));
        else {
          // error: type annotation not well formed
          result = false;
        }
      }

      if (speculate(p, s, l, REQARROW)) {
        if (speculate(p, s, l, LBRACE)) {
          if (speculate_term(p, s, l)) {
            if (speculate(p, s, l, RBRACE));
            else
              // error: no trailing RBRACE
              result = false;
          }
          else
            // error: no well formed term after the LBRACE
            result = false;
        }
        else if (speculate_term(p, s, l)) ;
        else {
          // error: lambda body not well formed
          result = false;
        }
      }
      else {
        // error: expected '=>' after ID or type annotation.
        result = false;
      }
    } else {
      // error: expected ID after '\'
      result = false;
    }
  } else {
    // error: expected '\' to start lambda term
    result = false;
  }
  return result;
}

bool speculate_entity(Parser* p, Scanner* s, StrLoc* l)
{
  bool result = true;
  if      (speculate_constant(p, s, l));
  else if (speculate_lambda(p, s, l));
  else result = false;
  return result;
}

bool speculate_binop(Parser* p, Scanner* s, StrLoc* l)
{
  bool result = true;
  if      (speculate(p, s, l, COLONEQUALS));
  else if (speculate(p, s, l, RARROW));
  else result = false;
  return result;
}

bool speculate_unop(Parser* p, Scanner* s, StrLoc* l)
{
  bool result = true;
  result = false;
  return result;
}


































/* -------------------------------------------------------------------------- */
