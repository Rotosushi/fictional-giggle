
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

void fillTokens(Parser* p, Scanner* s, StrLoc* loc, int i)
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

void nexttok(Parser* p, Scanner* s, StrLoc* loc)
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

  fillTokens(p, s, 1);
}

bool speculate(Parser* parser, Scanner* scanner, StrLoc* llocp, Token token)
{
  if (token == curtok(parser)) {
    nexttok(parser, scanner, llocp);
    return true;
  } else
    return false;
}

bool predicts_unop(Token t)
{
  switch(t) {
    /*
    case MINUS: return true
    */
    default: return false;
  }
}

bool predicts_binop(Token t)
{
  switch(t) {
    case COLONEQUALS: case RARROW:
      return true;

    default: return false;
  }
}

bool predicts_entity(Token t)
{
  switch (t) {
    case NIL: case NIL_TYPE: case ID:
    case LPAREN: case BSLASH:
    {
      return true;
    }

    default:
    {
      return false;
    }
  }
}

bool predicts_constant(Token t)
{
  switch(t) {
    case NIL: case NIL_TYPE:
    {
      return true;
    }
    default:
    {
      return false;
    }
  }
}

bool predicts_primary(Token t)
{
  /*
  primary   := entity
             | unop-expr
             | parens
             | call
  */
  switch (t) {
    case NIL: case NIL_TYPE: case ID:
    case LPAREN: case BSLASH:
    {
      return true;
    }

    default:
    {
      return false;
    }
  }
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

/*
constant := nil
          | Nil

lambda := \ id (: type)? => term

entity := id
        | constant
        | lambda

unop-expr := unop term

parens    := '(' term ')'

call      := term term

primary   := entity
           | unop-expr
           | parens
           | call

binop-expr := term binop term

term := primary
      | binop-expr


    a + b c - d
    -->> (+ a (- (b c) d))

    b c d e * f g h i
    -->> (* (b d c e) (f g h i))

    a b + c d * d e - f g
    -->> (* (+ (a b) (c d)) (- (d e) (f g)))

    (a binop b) (c binop d binop e)
    -->>(binop a b) (binop c d e)
*/


Ast* parse_constant(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_lambda(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_entity(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_call(Parser* p, Scanner* s, StrLoc* l, Ast* lhs, StrLoc* lhsloc);
Ast* parse_infix_expr(Parser* p, Scanner* s, StrLoc* l, Ast* lhs, StrLoc* lhsloc);
Ast* parse_unop(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_parens(Parser* p, Scanner* s, StrLoc* l);
Ast* parse_primary(Parser* p, Scanner* s, StrLoc* l);

Ast* parse_term(Parser* p, Scanner* s, StrLoc* l)
{
  /*
      constant := nil
                | Nil

      lambda := \ id (: type)? => term

      entity := id
              | constant
              | lambda

      unop-expr := unop term

      parens    := '(' term ')'

      call      := term term

      primary   := entity
                 | unop-expr
                 | parens
                 | primary primary

      binop-expr := term binop term

      term := primary
            | binop-expr
            // | module
            // | abstract-type
  */

  Ast* term = NULL;
  Token ct = curtok(p);
  StrLoc* termloc = curloc(p);
  if (predicts_primary(ct)) {
    term = parse_primary(p, s, l);
  }

  /*
    if we have a valid term parsed then we
    still need to try and parse possible
    binop or call expressions, if the token after the
    valid term is a binop, then we need to parse
    an operator precedence expression, if the term predicts
    another primary expression then we need to parse a call expr.

     the two most primitive operators in the language are:
     bind (:=) is a binop taking an ID term and any other term
     which creates an environmental binding between that ID and term.

     the type constructor (->) can be described in terms of
     a right-assoc binop operating on Type entities.

     (in the future we will also have the sequence symbol (;)
      which is a binop of very low precedence
      which simply evaluates it's arguments
      and discards the result of the lhs.)

      if we parsed a term then result will be
      non-null, since result is always initialized
      to NULL the only way for it to become non-null
      is by the flow of control having validly parsed
      some tokens into a Ast Node.
      if all of those parsed statements
      failed, that is conveyed by the
      result ptr being NULL.
      hence, if result is non-null
      then it must point to a valid term,
      now, any term can be followed by a binop, provided a term of the correct
      type appears on the other side. but, given we are simply
      parsing we accept any well-formed term on either side
      of any valid operator regardless of type.
  */
  if (term != NULL) {

    ct = curtok(p);
    if (predicts_binop(ct)) {
      term = parse_affix_expr(p, s, l, term, termloc);
    }
    else if (predicts_primary(ct)) {
      term = parse_call(p, s, l, term, termloc);
    } else {
      // a term by itself
    }
  }
  else {
    // error: malformed term.
  }

  return result;
}

Ast* parse_constant(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  constant := nil
            | Nil
  */
  Token ct = curtok(p);
  Ast* constant = NULL;

  if (ct == NIL) {
    constant = CreateAstEntityNil(curloc(p));
  }
  else if (ct == NIL_TYPE) {
    constant = CreateAstEntityTypeNil(curloc(p));
  } else {
    // error: not a valid constant term
  }

  // eat the constant token
  nexttok(p, s, l);

  return constant;
}

Ast* parse_lambda(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  lambda := \ id (: type)? => term
  */
  Token ct = curtok(p);
  Ast* type = NULL, *body = NULL, *lambda = NULL;
  StrLoc* begin_loc = NULL, *end_loc = NULL;
  StrLoc lambda_loc;
  char* arg_name = NULL;

  if (ct == BSLASH) {
    begin_loc = curloc(p);

    nexttok(p, s, l); // eat '\\'
    ct = curtok(p);

    if (ct == ID) {
      arg_name = curtext(p); // save the text associated with the ID token

      nexttok(p, s, l); // eat ID
      ct = curtok(p);

      if (ct == COLON) {
        nexttok(p, s, l); // eat ':'
                                    // the colon predicts a type expression,
        type = parse_term(p, s, l); // so we parse it here.
      }

      if (ct == REQARROW) {
        nexttok(p, s, l); // eat '=>' which predicts the body of the function.

        if (ct == LBRACE) { // an explicit scope can be denoted with enclosing '{''}'
          nexttok(p, s, l);

          body = parse_term(p, s, l);

          if (ct == RBRACE) {
            nexttok(p, s, l);
          } else {
            // error: missing rbrace after term
          }
        } else {
          body = parse_term(p, s, l);
        }

        end_loc = curloc(p);

        lambda_loc.first_line   = begin_loc->first_line;
        lambda_loc.first_column = begin_loc->first_column;
        lambda_loc.last_line    = end_loc->first_line;
        lambda_loc.last_column  = end_loc->first_column;

        lambda = CreateAstEntityFn(strdup(arg_name), type, body, &lambda_loc);
      } else {
        // error: expecting an REQARROW before the body of the lambda
      }
    }
  } else {
    // error: only BSLASH predicts a lambda
  }

  return lambda;
}

Ast* parse_entity(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  entity := id
          | constant
          | lambda
  */
  Token ct = curtok(p);
  Ast* entity = NULL;

  if (ct == ID) {
    entity = CreateAstId(strdup(curtext(p)));
    nexttok(p, s, l); // eat ID
  }
  else if (predicts_constant(ct)) {
    entity = parse_constant(p, s, l);
  }
  else if (ct == BSLASH) {
    entity = parse_lambda(p, s, l);
  }
  else {
    // error: malformed entity
  }

  return result;
}

Ast* parse_call(Parser* p, Scanner* s, StrLoc* l, Ast* lhs)
{
  Ast* rhs = NULL, *result == NULL;
  rhs = parse_term(p, s, l);

  if (rhs) {
    result = createAstCall(lhs, rhs);
  } else {
    ; // error: malformed rhs.
  }

  return result;
}

Ast* parse_affix_expr(Parser* p, Scanner* s, StrLoc* l, Ast* lhs, int minPrecdnce)
{
  /*
  parse_expression_1(lhs, min_precedence)
  lookahead := peek next token
  while lookahead is a binary operator whose precedence is >= min_precedence
        op := lookahead
        advance to next token
        rhs := parse_primary ()
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is greater
                 than op's, or a right-associative operator
                 whose precedence is equal to op's
            rhs := parse_expression_1 (rhs, lookahead's precedence)
            lookahead := peek next token
        lhs := the result of applying op with operands lhs and rhs
    return lhs
  */
  Token ct = curtok(p);
  while
}

Ast* parse_binop(Parser* p, Scanner* s, StrLoc* l, Ast* first_lhs)
{
    return parse_affix_expr(p, s, l, first_lhs, 0);
}

Ast* parse_unop(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  unop-expr := unop term
  */
  Token ct = curtok(p);
  Ast* result = NULL;

  if (predicts_unop(ct)) {
    /* nothing predicts_unop */
  }

  return result;
}

Ast* parse_parens(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  parens := '(' term ')'
  */
  Token ct = curtok(p);
  Ast* result = NULL;
  if (ct == LPAREN) {
    nexttok(p, s, l);

    result = parse_term(p, s, l);
    ct = curtok(p);

    if (ct == RPAREN) {
      nexttok(p, s, l);
    } else {
      // error: missing rparen after term
    }
  } else {
    // error: missing lparen?
  }
  return result;
}

Ast* parse_primary(Parser* p, Scanner* s, StrLoc* l)
{
  /*
  primary   := entity
             | unop-expr
             | parens
  */
  Token ct = curtok(p);
  Ast* result = NULL;

  if (predicts_entity(ct)) {
    result = parse_entity(p, s, l);
  }
  else if (predicts_unop(ct)) {
    result = parse_unop(p, s, l);
  }
  else if (ct == LPAREN) {
    result = parse_parens(p, s, l);
  }
  else {
    // error: malformed primary
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
