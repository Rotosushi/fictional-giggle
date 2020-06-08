
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "parser.h"
#include "lexer.h"
#include "precedencetable.h"
#include "stringset.h"
#include "ast.h"
#include "error.h"

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

void InitializePrecedenceTable(PrecedenceTable* pTable)
{
  /*
  my previous attempt at the precedence table
  obviously we want to maintain the same precedence
  relations between the common math symbols. and also
  understanding that for many, the c precedence table
  is de-facto standard for programming languages.

  so, starting from the perspective of emulating mathematics
  we want to preform basic actions on numbers/entities.
  3 + 4 * 5 HAS TO parse to (* 5 (+ 3 4))
  when a programmer wants to preform bitwise operations
  on numbers, they probably want the bitwise operations to
  operate on numbers which have already been manipulated
  to some final value.
  x || y * z,
  likewise when we go up a step to the logical connectives,
  we want to test for truth and falsehood upon values which
  have been fully operated upon. a + b > c * d.

  the same argument is made when we take another step up to
  equality comparison.
  say to the case of
  a + b > c * d == e - f < g \ h

  we want to test for equality between the largest lhs and right we can
  group together. because we are assuming the programmer wants to compare
  between fully evaluated terms, which implies gathering as many operations
  into the evaluation tree before we insert the comparison operation.

  and, the same argument extends to the programming language specific operators
  ',', ';', ':=', '[', ']', '(', ')', '{', '}'
  and the type connectives '->', '+', '|', '&', '!'

  why do I invert the logical symbols and the bitwise symbols?
  well, for logical consistency, and the fact that logical connectives
  are more common than bitwise operations, by a wide-margin.
  (and now, LSHIFT <<, and RSHIFT >>, align with the rest of the
   bitwise operators OR ||, and &&, and xor ^^, in being two symbol operators.
  )
  when we consider equals and not equals, =, ~= resp. one can probably intuit
  the meaning of the compound symbol ~= just from knowing that = means equal-to
  and ~ means logical-negation. this also aligns with ~ being the logical not.
  instead of ! as in c. why is that? well, the operator *, is used to represent
  type kinds in the theory, and it would be nice to align the language to some
  theory symbolically. since we are deciding to reapropriate * for types, then
  it could create confusion to also use it as the indirection unop. so we will
  pay some homage to ML by taking ! to be the indirection operator. which takes
  ! away from the logical commectives, because again too much overloading
  increases the cognitive complexity of the kernel, and so we must select
  a new symbol for logical-negation.
  hence, ~ for logical-negation, and ~~ for bitwise negation. notice how
  this aligns with every bitwise operator, being composed of a repetition
  of some other operator. when a programmer sees a || instead of a | it should
  always be able to be read as a bitwise operation. this at least holds for the
  kernel, obviously if we give programmers the tools to both overload existing
  operators, and define new operators, they can define new operators which
  negate the truth of the above statement quite easily. but that is separate
  from the logical consistency of the kernel.

  (normally in ML ~ is the unop minus to the binop minus -, we just use the
   fact that unary minus always appears in prefix position and disambiguate
   by the position instead of symbol the operation to be carried out. meaning
   both forms of minus are symbolically stated by the - symbol, this is in
   some sense of the word, overloading the symbol with two meanings, however
   since the two meanings are entirely disambiguated by the grammar there is no
   need to consider any special logic to support the two definitions, we can
   )


    ptable[T_COMMA] = 1;
  	ptable[T_EQ] = 2;

  	ptable[T_EQUALS] = 3;
  	ptable[T_NOT_EQUALS] = 3;

  	ptable[T_LESS] = 4;
  	ptable[T_GREATER] = 4;
  	ptable[T_LESS_EQUALS] = 4;
  	ptable[T_GREATER_EQUALS] = 4;

  	ptable[T_OR] = 5;
  	ptable[T_XOR] = 6;
  	ptable[T_AND] = 7;

  	ptable[T_BIT_OR] = 8;
  	ptable[T_BIT_XOR] = 9;
  	ptable[T_BIT_AND] = 10;

  	ptable[T_BIT_LSHIFT] = 11;
  	ptable[T_BIT_RSHIFT] = 11;

  	ptable[T_ADD] = 12;
  	ptable[T_SUB] = 12;

  	ptable[T_MULT] = 13;
  	ptable[T_DIV] = 13;
  	ptable[T_MOD] = 13;
  */

  InsertOpPrec(pTable, ":=", 2, A_LEFT);
  InsertOpPrec(pTable, "->", 3, A_RIGHT);
}

void InitializeBinops(StringSet* binopSet)
{
  appendStr(":=", binopSet);
  appendStr("->", binopSet);
}

void InitializeUnops(StringSet* unopSet)
{

}

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
  result->pTable = CreatePrecedenceTable();
  InitializePrecedenceTable(result->pTable);
  InitializeBinops(result->binopSet);
  InitializeUnops(result->unopSet);
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
  DestroyPrecedenceTable(p->pTable);
  DestroyStringSet(p->binopSet);
  DestroyStringSet(p->unopSet);
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

void fillTokens(Parser* p, Scanner* s, int i)
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

       while ((t = yylex(p, s)) == MORE) {
         yyfill(s);
       }

       p->tokbuf[i] = t;
       p->texbuf[i] = yytext(s);
       p->locbuf[i] = *yylloc(s);
    }
  }

}

void nexttok(Parser* p, Scanner* s)
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

bool speculate(Parser* parser, Scanner* scanner, Token token)
{
  if (token == curtok(parser)) {
    nexttok(parser, scanner);
    return true;
  } else
    return false;
}

bool predicts_unop(Parser* p, char* op)
{
  if (isMember(op, p->unopSet)) return true;
  else return false;
}

bool predicts_binop(Parser* p, char* op)
{
  if (isMember(op, p->binopSet)) return true;
  else return false;
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

bool predicts_literal(Token t)
{
  switch(t) {
    case NIL: case NIL_TYPE: case BSLASH:
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

Ast* parse_term(Parser* p, Scanner* s);
bool speculate_term(Parser* p, Scanner* s);

Ast* parse(Parser* parser, Scanner* scanner)
{
  Ast* result = NULL;

  mark(parser);
  bool term = speculate_term(parser, scanner);
  release(parser);

  if (term)
    result = parse_term(parser, scanner);

  return result;
}

/*
literal := nil
         | lambda

lambda := \ id (: type)? => term

type := Nil
      | type '->' type

entity := id
        | literal
        | type

unop-expr := unop term

parens    := '(' term ')'

call      := term term

primary   := entity
           | unop-expr
           | parens

binop-expr := term binop term

term := primary
      | binop-expr
      | call


    a + b c - d
    -->> (+ a (- (b c) d))

    b c d e * f g h i
    -->> (* (b d c e) (f g h i))

    a b + c d * d e - f g
    -->> (* (+ (a b) (c d)) (- (d e) (f g)))

    (a binop b) (c binop d binop e)
    -->>(binop a b) (binop c (binop d e))
*/


Ast* parse_literal(Parser* p, Scanner* s);
Ast* parse_lambda(Parser* p, Scanner* s);
Ast* parse_entity(Parser* p, Scanner* s);
Ast* parse_call(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_binop(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_infix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrec);
Ast* parse_unop(Parser* p, Scanner* s);
Ast* parse_parens(Parser* p, Scanner* s);
Ast* parse_primary(Parser* p, Scanner* s);

Ast* parse_term(Parser* p, Scanner* s)
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
  StrLoc* termlhsloc = curloc(p), *termrhsloc = NULL;
  if (predicts_primary(ct)) {
    term = parse_primary(p, s);
    termrhsloc = curloc(p);
  }

  /*
    if we have a valid term parsed then we
    still need to try and parse possible
    binop or call expressions, if the token after the
    valid term is a binop, then we need to parse
    an operator precedence expression, if the term predicts
    another primary expression then we need to parse a call expr.
    otherwise, there is no valid term after the first one parsed
    and we can return the term by itself.

     the two most primitive operators in the language are:
     bind (:=) is a binop taking an ID entity and any other term
     which creates an environmental binding between that ID and term.

     the type constructor (->) can be described in terms of
     a right-assoc binop operating on Type entities.

     (in the future we will also have the sequence symbol (;)
      which is a binop of very low precedence
      which simply evaluates it's arguments
      and discards the result of the lhs.)
  */
  if (term != NULL) {
    StrLoc LhsLoc;
    LhsLoc.first_line   = termlhsloc->first_line;
    LhsLoc.first_column = termlhsloc->first_column;
    LhsLoc.last_line    = termrhsloc->last_line;
    LhsLoc.last_column  = termrhsloc->last_column;
    ct = curtok(p);
    char* ctxt = curtext(p);
    if (predicts_binop(p, ctxt)) {
      term = parse_binop(p, s, term, &LhsLoc);
    }
    else if (predicts_primary(ct)) {
      term = parse_call(p, s, term, &LhsLoc);
    } else {
      // a term by itself
    }
  }
  else {
    // error: malformed term.
  }

  return term;
}

Ast* parse_literal(Parser* p, Scanner* s)
{
  /*
  constant := nil
            | Nil
            | lambda
  */
  Token ct = curtok(p);
  Ast* literal = NULL;

  if (ct == NIL) {
    literal = CreateAstEntityLiteralNil(curloc(p));
    // eat the nil
    nexttok(p, s);
  }
  else if (ct == NIL_TYPE) {
    literal = CreateAstEntityTypeNil(curloc(p));
    // eat the Nil
    nexttok(p, s);
  }
  else if (ct == BSLASH) {
    literal = parse_lambda(p, s);
  }
  else {
    // error: not a valid constant term
  }

  return literal;
}

Ast* parse_lambda(Parser* p, Scanner* s)
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

    nexttok(p, s); // eat '\\'
    ct = curtok(p);

    if (ct == ID) {
      arg_name = curtext(p); // save the text associated with the ID token

      nexttok(p, s); // eat ID
      ct = curtok(p);

      if (ct == COLON) {
        nexttok(p, s); // eat ':'
                                    // the colon predicts a type expression,
        type = parse_term(p, s); // so we parse it here.
      } else {
        type = CreateAstEntityTypePoly();
      }

      if (ct == REQARROW) {
        nexttok(p, s); // eat '=>' which predicts the body of the function.

        if (ct == LBRACE) { // an explicit scope can be denoted with enclosing '{''}'
          nexttok(p, s);

          body = parse_term(p, s);

          if (ct == RBRACE) {
            nexttok(p, s);
          } else {
            // error: missing rbrace after term
          }
        } else {
          body = parse_term(p, s);
        }

        if (body == NULL) {
          error_abort("unexpected NULL from parsed lambda body! aborting", __FILE__, __LINE__);
        }

        end_loc = curloc(p);

        lambda_loc.first_line   = begin_loc->first_line;
        lambda_loc.first_column = begin_loc->first_column;
        lambda_loc.last_line    = end_loc->first_line;
        lambda_loc.last_column  = end_loc->first_column;

        lambda = CreateAstEntityLiteralProc(strdup(arg_name), type, body, &lambda_loc);
      } else {
        // error: expecting an REQARROW before the body of the lambda
      }
    }
  } else {
    // error: only BSLASH predicts a lambda
  }

  return lambda;
}

Ast* parse_entity(Parser* p, Scanner* s)
{
  /*
  entity := id
          | literal
  */
  Token ct = curtok(p);
  Ast* entity = NULL;


  if (ct == ID) {
    entity = CreateAstEntityId(strdup(curtext(p)), curloc(p));
    nexttok(p, s); // eat ID
  }
  else if (predicts_literal(ct)) {
    entity = parse_literal(p, s);
  }

  else {
    // error: malformed entity
  }

  return entity;
}

Ast* parse_call(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc)
{
  Ast* rhs = NULL, *call = NULL;
  rhs = parse_term(p, s);
  StrLoc* rhsloc = curloc(p);

  if (rhs) {
    StrLoc callloc;
    callloc.first_line   = lhsloc->first_line;
    callloc.first_column = lhsloc->first_column;
    callloc.last_line    = rhsloc->first_line;
    callloc.last_column  = rhsloc->first_column;
    call = CreateAstCall(lhs, rhs, &callloc);
  } else {
    ; // error: malformed rhs.
  }

  return call;
}

Ast* parse_affix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrecdnce)
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

  // peek at the operator that triggered this call of parse_affix_expr;
  Token lookahead   = curtok(p);
  char* loptxt      = curtext(p);
  int   lopPrec     = -1;

  // collapse all lhs expressions which rotate a new top of the list
  // into a single lhs.
  while (predicts_binop(p, loptxt) && (lopPrec = LookupOpPrec(p->pTable, loptxt)) >= minPrecdnce) {
    Token optok = lookahead; // save the current top op of the tree
    char* optxt = loptxt;

    nexttok(p, s);
    StrLoc* llocp = curloc(p);
    StrLoc rhsloc;

    rhsloc.first_line   = llocp->first_line;
    rhsloc.first_column = llocp->first_column;
    // rhs := parse_primary()
    Ast* rhs   = parse_primary(p, s);
    llocp = curloc(p);
    rhsloc.last_line   = llocp->last_line;
    rhsloc.last_column = llocp->last_column;

    // collapse all rhs expressions which rotate a new top of the list into
    // a single rhs, (this means operators with strictly higher precedence,
    // and right associative operators with equal precedence.) in both
    // of these cases the rhs needs to be parsed as if it was a brand new lhs.
    // this case it contained in a loop, because it could be that we need to
    // gather more than one instance of this case into a single tree.
    lookahead  = curtok(p);
    loptxt     = curtext(p);
    while (predicts_binop(p, loptxt) &&  \
        ((LookupOpPrec(p->pTable, loptxt) > lopPrec) ||
        ((LookupOpPrec(p->pTable, loptxt) == lopPrec) && (LookupOpAssoc(p->pTable, loptxt) == A_RIGHT))))
    {
      rhs = parse_affix_expr(p, s, rhs, &rhsloc, lopPrec);
      lookahead  = curtok(p);
      loptxt     = curtext(p);
    }

    // create a new top node of the tree by attaching the previous
    // good lhs, and the result of collapsing the rhs into a proper
    // lhs; to the lhs and rhs of the new top.
    // in the case that we encountered some number of equal precedence
    // operations upon entities, we push these operations into the lhs
    // tree via this call and the while loop.
    StrLoc binoploc;
    binoploc.first_line   = lhsloc->first_line;
    binoploc.first_column = lhsloc->first_column;
    binoploc.last_line    = rhsloc.last_line;
    binoploc.last_column  = rhsloc.last_column;
    lhs = CreateAstBinop(optxt, lhs, rhs, &binoploc);
  }
  return lhs;
}

Ast* parse_binop(Parser* p, Scanner* s, Ast* firstlhs, StrLoc* lhsloc)
{
    return parse_affix_expr(p, s, firstlhs, lhsloc, 0);
}

Ast* parse_unop(Parser* p, Scanner* s)
{
  /*
  unop-expr := unop term
  */
  Token ct = curtok(p);
  char* ctxt = curtext(p);
  Ast* result = NULL;

  if (predicts_unop(p, ctxt)) {
    /* nothing predicts_unop */
  }

  return result;
}

Ast* parse_parens(Parser* p, Scanner* s)
{
  /*
  parens := '(' term ')'
  */
  Token ct = curtok(p);
  Ast* result = NULL;
  if (ct == LPAREN) {
    nexttok(p, s);

    result = parse_term(p, s);
    ct = curtok(p);
    if (ct == RPAREN) {
      nexttok(p, s);

    } else {
      // error: missing rparen after term
    }
  } else {
    // error: missing lparen?
  }
  return result;
}

Ast* parse_primary(Parser* p, Scanner* s)
{
  /*
  primary   := entity
             | unop-expr
             | parens
  */
  Token ct = curtok(p);
  char* ctxt = curtext(p);
  Ast* result = NULL;

  if (predicts_entity(ct)) {
    result = parse_entity(p, s);
  }
  else if (predicts_unop(p, ctxt)) {
    result = parse_unop(p, s);
  }
  else if (ct == LPAREN) {
    result = parse_parens(p, s);
  }
  else {
    // error: malformed primary
  }

  return result;
}

bool speculate_constant(Parser* p, Scanner* s);
bool speculate_lambda(Parser* p, Scanner* s);
bool speculate_entity(Parser* p, Scanner* s);
bool speculate_binop(Parser* p, Scanner* s);
bool speculate_unop(Parser* p, Scanner* s);

bool speculate_term(Parser* p, Scanner* s)
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
  if (speculate_entity(p, s)) {   // is it an entity?

  }

  else if (speculate_unop(p, s)) {  // is it a unop expression?
    if (speculate_term(p, s));
    else {
      /* error: term after the unop is malformed */
      result = false;
    }
  }

  else if (speculate(p, s, LPAREN)) {
    if (speculate_term(p, s)) {
      if (speculate(p, s, RPAREN));
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
    if (speculate_binop(p, s)) {  // is it an expression?
      if (speculate_term(p, s)); // is the rhs parsable?
      else {
        /* error: rhs of binop expr is malformed */
        result = false;
      }
    }
    else if (speculate_term(p, s)) { // is it a call?
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

bool speculate_constant(Parser* p, Scanner* s)
{
  bool result = true;
  if      (speculate(p, s, NIL));
  else if (speculate(p, s, NIL_TYPE));
  else result = false;
  return result;
}


bool speculate_lambda(Parser* p, Scanner* s)
{
  bool result = true;
  if (speculate(p, s, BSLASH)) {
    if (speculate(p, s, ID)) {
      if (speculate(p, s, COLON)) {
        if (speculate_term(p, s));
        else {
          // error: type annotation not well formed
          result = false;
        }
      }

      if (speculate(p, s, REQARROW)) {
        if (speculate(p, s, LBRACE)) {
          if (speculate_term(p, s)) {
            if (speculate(p, s, RBRACE));
            else
              // error: no trailing RBRACE
              result = false;
          }
          else
            // error: no well formed term after the LBRACE
            result = false;
        }
        else if (speculate_term(p, s));
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

bool speculate_entity(Parser* p, Scanner* s)
{
  bool result = true;
  if      (speculate_constant(p, s));
  else if (speculate_lambda(p, s));
  else result = false;
  return result;
}

bool speculate_binop(Parser* p, Scanner* s)
{
  bool result = true;
  if      (speculate(p, s, COLONEQUALS));
  else if (speculate(p, s, RARROW));
  else result = false;
  return result;
}

bool speculate_unop(Parser* p, Scanner* s)
{
  bool result = true;
  result = false;
  return result;
}


































/* -------------------------------------------------------------------------- */
