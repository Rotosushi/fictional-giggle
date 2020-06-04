
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "lexer.h"
#include "parser.h"


Scanner* createScanner(FILE* in)
{
    Scanner* scnr = (Scanner*)malloc(sizeof(Scanner));
    memset (scnr->buf, 0, SCANNER_BUF_SZ + 1);
    scnr->yych = 0;
    scnr->end = scnr->cursor = scnr->token = scnr->marker = scnr->buf + SCANNER_BUF_SZ;
    scnr->yyaccept = 0;
    scnr->state = -1;
    if (in) {
      if (in == stdin)
        scnr->is_stdin = true;
      else
        scnr->is_stdin = false;

      scnr->yyin = in;
    }
    else {
      scnr->yyin = stdin;
      scnr->is_stdin = true;
    }
    return scnr;
}

void destroyScanner(Scanner* scanner)
{
  free(scanner);
}

int yyfill(Scanner* scanner)
{
  /*
    buf [c0, c1, c2, .., ctoken-1, ctoken, .., cend-1, cend, .., buf+SCANNER_BUF_SZ]

    ->token points to the character just past the last token processed.
    (in other words, at the beginning of the next token to process.)
    the difference between the start of the array (->buf) and
    the end of the last token (->token) is the space currently
    being occupied by processed characters in the array.
    (its the 'processed' text.)

    available is the slots available in the buffer to fill
    with more characters. it is the total slots in the array available
    minus the slots taken up by the unprocessed slots sitting
    between the end of the last token (->token) and the end of
    the buffered input (->end)

    so the goal of this function is to shift the unprocessed
    input left in the buffer to the bottom of the array, and
    fill the available space with more input from the in FILE.
  */
  size_t charsRead = 0;
  size_t processed = scanner->token - scanner->buf;
  size_t available = SCANNER_BUF_SZ - (scanner->end - scanner->token);

  if (available < 1) return -1;

  memmove(scanner->buf, scanner->token, SCANNER_BUF_SZ - processed);
  scanner->end    -= processed;
  scanner->cursor -= processed;
  scanner->token  -= processed;

  if (scanner->is_stdin)
    printf(":> ");
  charsRead = getline(&(scanner->end), &available, scanner->yyin);

  if (charsRead < 0) {
    /*
    getline returns -1 after errors and eof;
    so distinguish between real errors and
    the known case of end of input.
     */
    if (!feof(scanner->yyin)) {
      perror("yylex:getline");
      exit(1);
    }
  }
  else {
    scanner->end += charsRead;
    scanner->end[0] = 0;
  }

  return charsRead;
}

char* yytext(Scanner* scanner)
{
    return strndup(scanner->token, scanner->cursor - scanner->token);
}


void update_location(StrLoc* llocp, char* token, int length)
{
    llocp->first_line   = llocp->last_line;
    llocp->first_column = llocp->last_column;

    for (int i = 0; i < length; i++) {
        if (token[i] == '\n') {
            llocp->last_line++;
            llocp->last_column = 1;
        }
        else {
            llocp->last_column++;
        }
    }
}

#define YYGETSTATE()  scanner->state
#define YYSETSTATE(s) scanner->state = s
#define YYMARKER      scanner->marker
#define yyaccept      scanner->yyaccept
#define YYFILL()      return MORE
/*!re2c
    alpha      = [a-zA-Z];
    digit      = [0-9];
    alnum      = [alpha|digit];
    hyphenId   = [-]?[alnum_]+;
    identifier = [alpha_][hyphenId]*;

*/



int yylex(Parser* parser, Scanner* scanner, struct StrLoc* llocp)
{
  /*!getstate:re2c*/
  scanner->token = scanner->cursor;
loop:
  /*!re2c
      re2c:define:YYCTYPE  = char;
      re2c:define:YYCURSOR = scanner->cursor;
      re2c:define:YYLIMIT  = scanner->end;
      re2c:variable:yych   = scanner->yych;
      re2c:eof = 0;

      [ \t\n\r]  { update_location(llocp, scanner->token, scanner->cursor - scanner->token); goto loop; }
      *          { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return ERR; }
      $          { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return END; }
      "nil"      { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return NIL; }
      ":"        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return COLON; }
      ":="       { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return COLONEQUALS; }
      "\\"       { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return BSLASH; }
      "->"       { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return RARROW; }
      "=>"       { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return REQARROW; }
      ";"        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return SEMICOLON; }
      "("        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return LPAREN; }
      ")"        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return RPAREN; }
      "{"        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return LBRACE; }
      "}"        { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return RBRACE; }
      identifier { update_location(llocp, scanner->token, scanner->cursor - scanner->token); return ID; }
  */
}








































/* ---------------------------------------------------------------- */
