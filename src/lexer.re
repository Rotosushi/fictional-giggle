
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "lexer.h"
#include "parser.h"
#include "error.h"


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

void yysetbuffer(Scanner* scnr, char* text, int len)
{
    if (len > SCANNER_BUF_SZ)
        error_abort("cannot buffer input text, aborting", __FILE__, __LINE__);

    memset (scnr->buf, 0, SCANNER_BUF_SZ);
    memcpy (scnr->buf, text, len);

    scnr->cursor = scnr->marker = scnr->mrkctx = scnr->token = scnr->buf;
    scnr->end = scnr->buf + len + 1;
}

int yyfill(Scanner* scanner)
{
  /*
    buf->[c0, c1, c2, .., ctoken-1, ctoken, .., cend-1, cend, .., cbuf+SCANNER_BUF_SZ]

    ->token points to the character just past the last token processed.
    (in other words, at the beginning of the next token to process.)
    the difference between the start of the array (->buf) and
    the end of the last token (->token) is the space currently
    being occupied by processed characters in the array.
    (its the 'processed' text.)

    available is the slots in the buffer left which can be fill
    with more characters. it is the total slots in the array available
    minus the slots taken up by the unprocessed slots sitting
    between the end of the last token (->token) and the end of
    the buffered input (->end) (the 'unprocessed' characters as it were)

    so the goal of this function is to shift the unprocessed
    input left in the buffer to the bottom of the array, and
    fill the available space with more input from the input FILE.
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

StrLoc* yylloc(Scanner* scanner)
{
    return (StrLoc*)(&(scanner->yylloc));
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



/*
typedef struct Scanner {
	char  buf[SCANNER_BUF_SZ + 1];
    char* end;
    char* cursor;
	char* marker;
    char* token;
	unsigned int yyaccept;
	int state;
	FILE* yyin;
	char  yych;
	bool  is_stdin;
	struct {
		int first_line;
		int first_column;
		int last_line;
		int last_column;
	} yylloc;
} Scanner;

    needs of re2c:

    yych holds current input character.
    YYCTYPE is the type of yych

    YYCURSOR is of type YYCTYPE*, and is used to search for matches

    YYLIMIT is of type YYCTYPE*, and is used to delimit the end of the buffer.

    YYMARKER is of type YYCTYPE* and is used to backup the cursor after a successful
                match, in certain circumstances.


#define  YYPEEK ()         *YYCURSOR
#define  YYSKIP ()         ++YYCURSOR
#define  YYBACKUP ()       YYMARKER = YYCURSOR
#define  YYBACKUPCTX ()    YYCTXMARKER = YYCURSOR
#define  YYRESTORE ()      YYCURSOR = YYMARKER
#define  YYRESTORECTX ()   YYCURSOR = YYCTXMARKER
#define  YYRESTORERAG (t)  YYCURSOR = t
#define  YYLESSTHAN (n)    YYLIMIT - YYCURSOR < n
#define  YYSTAGP (t)       t = YYCURSOR
#define  YYSTAGPD (t)      t = YYCURSOR - 1
#define  YYSTAGN (t)       t = NULL


*/

/*!re2c
    alpha      = [a-zA-Z];
    digit      = [0-9];
    alnum      = [alpha|digit];
    identifier = [-a-zA-Z_][-a-zA-Z0-9_]*;
*/

#define YYPEEK()       *scanner->cursor
#define YYSKIP()       ++(scanner->cursor)
#define YYBACKUP()     scanner->marker = scanner->cursor
#define YYRESTORE()    scanner->cursor = scanner->marker
#define YYBACKUPCTX()  scanner->mrkctx = scanner->cursor
#define YYRESTORECTX() scanner->cursor = scanner->mrkctx
#define YYLESSTHAN(n)  (scanner->end - scanner->cursor) < n

int yylex(Parser* parser, Scanner* scanner)
{

  scanner->token = scanner->cursor;
loop:
  /*!re2c
      re2c:define:YYCTYPE  = char;
      re2c:sentinel = -1;
      re2c:yyfill:enable = 0;

      [ \t\n\r]  { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); goto loop; }
      *          { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ERR; }
      "nil"      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL; }
      "Nil"      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL_TYPE; }
      ":"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLON; }
      ":="       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLONEQUALS; }
      "\\"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BSLASH; }
      "->"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BINOP; }
      "=>"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return REQARROW; }
      ";"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return SEMICOLON; }
      "("        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LPAREN; }
      ")"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RPAREN; }
      "{"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LBRACE; }
      "}"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RBRACE; }
      identifier { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ID; }
  */
}








































/* ---------------------------------------------------------------- */
