#include <stdio.h>
#include <stdlib.h>
#include "error.h"

/*
  https://www.clear.rice.edu/comp506/Labs/IBM-Note.pdf

  looking at replicating the behavior to acheive a
  error printer that can point to where in the line of
  input text the error happened.

  this means factoring yyerror out of parser.y
  and into this file first and foremost.
*/

void error_abort(char *s, const char* file, int line)
{
  fprintf(stderr, "f: %s \n l: %d \n %s\n", file, line, s);
  exit(1);
}
