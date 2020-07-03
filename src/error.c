#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "ast.h"
#include "error.h"

/*
  https://www.clear.rice.edu/comp506/Labs/IBM-Note.pdf

  looking at replicating the behavior to acheive a
  error printer that can point to where in the line of
  input text the error happened.
*/

void error_abort(char *s, const char* file, int line)
{
  fprintf(stderr, "[f:%s l:%d]%s\n", file, line, s);
  exit(1);
}

void error_print(char* givenText, Ast* err, char* fmt, ...)
{
  va_list args;
  va_start (args, fmt);

  int len = strlen(givenText);
  printf("%s", givenText);

  if (err->hasloc) {
    for(int i = 0; i < len; i++) {
      if (i < err->lloc.first_column) {
        printf("~");
      }
      else if (i < err->lloc.last_column) {
        printf("^");
      }
      else {
        printf("~");
      }
    }
    printf("\n");
  }

  printf(fmt, args);

  va_end(args);
}
