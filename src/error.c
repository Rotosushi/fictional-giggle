#include <stdio.h>
#include <stdlib.h>
#include "error.h"

void error_abort(char* s)
{
  fprintf(stderr, "%s\n", s);
  exit(1);
}
