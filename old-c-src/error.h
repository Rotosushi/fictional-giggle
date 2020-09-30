#ifndef ERROR_H
#define ERROR_H

#include "ast.h"

void error_abort(char *s, const char* file, int line);
void error_print(char *givenText, Ast* err, char* fmt, ...);

#endif
