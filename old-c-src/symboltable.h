#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H
#include "ast.h"

typedef struct symbol {
  char *id;
  Ast  *term;
  struct symbol* next;
} symbol;

typedef struct Symboltable {
  symbol* symbols;
  symbol* end;
} Symboltable;

Ast*    lookup(char* name, Symboltable* symtable);
void    bind  (char* name, Ast* term, Symboltable* symtable);
void  unbind  (char* name, Symboltable* symtable);
Symboltable* createSymboltable();
void destroySymboltable(Symboltable* symtable);
#endif
