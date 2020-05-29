#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H
#include "ast.h"

typedef struct symbol {
  char *id;
  Ast  *term;
  struct symbol* next;
} symbol;

typedef struct symboltable {
  symbol* symbols;
  symbol* end;
} symboltable;

Ast*    lookup(char* name, symboltable* symtable);
void    bind  (char* name, Ast* term, symboltable* symtable);
void  unbind  (char* name, symboltable* symtable);
symboltable* createSymboltable();
void destroySymboltable(symboltable* symtable);
#endif
