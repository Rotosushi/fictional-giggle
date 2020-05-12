#include <string.h>
#include <stdlib.h>


#include "symboltable.h"
#include "ast.h"
#include "error.h"

symbol* lookup(char* name, symboltable* symtable)
{
  symbol* sym = symtable->symbols;
  while (sym != NULL)
  {
    int cmp = strcmp(name, sym->id);
    if (cmp == 0) return sym;
    sym = sym->next;
  }
  return NULL;
}

void bind(char* name, Ast* term, symboltable* symtable)
{
  symbol* sym = (symbol*)malloc(sizeof(symbol));
  sym->id   = strdup(name);
  sym->term = CopyAst(term);
  sym->next = NULL;

  if (symtable->symbols == NULL) {
    symtable->symbols = sym;
    symtable->end     = sym;
  }
  else {
    symtable->end->next = sym;
    symtable->end = sym;
  }
}

void unbind(char* name, symboltable* symtable)
{
  if (name == NULL)
    error_abort("cannot unbind empty name! aborting");


  symbol *prv = NULL, *ths = symtable->symbols;

  int cmp = strcmp(name, ths->id);
  /* remove the head of the list */
  if (cmp == 0) {
    symtable->symbols = symtable->symbols->next;
    free(ths->id);
    AstDelete(ths->term);
    free(ths);
  }
  else while (ths != NULL) {
    cmp = strcmp(name, ths->id);
    if (cmp == 0) {
      /* remove the end of the list */
      if (ths->next == NULL) {
        symtable->end = prv;
        prv->next    = NULL;
        free(ths->id);
        AstDelete(ths->term);
        free(ths);
      }
      /* remove some middle element */
      else {
        prv = ths;
        ths = ths->next;
        free(ths->id);
        AstDelete(ths->term);
        free(prv);
      }
      break;
    }
    prv = ths;
    ths = ths->next;
  }
}

void destroySymtable(symboltable* symtable)
{
  symbol *prv = NULL, *ths = symtable->symbols;
  while (ths != NULL) {
    prv = ths;
    ths = ths->next;
    free(prv->id);
    AstDelete(prv->term);
    free(prv);
  }
  symtable->end = NULL;
}
