#include <string.h>
#include <stdlib.h>


#include "symboltable.h"
#include "ast.h"
#include "error.h"


symboltable* createSymboltable()
{
  symboltable* symtable = (symboltable*)malloc(sizeof(symboltable));
  symtable->symbols = NULL;
  symtable->end = NULL;
  return symtable;
}

Ast* lookup(char* name, symboltable* symtable)
{
  if (name == NULL)
    error_abort("cannot lookup NULL name! aborting", __FILE__, __LINE__);

  if (symtable == NULL) {
    error_abort("cannot lookup name in NULL symboltable! aborting", __FILE__, __LINE__);
  }

  symbol* sym = symtable->symbols;
  while (sym != NULL)
  {
    int cmp = strcmp(name, sym->id);
    if (cmp == 0) return CopyAst(sym->term);
    sym = sym->next;
  }
  return NULL;
}

void bind(char* name, Ast* term, symboltable* symtable)
{
  if (name == NULL)
    error_abort("cannot bind NULL name! aborting", __FILE__, __LINE__);

  if (term == NULL)
    error_abort("cannot bind a name to a NULL term! aborting", __FILE__, __LINE__);

  if (symtable == NULL) {
    error_abort("cannot lookup name in NULL symboltable! aborting", __FILE__, __LINE__);
  }
  /* the binding list simply maintains the order
      in which the elements are inserted
  */
  symbol* sym = (symbol*)malloc(sizeof(symbol));
  sym->id   = strdup(name);
  sym->term = CopyAst(term);
  sym->next = NULL;

  if (symtable->symbols == NULL) {
    symtable->symbols = sym;
    symtable->end     = sym;
  }
  else {
    sym->next = symtable->symbols;
    symtable->symbols = sym;
  }
}

void unbind(char* name, symboltable* symtable)
{
  if (name == NULL)
    error_abort("cannot unbind empty name! aborting", __FILE__, __LINE__);

  if (symtable == NULL) {
    error_abort("cannot lookup name in NULL symboltable! aborting", __FILE__, __LINE__);
  }

  symbol *prv = NULL, *ths = symtable->symbols;

  int cmp = strcmp(name, ths->id);
  /* remove the head of the list */
  if (cmp == 0) {
    symtable->symbols = symtable->symbols->next;
    free(ths->id);
    DeleteAst(ths->term);
    free(ths);
  }
  else while (ths != NULL) {
    if (cmp == 0) {
      /* remove the end of the list */
      if (ths->next == NULL) {
        symtable->end = prv;
        prv->next    = NULL;
        free(ths->id);
        DeleteAst(ths->term);
        free(ths);
      }
      /* remove some middle element */
      else {
        prv->next = ths->next;
        free(ths->id);
        DeleteAst(ths->term);
        free(prv);
      }
      break;
    }
    prv = ths;
    ths = ths->next;
    if (!ths) break;
    cmp = strcmp(name, ths->id);
  }
}

void destroySymtable(symboltable* symtable)
{
  symbol *prv = NULL, *ths = symtable->symbols;
  while (ths != NULL) {
    prv = ths;
    ths = ths->next;
    free(prv->id);
    DeleteAst(prv->term);
    free(prv);
  }
  free(symtable);
  symtable = NULL;
}
