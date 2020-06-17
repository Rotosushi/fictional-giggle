
#include <stdlib.h>
#include <string.h>

#include "precedencetable.h"


PrecedenceTable* CreatePrecedenceTable() {
  PrecedenceTable* table = (PrecedenceTable*)malloc(sizeof(PrecedenceTable));
  table->root = NULL;
  table->len  = 0;
  return table;
}

void DestroyPrecedenceTable(PrecedenceTable* table)
{
  OpPrec* cur = table->root, *prv = NULL;

  while (cur) {
    prv = cur;
    cur = cur->next;
    free(prv->op);
    free(prv);
  }

  table->root = NULL;
  table->len  = 0;
}

void InsertOpPrec(PrecedenceTable* table, char* op, int prec, ASSOC associativity)
{
  OpPrec* cur = table->root, *prv = NULL;
  int cmp     = -1;

  /*
    cmp < 0  -> the first different character was larger in s1 than s2
    cmp == 0 -> the two strings are equal
    cmp > 0  -> the first differenct character was larger in s2 than s1

    we store in sorted order, so we want to traverse the list
    until we find the first op that is greater than the op being
    inserted, and insert just before it.

    we also need to be aware of the two basic list cases,
    needing to insert into the empty list, and inserting
    at the end of the list.

  */
  if (cur == NULL) {
    table->root        = (OpPrec*)malloc(sizeof(OpPrec));
    table->root->op    = strdup(op);
    table->root->prec  = prec;
    table->root->assoc = associativity;
    table->len        += 1;
  } else {
    // iterate through the list until we find either
    // A: an element which is "larger" than the passed operator. (in the strcmp sense)
    // B: we find the end of the list.
    while (cur != NULL && (cmp = strcmp(cur->op, op)) > 0) {
      prv = cur;
      cur = cur->next;
    }

    if (cur) {
      OpPrec* opPrec = (OpPrec*)malloc(sizeof(OpPrec));
      opPrec->op     = strdup(op);
      opPrec->prec   = prec;
      opPrec->assoc  = associativity;
      opPrec->next   = cur;
      prv->next      = opPrec;
      table->len     += 1;
    } else {
      if (cmp == 0) {
        return; // the op already has a prec.
      } else {
        OpPrec* opPrec = (OpPrec*)malloc(sizeof(OpPrec));
        opPrec->op     = strdup(op);
        opPrec->prec   = prec;
        opPrec->assoc  = associativity;
        prv->next      = opPrec;
        table->len     += 1;
      }
    }
  }
}

void DeleteOpPrec(PrecedenceTable* table, char* op)
{
  return;
}

int  LookupOpPrec(PrecedenceTable* table, char* op)
{
  OpPrec *cur = table->root;
  int cmp = -1;

  /*
    polymorphic containers are necessary inclusion
    in pink. it is so much easier to say
    "make a map of strings to ints" in c++ than c.
  */
  while (cur != NULL && (cmp = strcmp(cur->op, op)) != 0) {
    cur = cur->next;
  }

  if (cur) {
    return cur->prec;
  } else {
    return -1;
  }

}

ASSOC LookupOpAssoc(PrecedenceTable* table, char* op)
{
  OpPrec *cur = table->root;
  int cmp = -1;

  while (cur != NULL && (cmp = strcmp(cur->op, op)) != 0) {
    cur = cur->next;
  }

  if (cur) {
    return cur->assoc;
  } else {
    return A_UNDEF;
  }

}
