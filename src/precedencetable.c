
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
  if (table->root == NULL) {
    table->root        = (OpPrec*)calloc(1, sizeof(OpPrec));
    table->root->op    = strdup(op);
    table->root->prec  = prec;
    table->root->assoc = associativity;
    table->len        += 1;
  } else {
    OpPrec* new       = (OpPrec*)calloc(1, sizeof(OpPrec));
    new->op           = op;
    new->prec         = prec;
    new->assoc        = associativity;
    new->next         = table->root->next;
    table->root->next = new;
    table->len       += 1;
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
