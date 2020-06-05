#ifndef PRECEDENCETABLE_H
#define PRECEDENCETABLE_H

typedef struct OpPrec {
  char* op;
  int   prec;
  struct OpPrec* next;
} OpPrec;

typedef struct PrecedenceTable {
  OpPrec* root;
  int     len;
} PrecedenceTable;

PrecedenceTable* CreatePrecedenceTable();
void DestroyPrecedenceTable(PrecedenceTable* table)
void InsertOpPrec(PrecedenceTable* table, char* op, int prec);
void DeleteOpPrec(PrecedenceTable* table, char* op);
int  LookupOpPrec(PrecedenceTable* table, char* op);

#endif
