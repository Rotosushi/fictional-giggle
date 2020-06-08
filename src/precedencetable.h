#ifndef PRECEDENCETABLE_H
#define PRECEDENCETABLE_H

typedef enum ASSOC {
  A_UNDEF,
  A_LEFT,
  A_RIGHT,
  A_NONE,
} ASSOC;

typedef struct OpPrec {
  char* op;
  int   prec;
  ASSOC assoc;
  struct OpPrec* next;
} OpPrec;

typedef struct PrecedenceTable {
  OpPrec* root;
  int     len;
} PrecedenceTable;

PrecedenceTable* CreatePrecedenceTable();
void  DestroyPrecedenceTable(PrecedenceTable* table);
void  InsertOpPrec(PrecedenceTable* table, char* op, int prec, ASSOC assoc);
void  DeleteOpPrec(PrecedenceTable* table, char* op);
int   LookupOpPrec(PrecedenceTable* table, char* op);
ASSOC LookupOpAssoc(PrecedenceTable* table, char* op);

#endif
