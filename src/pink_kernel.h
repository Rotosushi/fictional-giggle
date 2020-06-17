#ifndef PINK_KERNEL_H
#define PINK_KERNEL_H
#include "ast.h"
#include "symboltable.h"
#include "precedencetable.h"
#include "stringset.h"

void InitEnvWithKernel (Symboltable* env);
void InitializePrecedenceTable(PrecedenceTable* pTable);
void InitializeBinops(StringSet* binopSet);
void InitializeUnops(StringSet* unopSet);

#endif
