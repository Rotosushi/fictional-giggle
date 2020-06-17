
#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <stdbool.h>

#include "symboltable.h"
#include "ast.h"

Ast* type_of(Ast* term, Symboltable* env);
bool typesEqual(Ast* t1, Ast* t2, Symboltable* env);

Ast* HasInstance(ProcSet* set, Ast* type, Symboltable* env);

#endif
