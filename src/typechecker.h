
#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <stdbool.h>

#include "symboltable.h"
#include "ast.h"

Ast* type_of(Ast* term, Symboltable* env);
bool typesEqual(Ast* t1, Ast* t2, Symboltable* env);
bool is_polymorphic(Ast* t1);
Ast* HasInstance(Ast* proc, Ast* type, Symboltable* env);

#endif
