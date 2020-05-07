
#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include <stdbool.h>

#include "symboltable.h"
#include "ast.h"

Ast* type_of(Ast* term, symboltable* env);
bool typesEqual(Ast* t1, Ast* t2, symboltable* env);

#endif
