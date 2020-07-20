#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
using std::optional;
using std::pair;
using std::get;


#include "Ast.hh"
#include "SymbolTable.hh"
#include "OperatorTable.hh"

#include "Typechecker.hh"


bool Typechecker::TypesEqual(const Type* t1, const Type* t2)
{
  
}
