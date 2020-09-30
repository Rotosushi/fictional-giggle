#pragma once
#include <memory>
using std::unique_ptr;
#include <vector>
using std::vector;

#include "Ast.hh"

typedef unique_ptr<Ast> (*unop_eliminator)(const Ast* const);

class Unop {
public:
  vector<pair<unique_ptr<TypeNode>, unop_eliminator>> primitive_eliminators;
  vector<ProcedureLiteral> composite_eliminators;
};
