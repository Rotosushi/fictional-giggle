#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <list>
using std::list;
#include <pair>
using std::pair;
using std::make_pair;
#include <optional>
using std::optional;

#include "Ast.hpp"
#include "Entity.hpp"

typedef shared_ptr<Ast> (*primitive_unop_eliminator)(shared_ptr<Ast> rhs);

class UnopEliminator
{
  primitive_unop_eliminator prim_eliminator;
  
};
