#pragma once

#include <string>
using std::string;
#include <set>
using std::set;
#include <memory>
using std::unique_ptr;

#include "BinopTable.hh"
#include "UnopTable.hh"

void init_binops(BinopTable& binops);
void init_unops(UnopTable& unops);


/*
  composite operations are applied like
  a procedure.

  composite operators are defined by the programmer
  and we use regular name lookup procedures to
  get the correct body term.

  primitive operations act directly on two
  Ast operands. and as such, each particular
  implementation needs to be defined.

  what if the programmer overrides a primitive
  operation?

  well, in both of the cases we store programmer definitions
  as Procedures. which means that binops inherit overloading
  and theoretically polymorphism via each Procedure,
  and HasInstance as the procedure to extract individual
  executable procedure literals.

*/
