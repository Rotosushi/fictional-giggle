#pragma once

#include <string>
using std::string;
#include <set>
using std::set;
#include <memory>
using std::unique_ptr;

#include "BinopTable.hh"

void init_binops(BinopTable& binops);
void init_unops(set<string>& unops);


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
*/
