#pragma once
#include <memory>
using std::unique_ptr;
using std::make_unique;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::make_pair;

#include "Ast.hh"

/* Associativity */
enum class Assoc {
  Left,
  Right,
  None,
};

typedef unique_ptr<Ast> (*binop_eliminator)(const Ast* const, const Ast* const);



/*
  a binary operation is a language abstraction
  partially built atop the procedure abstraction,
  and partially built atop procedures we write ourselves.

  since we allow both primitive operations and composite
  operations to be overloaded, in both cases we need to
  save a mapping of types to elimination procedures.
  the difficulty is in that the elimination procedure
  of a primitive operation is a function in c++, and
  the elimination procedure of a composite operation
  is defined in terms of an Ast defined by the programmer.

  and, an additional complication is in the case of
  a user overloaded primitive binop; in this case we
  need to have logic which seaches both the primitive
  mappings and the composite mappings.


  type -> type -> type

*/

class Binop {
public:
  string optxt;
  int precedence;
  Assoc associativity;
  /*
    together the primitive and composite eliminators
    are considered the binops overload set.

    also, both members are pointers in order to reduce
    the size of the Binop object. as this object is
    used within the parser for the above peices of
    information, and we don't want to construct a
    heavy ProcedureDefinition or a large array of
    pointers every time we parse the binop. so
    instead the object is probably around the size
    of four ints. i.e. close in size to a Location struct.
  */
  vector<pair<unique_ptr<TypeNode>, binop_eliminator>> primitive_eliminators;
  ProcedureDefinition composite_eliminator;

  Binop() = default;
  ~Binop() = default;
  Binop(string txt, int prec, Assoc assoc, vector<pair<unique_ptr<TypeNode>, binop_eliminator>> prim_elims, vector<ProcedureLiteral> comp_elims)
    : optxt(txt), precedence(prec), associativity(assoc), composite_eliminators(comp_elims)
  {
    for (auto&& prim_pair : prim_elims)
    {
      primitive_eliminators.push_back(make_pair(make_unique(get<0>(prim_pair)->clone()), get<1>(prim_pair)));
    }
  }

  Binop(const Binop& rhs)
    : optxt(rhs.optxt), precedence(rhs.precedence), associativity(rhs.associativity),
  {
    for (auto&& prim_pair : rhs.primitive_eliminators)
    {
      primitive_eliminators.push_back(make_pair(make_unique(get<0>(prim_pair)->clone()), get<1>(prim_pair)));
    }
  }

  Binop& operator=(const Binop& rhs)
  {
    optxt = rhs.optxt;
    precedence = rhs.precedence;
    associativity = rhs.associativity;
    primitive_eliminators.clear();
    for (auto&& prim_pair : rhs.primitive_eliminators)
    {
      primitive_eliminators.push_back(make_pair(make_unique(get<0>(prim_pair)->clone()), get<1>(prim_pair)));
    }
    composite_eliminators = rhs.composite_eliminators;
  }
};
