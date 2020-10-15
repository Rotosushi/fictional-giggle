#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <list>
using std::list;
#include <tuple>
using std::tuple;
using std::make_tuple;
#include <optional>
using std::optional;

#include "Ast.hpp"


typedef shared_ptr<Ast> (*primitive_unop_eliminator)(shared_ptr<Ast> rhs);

/*
  as with the binop eliminator object,
  this is essentially a command pattern
  which holds the function pointer,
  or an evaluatable Lambda, corresponding
  to some actual argument type.
  when the program attempts to find the
  correct version of the procedure, that
  corresponds to dispatching over the
  runtime types in the assembly version,
  and when we subsequently call the
  eliminators operator() procedure, that
  corresponds to applying the procedure
  what was dispatched too in the assembly.
*/
class UnopEliminator
{
  primitive_unop_eliminator primitive_eliminator;
  // Lambda                 composite_eliminator;
  shared_ptr<Type>          result_type;

public:
    UnopEliminator(primitive_unop_eliminator elim)
      : primitive_eliminator(elim) {}

    shared_ptr<Ast> operator()(shared_ptr<Ast> rhs);
    shared_ptr<Type> result_type();
};

class UnopEliminatorSet
{
  list<tuple<shared_ptr<Type>, shared_ptr<Type>, primitive_unop_eliminator>> primitive_eliminators;
  // PolyLambda composite_eliminators;
public:
  void RegisterPrimitiveEliminator(shared_ptr<Type> argtype, shared_ptr<Type> restype, primitive_unop_eliminator elim);
  optional<UnopEliminator> HasEliminator(shared_ptr<Type> argtype);
};

class UnopSet
{
  list<pair<string, shared_ptr<UnopEliminatorSet>>> set;

public:
  void RegisterUnop(const string& op, shared_ptr<UnopEliminatorSet> set);
  optional<shared_ptr<UnopEliminatorSet>> FindUnop(const string& op);
};
