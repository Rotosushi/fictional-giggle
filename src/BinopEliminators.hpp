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
//#include "Entity.hpp"

typedef shared_ptr<Ast> (*primitive_eliminator)(const Ast* const, const Ast* const);

/*
  eventually, this class will encapsulate
  the code which passes the arguments into
  either the primitive or composite
  eliminator held within. thus, using the binop
  eliminator in outer code simply becomes
  did i receive a matching eliminator?
  if yes, call it and return the result.
  if no, report the error.
*/
class BinopEliminator
{
  /*
  eventually, like the Judgements
  bool is_primitive;
  union U {
    primitive_eliminator pelim;
    Lambda celim;
  } u;

  then, the operator() code can
  apply the passed arguments to either
  depending. presumably, this eliminator
  will be the dispatched procedure. as in,
  the assembly subroutine that would be called if this
  was a translation into assembly, corresponding to
  one of: a direct application of a primitive operation,
  a call to the dispatch procedure of a polymorph
  procedure, or i think lastly, a direct call to
  a monomorph procedure, either a monomorphic procedure
  or an optimized call directly of an instance of a
  polymorph procedure.
  */
  primitive_eliminator eliminator;

public:
  BinopEliminator(primitive_eliminator elim)
    : eliminator(elim) {}

  shared_ptr<Ast> operator()(shared_ptr<Ast> lhs, shared_ptr<Ast> rhs);
};


class BinopEliminatorSet
{
  list<tuple<shared_ptr<Type>, shared_ptr<Type>, primitive_eliminator>> primitive_eliminators;
  // Lambda composite_eliminators
public:
void RegisterPrimitiveEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype, binop_eliminator elim);
// void RegisterCompositeEliminator(Lambda composite_eliminators);
optional<BinopEliminator> HasEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype);
};

class BinopSet
{
  list<pair<string, shared_ptr<BinopEliminatorSet>>> set;

public:
  void RegisterBinopEliminatorSet(const string& op, shared_ptr<BinopEliminatorSet> set);
  optional<shared_ptr<BinopEliminatorSet>> FindEliminatorSet(const string& op);
};
