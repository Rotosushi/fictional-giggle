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
class Eliminator
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
  depending.
  */
  primitive_eliminator eliminator;

public:
  Eliminator(primitive_eliminator elim)
    : eliminator(elim) {}

  shared_ptr<Ast> operator()(shared_ptr<Ast> lhs, shared_ptr<Ast> rhs);
};


class BinopEliminatorSet : public Ast
{
  list<tuple<shared_ptr<Type>, shared_ptr<Type>, primitive_eliminator>> primitive_eliminators;
  // Lambda composite_eliminators
public:
void RegisterPrimitiveEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype, binop_eliminator elim);
optional<Eliminator> HasEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype);
};
