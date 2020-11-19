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
#include <utility>
using std::pair;

class Ast;
class Type;

/*
  function pointers seem usefull,
  but also, what does a function
  pointer even mean for a polymorphic
  function? i suppose a pointer to
  the dispatch procedure, but, really
  with the way dynamic dispatch works
  in the language how could we type the
  function pointer as anything other
  than poly -> poly? which implies that
  the programmer can apply a function
  pointer using any argument list that is
  available to a caller of the procedure.
  what is the true difference between two
  function pointers? which particular function
  address is stored within itself.

*/
typedef shared_ptr<Ast> (*primitive_binop_eliminator)(shared_ptr<Ast>, shared_ptr<Ast>);

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
  primitive_binop_eliminator primitive_eliminator;
  // Lambda                  composite_eliminator;
  shared_ptr<Type>           result_type;

public:
  BinopEliminator(primitive_binop_eliminator elim, shared_ptr<Type> result_type)
    : primitive_eliminator(elim), result_type(result_type) {}

  shared_ptr<Ast> operator()(shared_ptr<Ast> lhs, shared_ptr<Ast> rhs);
  shared_ptr<Type> GetResultType();
};


class BinopEliminatorSet
{
  list<tuple<shared_ptr<Type>, shared_ptr<Type>, shared_ptr<Type>, primitive_binop_eliminator>> primitive_eliminators;
  // Lambda composite_eliminators
public:
void RegisterPrimitiveEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype, shared_ptr<Type> restype, primitive_binop_eliminator elim);
// void RegisterCompositeEliminator(Lambda composite_eliminators);
optional<BinopEliminator> HasEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype);
};

class BinopSet
{
  list<pair<string, shared_ptr<BinopEliminatorSet>>> set;

public:
  void RegisterBinop(const string& op, shared_ptr<BinopEliminatorSet> set);
  optional<shared_ptr<BinopEliminatorSet>> FindBinop(const string& op);
};
