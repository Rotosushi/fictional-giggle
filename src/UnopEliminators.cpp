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
using std::make_optional;

#include "Ast.hpp"
#include "UnopEliminators.hpp"


shared_ptr<Ast> UnopEliminator::operator()(shared_ptr<Ast> rhs)
{
  return primitive_eliminator(rhs);
}

shared_ptr<Type> UnopEliminator::GetResultType()
{
  return result_type;
}

void UnopEliminatorSet::RegisterPrimitiveEliminator(shared_ptr<Type> argtype, shared_ptr<Type> restype, primitive_unop_eliminator elim)
{
  primitive_eliminators.push_back(make_tuple(argtype, restype, elim));
}

optional<UnopEliminator> UnopEliminatorSet::HasEliminator(shared_ptr<Type> rtype)
{
  /*
    we somehow need to catch the "&" and "*"
    ref types specially? so we can extract the
    primitive procedure which allows us to
    wrap or unwrap terms in our language with references.

  */
  auto eliminator_arguments_match =
    [](tuple<shared_ptr<Type>, shared_ptr<Type>, primitive_unop_eliminator> eliminator, shared_ptr<Type> rtype)
  {
    bool result;
    if (TypesEquivalent(get<0>(eliminator), rtype))
    {
      result = true;
    }
    // i think this solves the problem, when this algorithm
    // encounters the formal arg type RefPolyType of "&"
    // and the formal arg type PolyType of "*"
    // it will match against any passed type.
    else if (get<0>(eliminator)->is_polymorphic())
    {
      result = true;
    }
    else
    {
      result = false;
    }
    return result;
  };

  for (auto&& elim_tuple : primitive_eliminators)
  {
    if (eliminator_arguments_match(elim_tuple, rtype))
    {
      return make_optional(UnopEliminator(get<primitive_unop_eliminator>(elim_tuple), get<1>(elim_tuple)));
    }
  }
  return optional<UnopEliminator>();
}

void UnopSet::RegisterUnop(const string& op, shared_ptr<UnopEliminatorSet> unop)
{
  set.push_back(make_pair(op, unop));
}

optional<shared_ptr<UnopEliminatorSet>> UnopSet::FindUnop(const string& op)
{
  for (auto&& unop : set)
  {
    if (get<string>(unop) == op)
    {
      return make_optional(get<shared_ptr<UnopEliminatorSet>>(unop));
    }
  }
  return optional<shared_ptr<UnopEliminatorSet>>();
}
