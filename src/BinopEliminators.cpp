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
using std::make_pair;

#include "Ast.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "BinopEliminators.hpp"


shared_ptr<Ast> BinopEliminator::operator()(shared_ptr<Ast> lhs, shared_ptr<Ast> rhs)
{
  /*
    for now we always directly call the
    held eliminator, as the lookup
    mechanisms are already going to
    disambiguate between which eliminator
    to call. and further, we only ever
    have the ability to construct the
    eliminator holding a single primitive eliminator.
    (in the future case of having to select between
      which alternative to apply, the language lambda
      or the primitive function, this code is going
      to become an
      [if primitive then apply primitive else apply composite]
       block.)
  */
  return this->eliminator(lhs.get(), rhs.get());
}

shared_ptr<Type> BinopEliminator::result_type()
{
  return result_type;
}

void BinopEliminatorSet::RegisterPrimitiveEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype, shared_ptr<Type> restype, primitive_binop_eliminator prim_elim)
{
  primitive_eliminators.push_front(make_tuple(ltype, rtype, restype, prim_elim));
}

optional<BinopEliminator> BinopEliminatorSet::HasEliminator(shared_ptr<Type> ltype, shared_ptr<Type> rtype)
{
  auto eliminator_arguments_match =
    [](tuple<shared_ptr<Type>, shared_ptr<Type>, shared_ptr<Type>, primitive_binop_eliminator> eliminator, shared_ptr<Type> ltype, shared_ptr<Type> rtype) -> bool
  {
    bool result;
    if (TypesEquivalent((get<0>(eliminator)).get(), ltype.get()))
    {
      if (TypesEquivalent((get<1>(eliminator)).get(), rtype.get()))
      {
        result = true;
      }
      else
      {
        result = false;
      }
    }
    else
    {
      resut = false;
    }
    return result;
  }

  for (auto&& elim_tuple : primitive_eliminators)
  {
    if (eliminator_arguments_match(elim_tuple, ltype, rtype))
    {
      return make_optional(BinopEliminator(get<3>(elim_tuple), get<2>(elim_tuple));
    }
  }
  return optional<BinopEliminator>();
}

void BinopSet::RegisterBinop(const string& op, shared_ptr<BinopEliminatorSet> set)
{
  set.push_back(make_pair(op, set));
}

optional<shared_ptr<BinopEliminatorSet>> BinopSet::FindBinop(const string& op)
{
  for (auto&& binop : set)
  {
    if (get<string>(binop) == op)
    {
      return make_optional(get<shared_ptr<BinopEliminatorSet>>(binop));
    }
  }

  return optional<shared_ptr<BinopEliminatorSet>>();
}
