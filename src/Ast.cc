
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;

using std::pair;
using std::get;
#include <optional>
using std::optional;


#include "Location.hh"
#include "Ast.hh"
#include "SymbolTable.hh"
#include "Typechecker.hh"


optional<ProcedureLiteral> ProcedureDefinition::HasInstance(const TypeNode* const target_type, SymbolTable* env)
{
  /*
    either returns the procedure associated with the passed argument type, or
    returns failure if there is no procedure associated with the argument type.

    If this procedure is polymorphic, we first construct a monomorphic version
    and then typecheck that, if it passes, we insert one copy of the
    monomorphic procedure into the ProcSet to cache the result,
    and return another copy as the result instance; if the typecheck
    fails, we return the empty option.

    If the procedure is monomorphic, then we can typecheck against
    the .def directly (because it has some monomorphic type), and
    if that fails we can check the .set for a possible other instance.
    (however, given the current grammar, we cannot actually define
     overloads yet. not until we add a formal function definition term.)
  */

  // helper function for using find_if against the Procedure sets
  auto arg_type_equals_target_type = [target_type](const ProcedureLiteral& elem) -> bool
  {
    TypeNode* elem_type = dynamic_cast<TypeNode*>(elem.arg_type.get());
    // recall that equivalent returns a judgement,
    // so we convert to bool to simply return the
    // success or failure here.
    return Typechecker::equivalent(target_type, elem_type);
  };
  // I factored out looking throughout the set,
  // given that it needs to happen first in either
  // case anyways.
  auto instance = find_if(set.begin(), set.end(), arg_type_equals_target_type);

  if (instance != set.end())
  {
    return optional<ProcedureLiteral>(*instance);
  }
  //!instance

  if (poly)
  {
    /*
      if the procedure definition is polymorphic, then we
      cannot typecheck against the definitions argument type.
      as the rest of the typechecker simply assumes
      polymorphic means well-formed. (essentially giving the
      benefit of the doubt to the programmer that some
      polymorphic term is well formed.)
      so, we instead typecheck against a new procedure
      whose body is identical to the definitions,
      but whose type is replaced with the target_type.
      essentially 'binding' the polymophic type variable
      to some monomorphic type in this instance of the
      procedure. if the typechecking succeeds, then
      we can return an instance after inserting the
      newly well-formed instance into the set.
      otherwise we return the none option.
      this destroys the TypeError present within
      the returned judgement.
    */
    env->bind(def.arg_id, make_unique<EntityNode>(target_type, Location()));
    Judgement result_type = Typechecker::getype(def.body.get(), env);
    env->unbind(def.arg_id);

    if (result_type)
    {
      set.emplace(set.begin(), ProcedureLiteral(def.arg_id, make_unique<EntityNode>(target_type, Location()), def.body->clone()));
      return optional<ProcedureLiteral>(ProcedureLiteral(def.arg_id, make_unique<EntityNode>(target_type, Location()), def.body->clone()));
    }
    else
    {
      return optional<ProcedureLiteral>();
    }
  }
  else
  {
    // if this is a monomorphic procedure, then
    // the only other place we can find some instance
    // is within the definition itself.
    TypeNode* def_arg_type = dynamic_cast<TypeNode*>(def.arg_type.get());

    if (!def_arg_type)
      throw "bad procedure arg type pointer\n";

    if (Typechecker::equivalent(target_type, def_arg_type))
    {
      return optional<ProcedureLiteral>(def);
    }
    else
    {
      return optional<ProcedureLiteral>();
    }
  }
}






















/* ------------------------------------------- */
