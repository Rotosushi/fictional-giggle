#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
using std::optional;
using std::pair;
using std::get;


#include "Ast.hh"
#include "SymbolTable.hh"
#include "OperatorTable.hh"

#include "Typechecker.hh"

Judgement Typechecker::equivalent(const TypeNode* t1, const TypeNode* t2)
{
  /*
    AtomicType can-compare-to AtomicType
    ProcType can-compare-to ProcType
    AtomicType cannot-compare-to ProcType
  */
  AtomicType *t1p, *t2p;
  if ((t1p = dynamic_cast<AtomicType*>(t1)) != nullptr)
  {
    if ((t2p = dynamic_cast<AtomicType*>(t2)) != nullptr))
    {
      if (t1p->type == t2p->type)
      {
        return Judgement(make_unique<AtomicType*>(*t1p));
      }
      else
      {
        return Judgement(TypeError(Location(), "atomic types not equal"));
      }
    }
    else
    {
      return Judgement(TypeError(Location(), "cannot compare atomic type to proc type"));
    }
  }
  else if ((t1p = dynamic_cast<ProcType*>(t1)) != nullptr)
  {
    if ((t2p = dynamic_cast<ProcType*>(t2)) != nullptr)
    {
      auto j1 = equivalent(t1p->lhs.get(), t2p->lhs.get());
      // if j1 is true, it contains information we need,
      // if j2 is false, it contains information we need.
      if (j1)
      {
        auto j2 = equivalent(t1p->rhs.get(), t2p->rhs.get());
        if (j2)
        {
          // we need both sides of the procedure types to be equivalent
          // to judge the procedure types equivalent.
          return Judgement(make_unique<ProcType*>(*t1p));
        }
        else
        {
          // rhs is not equal.
          return j2;
        }
      }
      else
      {
        // lhs is not equal.
        return j1;
      }
    }
    else
    {
      return Judgement(TypeError(Location(), "cannot compare proc type to non proc type"));
    }
  }
  else
    throw "unknown type node";
}


Judgement Typechecker::HasInstance(EntityNode& proc, const TypeNode* const type, SymbolTable* env)
{
  /*
    either returns the procedure associated with the passed argument type,
    returns failure if there is no procedure associated with the argument type
    or if the procedure is polymorphic, we construct a monomorphic version
    and then typecheck that, we insert one copy of the monomorphic procedure
    into the ProcSet to cache the result,
    and return another copy as the result instance.
  */
  if (proc.value_tag == EntityValueTag::Proc)
  {
    ProcSetNode& p = proc.u.procedure;
    if (p.polymorphic)
    {
      /*
      first, look in the set of procedures
      in case we already made a valid
      monomorphic instance, or if the
      programmer provided an explicit
      overload.
      */
      std::list<Procedure>::iterator inst =
        find_if (p.set.begin(), p.set.end(),
                 [type](const Procedure& elem) -> bool
                 {
                   EntityNode* typenode = elem.arg_type.get();
                   TypeNode* elem_type = typenode->type.get();
                   return bool(equivalent(type, elem_type));
                 });

      if (inst != p.set.end()) {
        // if we get here, then inst points to the
        // valid monomorph, so we return a copy.
        return Judgement(make_unique<(move(unique_ptr<Ast>(new Procedure(*inst)))));
      } else {
        // we didn't find a valid instance, so
        // we can try to construct the requested
        // instance.
        auto proc = unique_ptr<Ast>(new Procedure(proc.u.procedure.def));

        /*
          bind the argument name to it's type
          during typechecking of the body.
        */
        env.bind(proc->id, proc->arg_type);
        auto T = getype(proc->body.get(), env);
        env.unbind(proc->id);

        auto monotype = dynamic_cast<MonoType*>(T.get());

        if (monotype) {
          if (monotype->type == AtomicType::Undef) {
            return optional<unique_ptr<Ast>>();
          } else {
            // the body was typeable with type T
            // so we insert the new valid definition
            // into the set of definitions and return
            // a copy of the procedure. (wrapped in an
            // Entity to pass it through a unique_ptr<Ast>)
            ps.set.emplace_front(*proc);
            return optional<unique_ptr<Ast>>(new Entity((new ProcType(), *proc, false));
          }
        } else {
          // this is the case of some procedure
          // result type.
          auto proctype = dynamic_cast<ProcType*>(T.get());

          if (proctype) {
            ps.set.emplace_front(*proc);
          }
        }
      }
    }
    else // procedure is monomorphic.
    {

    }
  }
  else
  {
    // error: entity is not a procedure.
    return Judgement(TypeError(proc.loc, "entity is not a procedure"));
  }
}

Judgement Typechecker::getype(const EmptyNode* const e, SymbolTable* env)
{
  /*
  any empty term has no type. as it is not anything
  we signal this with AtomicType::Undef,
  but this may warrant a None type?
  i don't really know just yet.
  (it's definitely not AtomicType::Nil,
   as that is the type of the nil literal.)
  */
  return unique_ptr<Type>(new AtomicType(PrimitiveType::Undef));
}

Judgement Typechecker::getype(const VariableNode* const v, SymbolTable* env)
{
  /*
        id is-in FV(ENV)
      ------------------
    ENV |- id : type = value : type
  */
  optional<unique_ptr<Ast>> bound_term = env[v.id];

  if (bound_term) {
    return getype(bound_term->get(), env);
  } else {
    return unique_ptr<Type>(new MonoType(AtomicType::Undef));
  }
}

Judgement Typechecker::getype(const Call* const c, SymbolTable* env)
{
  /*
  ENV |- term1 : type1 -> type2, term2 : type1
  --------------------------------------------
          ENV |- term1 term2 : type2
  */
  unique_ptr<Type> typeA = getype(c.lhs.get(), env);

  auto proctype = dynamic_cast<ProcType*>(typeA.get());

  if (proctype) {
    if (is_polymorphic(typeA.get())) {
      return unique_ptr<Type>(new MonoType(AtomicType::Poly));
    } else {

      unique_ptr<Type> typeB = getype(c.rhs.get(), env);

      if (!typeB) {
        throw "rhs not typeable\n";
      }

      Entity* ps = dynamic_cast<Entity*> c.lhs.get();
      auto inst = HasInstance(ps, typeB.get(), env);

      if (inst) {
        return unique_ptr<Type>(typeA.rhs->clone());
      } else {
        // error: procedure not typeable with given type.
        return unique_ptr<Type>(new MonoType(AtomicType::Undef));
      }
    }

  }
  else {
    auto monotype = dynamic_cast<MonoType*>(typeA.get());

    if (monotype && monotype->type == AtomicType::Poly) {
      /*
        recall that the only way
        we construct a polymorphic type is if the source code
        abstains from providing a type annotation within
        the parameter list of a procedure. since this is
        some name that has polymorphic type within
        this call node, we can infer that
        we must be typechecking the body of a function.
        given that polymorphic types are used to abstract over
        expressions without first knowing the actual types,
        it makes sense to interpret this
        lhs as a function type poly -> poly, and then
        attach the type of the rhs.

        (\x=>x)             : poly -> poly
        (\x=>x x)           : poly -> poly
        (\x=>\y=>x y)       : poly -> poly
        (\x=>\y=>\z=>x y z) : poly -> poly -> poly

        and given the reasoning above, since this is some polymorphic
        procedure application, we must avoid doing serious typechecking
        of the body. we are not evaluating this application, we are
        typing it, when we go to evaluate this subsequent application
        against a monomorphic function,
        then we will use the regular function typechecking as above.
      */
      return unique_ptr<Type>(new MonoType(AtomicType::Poly));
    } else if (!monotype) {
      throw "lhs not typeable\n";
    }
    else {
      /*
        we can only apply a call if the
        lhs represents something callable.

      */
      return unique_ptr<Type>(new MonoType(AtomicType::Undef));
    }
  }

}

Judgement Typechecker::getype(const Bind* const b, SymbolTable* env);
Judgement Typechecker::getype(const Binop* const b, SymbolTable* env);
Judgement Typechecker::getype(const Unop* const u, SymbolTable* env);
Judgement Typechecker::getype(const Cond* const c, SymbolTable* env);
Judgement Typechecker::getype(const Entity* const e, SymbolTable* env);
Judgement Typechecker::getype(const ProcSet* const p, SymbolTable* env);
Judgement Typechecker::getype(const MonoType* const m, SymbolTable* env);
Judgement Typechecker::getype(const ProcType* const p, SymbolTable* env);
