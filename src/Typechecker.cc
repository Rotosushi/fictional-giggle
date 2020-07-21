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


bool Typechecker::equivalent(const Type * const t1, const Type * const t2)
{
  if (!t1 || !t2)
    throw "cannot type compare against nullptr\n";

  MonoType* mt1p = dynamic_cast<MonoType*> (t1.get());
  MonoType* mt2p = dynamic_cast<MonoType*> (t2.get());

  // if either cast fails, we return false
  // because any MonoType does not equal
  // a ProcType.
  if (!mt1p || !mt2p) {
    return false;
  }
  else if (mt1p && mt2p) {
    // we compare MonoTypes by
    // comparing their tags.
    return mt1p.type == mt2p.type;
  } else {
    // if neither dynamic_cast above succeeded
    // then neither are MonoTypes, and the only
    // other kind of type is ProcType, so we
    // assume that either both are ProcTypes,
    // or the judgement is false.
    // and to compare ProcTypes, we compare
    // their branches.
    ProcType* pt1p = dynamic_cast<ProcType*> (t1.get());
    ProcType* pt2p = dynamic_cast<ProcType*> (t2.get());

    if (!pt1p || !pt2p)
      throw "cannot compare invalid Type ptrs\n";

    return TypesEqual(pt1p->lhs, pt2p->lhs)
        && TypesEqual(pt1p->rhs, pt2p->rhs);
  }
}

bool is_polymorphic(const Type* const t)
{
  if (!t)
    throw "cannot check for polymorphism of nullptr\n";

  auto monotype = dynamic_cast<MonoType*>(t);

  if (monotype) {
    if (monotype->type == AtomicType::Poly) {
      return true;
    } else {
      return false;
    }
  } else {
    auto proctype = dynamic_cast<ProcType*>(t);

    if (proctype) {
      return is_polymorphic(proctype.lhs.get())
          || is_polymorphic(proctype.rhs.get());
    } else {
      throw "invalid type\n";
    }
  }
}

optional<unique_ptr<Ast>> HasInstance(const ProcSet* const ps, const Type* const t, SymbolTable* env)
{
  /*
    either returns the procedure associated with the passed argument type,
    returns None if there is no procedure associated with the argument type
    or if the procedure is polymorphic, we construct a monomorphic version
    and then typecheck that, we insert one copy of the monomorphic procedure
    into the ProcSet to cache the result,
    and return another copy as the result instance.
  */
  if (ps.polymorphic) {
    /*
    first, look in the set of procedures
    in case we already made a valid
    monomorphic instance, or if the
    programmer provided an explicit
    overload.
    */
    auto inst = find_if (ps.set.begin(), ps.set.end(),
                        [](const Procedure& elem) -> bool {
                          Type* elem_type = elem.arg_type->type.get();
                          return equivalent(t, elem_type);
                        });

    if (inst != ps.set.end()) {
      // if we get here, then inst points to the
      // valid monomorph, so we return a copy.
      return optional<unique_ptr<Ast>>(move(unique_ptr<Ast>(new Procedure(*inst))));
    } else {
      // we didn't find a valid instance, so
      // we can try to construct the requested
      // instance.
      auto proc = unique_ptr<Ast>(new Procedure(*inst));

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
  } else {

  }
}

unique_ptr<Type> Typechecker::getype(const Empty* const e, SymbolTable* env)
{
  /*
  any empty term has no type. as it is not anything
  we signal this with AtomicType::Undef,
  but this may warrant a None type?
  i don't really know just yet.
  (it's definitely not AtomicType::Nil,
   as that is the type of the nil literal.)
  */
  return unique_ptr<Type>(new MonoType(AtomicType::Undef));
}

unique_ptr<Type> Typechecker::getype(const Variable* const v, SymbolTable* env)
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

unique_ptr<Type> Typechecker::getype(const Call* const c, SymbolTable* env)
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

unique_ptr<Type> Typechecker::getype(const Bind* const b, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const Binop* const b, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const Unop* const u, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const Cond* const c, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const Entity* const e, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const ProcSet* const p, SymbolTable* env);
unique_ptr<Type> Typechecker::getype(const MonoType* const m, SymbolTable* env);
unqiue_ptr<Type> Typechecker::getype(const ProcType* const p, SymbolTable* env);
