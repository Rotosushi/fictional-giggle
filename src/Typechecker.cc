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

Judgement Typechecker::equivalent(const TypeNode* t1, const TypeNode* t2) static
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
        return Judgement(t1p);
      }
      else
      {
        string errstr = "atomic types t1:["
                      + t1->to_string();
                      + "] t2:["
                      + t2->to_string();
                      + "] not equal\n";
        return Judgement(Location(), errstr);
      }
    }
    else
    {
      string errstr = "cannot compare atomic type t1:["
                    + t1->to_string();
                    + "] to procedure type t2:["
                    + t2->to_string();
                    + "]\n";
      return Judgement(Location(), errstr);
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
          return Judgement(t1p);
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
      string errstr = "cannot compare proc type t1:["
                    + t1->to_string();
                    + "] to atomic type t2:["
                    + t2->to_string();
                    + "]";
      return Judgement(Location(), errstr);
    }
  }
  else
    throw "unknown type node";
}

Judgement Typechecker::getype(const Ast* const a, SymbolTable* env)
{
  /*
  there has to be a better way of doing this,
  but c++ refuses to dispatch over runtime type
  because it's hard i suppose.
  */
  if (!a)
    throw "bad ast node";

  if (EmptyNode* e = dynamic_cast<EmptyNode*>(a); e != nullptr)
  {
    return getype(e, env);
  }

  if (VariableNode* v = dynamic_cast<VariableNode*>(a); v != nullptr)
  {
    return getype(v, env);
  }

  if (CallNode* c = dynamic_cast<CallNode*>(a); c != nullptr)
  {
    return getype(c, env);
  }

  if (BindNode* b = dynamic_cast<BindNode*>(a); b != nullptr)
  {
    return getype(b, env);
  }

  if (BinopNode* b = dynamic_cast<BinopNode*>(a); b != nullptr)
  {
    return getype(b, env);
  }

  if (UnopNode* u = dynamic_cast<UnopNode*>(a); u != nullptr)
  {
    return getype(u, env);
  }

  if (CondNode* c = dynamic_cast<CondNode*>(a); c != nullptr)
  {
    return getype(c, env);
  }

  if (EntityNode* e = dynamic_cast<EntityNode*>(a); e != nullptr)
  {
    return getype(e, env);
  }

  throw "bad ast node type\n";
}

Judgement Typechecker::getype(const EmptyNode* const e, SymbolTable* env)
{
  /*
  any empty term has no type. as it is not anything.
  we signal this with AtomicType::Undef,
  but this may warrant a None type?
  i don't really know just yet.
  (it's definitely not AtomicType::Nil,
   as that is the type of the nil literal.)
  */
  if (!e)
    throw "bad empty node\n";

  return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Undef, e.loc)))
}

Judgement Typechecker::getype(const VariableNode* const v, SymbolTable* env)
{
  /*
        id is-in FV(ENV)
      ------------------
    ENV |- id : type = value : type
  */
  if (!v)
    throw "bad variable node\n";

  Location dummy;
  optional<unique_ptr<Ast>> bound_term = env[v.id];

  if (bound_term) {
    return Judgement(getype(bound_term->get(), env));
  } else {
    return Judgement(v.loc, "name undefined in environment");
  }
}

Judgement Typechecker::getype(const Call* const c, SymbolTable* env)
{
  /*
  ENV |- term1 : type1 -> type2, term2 : type1
  --------------------------------------------
          ENV |- term1 term2 : type2
  */
  if (!c)
    throw "bad call node\n";

  Judgement typeA = getype(c->lhs.get(), env);

  if (typeA)
  {
    auto proctype = dynamic_cast<ProcType*>(typeA.jdgmt.get());

    // if the type of the lhs is not an
    // instance of a procedure type, then
    // there is no way we can continue
    // the judgement normally.
    // for how are we to extract a [type -> type]
    // from a single [type]?
    if (proctype)
    {
      // if the lhs is directly a procedure,
      // we could look at the boolean held
      // within to trivially tell if the
      // procedure is polymorphic.
      // however, since we cannot be garunteed
      // that the lhs of a call expression will
      // be a procedure directly, we must
      // induct over the type of the lhs term.
      auto type1 = dynamic_cast<TypeNode*>(proctype->lhs.get());
      auto type2 = dynamic_cast<TypeNode*>(proctype->rhs.get());

      if (!type1)
        throw "bad typeA lhs type ptr\n";

      if (!type2)
        throw "bad typeA rhs type ptr\n";

      if (type1->is_poly_type())
      {
        return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Poly, Location())));
      }
      else
      {
        Judgement typeB = getype(c->rhs.get(), env);

        if (typeB)
        {
          // does type1 equal typeB?
          if (equivalent(type1, typeB.jdgmt.get()))
          {
            // return type2, wrapped in an entity and a judgement.
            return Judgement(make_unique<EntityNode>(*type2, Location()));
          }
          else
          {
            string errstr = "target type ["
                          + type1->to_string()
                          + "] not equivalent to formal type ["
                          + typeB.jdgmt.get()->to_string()
                          + "]\n";
            return Judgement(TypeError(c->loc, errstr))
          }
        }
        else
        {
          // the rhs of this call was untypeable for the
          // reason stored within the Judgement TypeB
          return typeB;
        }
      } // !else
    }
    else
    {
      /*
      "the name has monomorphic type, so how could we
       extract some [type -> type] from just a [type]?"

      the instance where we can extract a procedure type
      ourselves, is when we see a polymorphic name, we
      can presume it will be filled with some procedure
      type by the programmer upon calling the polymorphic procedure.
      thusly we can allow any polymorphic term
      which we see to typecheck;
      and subsequently delay the actual typing of this term
      until we are typing within some call to HasInstance.
      this lets the body of some polymorphic procedure
      assume any form allowed by the grammar. but we
      still maintain the constraint that any term we
      evaluate be statically typeable.
      */
      auto monotype = dynamic_cast<AtomicType*>(typeA.jdgmt.get());

      if (monotype && monotype->type == AtomicType::Poly)
      {
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
        return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Poly, Location())));
      }
      else if (!monotype)
      {
        // the unique_ptr does not point to an instance of
        // an atomic type or a proc type, i dont know what
        // it points to, but it's very wrong, whatever it is.
        throw "bad typeA jdgmt pointer\n";
      }
      else
      {
        // monotype is filled, just not with a polymorphic name,
        // so we cannot 'call' a non procedure term.
        string errstr = "cannot call non-procedure type ["
                      + monotype->to_string()
                      + "]\n";
        return Judgement(TypeError(c->lhs.get()->loc, errstr));
      }
    }
  }
  else
  {
    // lhs of the call is not typeable
    // the reason is stored within the Judgement typeA
    return typeA;
  }
}

Judgement Typechecker::getype(const Bind* const b, SymbolTable* env)
{
    /*
      ENV |- term2 : type2
  ---------------------------------
    ENV |- id := term2 : Nil
    */

    Judgement type2 = getype(b->rhs.get(), env);

    if (type2)
    {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Nil), Location()));
    }
    else
    {
      return type2;
    }
}



Judgement Typechecker::getype(const Cond* const c, SymbolTable* env)
{
  /*
    ENV |- 'if' t1 : T1 'then' t2 : T2 'else' t3 : T3,
    if T1 has-type Bool, and T2 is-equal-to T3,
    -------------------------------------------------
    ENV |- ('if' t1 : T1 'then' t2 : T2 'else' t3 : T3) : T2
  */

  Judgement testtype = getype(c->test.get(), env);

  if (!testtype)
    return testtype;

  unique_ptr<TypeNode> booltype = make_unique<AtomicType>(AtomicType(PrimitiveType::Bool), Location());

  Judgement test1 = equivalent(testtype.jdgmt.get(), booltype.get());

  if (!test1)
  {
    return test1;
  }

  Judgement firsttype = getype(c->first.get(), env);

  if (!firsttype)
    return firsttype;

  Judgement secondtype = getype(c->second.get(), env);

  if (!secondtype)
    return secondtype;

  Judgement test2 = equivalent(firsttype.jdgmt.get(), secondtype.jdgmt.get());

  return test2;
}

Judgement Typechecker::getype(const Entity* const e, SymbolTable* env)
{
  switch(e->value_tag) {
    case EntityValueTag::Undef: {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Undef, Location())));
      break;
    }

    case EntityValueTag::Type: {
      return getype(e->type.get(), env);
      break;
    }

    case EntityValueTag::Nil: {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Nil), Location()));
      break;
    }

    case EntityValueTag::Int: {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Int), Location()));
      break;
    }

    case EntityValueTag::Bool: {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Bool), Location()));
      break;
    }

    case EntityValueTag::Proc: {
      return getype(&e->procedure, env);
      break;
    }

    default:
      throw "malformed value_tag\n";
  }
}

Judgement Typechecker::getype(const Procedure* const p, SymbolTable* env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */

}

Judgement Typechecker::getype(const AtomicType* const a, SymbolTable* env)
{
  switch(a->type) {
    case PrimitiveType::Undef:
      return Judgement(PrimitiveType::Undef);

    case PrimitiveType::Nil:
      return Judgement(PrimitiveType::Nil);

    case PrimitiveType::Int:
      return Judgement(PrimitiveType::Int);

    case PrimitiveType::Bool:
      return Judgement(PrimitiveType::Bool);

    case PrimitiveType::Poly:
      return Judgement(PrimitiveType::Poly);

    default:
      throw "malformed PrimitiveType tag";
  }
}

Judgement Typechecker::getype(const ProcType* const p, SymbolTable* env);
Judgement Typechecker::getype(const Binop* const b, SymbolTable* env);
Judgement Typechecker::getype(const Unop* const u, SymbolTable* env);
