
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

  if (!t1)
    throw "bad t1 type pointer\n";

  if (!t2)
    throw "bad t2 type pointer\n";


  if (const AtomicType* at1 = dynamic_cast<const AtomicType*>(t1); at1 != nullptr)
  {
    if (const AtomicType* at2 = dynamic_cast<const AtomicType*>(t2); at2 != nullptr)
    {
      if (at1->type == at2->type)
      {
        return Judgement(at1);
      }
      else
      {
        string errstr = "atomic types t1:["
                      + t1->to_string()
                      + "] t2:["
                      + t2->to_string()
                      + "] not equal\n";
        return Judgement(Location(), errstr);
      }
    }
    else
    {
      string errstr = "cannot compare atomic type t1:["
                    + t1->to_string()
                    + "] to procedure type t2:["
                    + t2->to_string()
                    + "]\n";
      return Judgement(Location(), errstr);
    }
  }
  else if (const ProcType* pt1 = dynamic_cast<const ProcType*>(t1); pt1 != nullptr)
  {
    if (const ProcType* pt2 = dynamic_cast<const ProcType*>(t2); pt2 != nullptr)
    {
      auto j1 = equivalent(pt1->lhs.get(), pt2->lhs.get());
      // if j1 is true, it contains information we need,
      // if j2 is false, it contains information we need.
      if (j1)
      {
        auto j2 = equivalent(pt1->rhs.get(), pt2->rhs.get());
        if (j2)
        {
          // we need both sides of the procedure types to be equivalent
          // to judge the procedure types equivalent.
          return Judgement(pt1);
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
    throw "bad type node";
}

Judgement Typechecker::getype(const Ast* const a, SymbolTable* env)
{
  /*
  there has to be a better way of doing this,
  but c++ refuses to dispatch over more than one
  runtime type because it's hard.
  */
  if (!a)
    throw "bad ast node";

  if (const EmptyNode* e = dynamic_cast<const EmptyNode*>(a); e != nullptr)
  {
    return getype(e, env);
  }

  else if (const VariableNode* v = dynamic_cast<const VariableNode*>(a); v != nullptr)
  {
    return getype(v, env);
  }

  else if (const CallNode* c = dynamic_cast<const CallNode*>(a); c != nullptr)
  {
    return getype(c, env);
  }

  else if (const BindNode* b = dynamic_cast<const BindNode*>(a); b != nullptr)
  {
    return getype(b, env);
  }

  else if (const BinopNode* b = dynamic_cast<const BinopNode*>(a); b != nullptr)
  {
    return getype(b, env);
  }

  else if (const UnopNode* u = dynamic_cast<const UnopNode*>(a); u != nullptr)
  {
    return getype(u, env);
  }

  else if (const CondNode* c = dynamic_cast<const CondNode*>(a); c != nullptr)
  {
    return getype(c, env);
  }

  else if (const EntityNode* e = dynamic_cast<const EntityNode*>(a); e != nullptr)
  {
    return getype(e, env);
  }

  else
  {
    throw "bad ast node type\n";
  }

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

Judgement Typechecker::getype(const CallNode* const c, SymbolTable* env)
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

Judgement Typechecker::getype(const BindNode* const b, SymbolTable* env)
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



Judgement Typechecker::getype(const CondNode* const c, SymbolTable* env)
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

Judgement Typechecker::getype(const EntityNode* const e, SymbolTable* env)
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

Judgement Typechecker::getype(const ProcedureNode* const p, SymbolTable* env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */

}

Judgement Typechecker::getype(const AtomicType* const a, SymbolTable* env)
{
  return Judgement(a);
}

Judgement Typechecker::getype(const ProcType* const p, SymbolTable* env)
{
  return Judgement(p);
}

Judgement Typechecker::getype(const BinopNode* const b, SymbolTable* env)
{
  /*
    a binop is a two argument procedure.
    so, we need a set of binary procedures
    against which we can lookup the op,
    to retrieve a procedure, in the same
    way that we name regular procedures and
    can lookup the procedure to find
    it's definition.

    this raises an issue however,
    given that procedures only take a single
    argument, how do we propose to define
    binary operators in terms of single argument
    procedures?

    to me, it would be nice if they were
    identical to procedures, it just so happens
    that their name must be made up of a subset
    of characters, and they have a precedence
    and associativity associated with their
    definition. so, we need a symboltable
    which is specifically for operators?
    and do we provide some sort of
    half-applied procedure object, which
    would support partial application of
    both procedures and operators?
    by partial application, i really mean
    currying. the order of arguments matters,
    as conceptually, multiple arguments
    are a tuple. if we call a procedure
    with less than the normal number of
    arguments, we can store the reseulting
    object as a closure, with the previously
    given set of arguments stored somehow
    and passed into the procedure via the
    associated parameters, with the unfilled
    arguments making up the argument list
    of the resulting object. if that makes sense.

    add := \x => \y => x + y

    add \(x, y) => x + y

    inc := add 1

    in the interpretive environment, with
    the currect execution schema, we can
    support closures naturally, as the tree
    itself gets the closed over information
    spliced in (as essentially a local copy)
    inc ==>> \y => (1) + y

    but this makes no sense from an assembly
    perspective, as there is no tree to
    dynamically express the procedure.
    instead we have a procedure body
    represented in assembly.
    a partially applied procedure seems
    to require a possibly infinite number
    of procedure definitions, one for each
    specific argument value. however,
    if instead we use the same procedure
    body, and instead store purely the
    arguments until such time as the
    entire argument list is provided.
    we can define this procedure 'inc'
    as a call to the procedure add, where
    the first argument is always the value
    one. the compiler can thusly choose to
    construct a new procedure body, with
    the sigle variable of some type, replaced
    by a literal value in the same location
    (according to however that value is stored
     when passed as an argument.)
    or, we can utilize the exact same procedure
    body, and implement every application of
    the inc procedure as exactly a call to
    add with the first argument filled in by
    the literal value 1.
    this seems to follow for any given type that
    can be stored as a local value. we can store
    the 'saved' value in memory, and store the
    closure as a procedure address and a pointer
    to the closed over values. then when the
    compiler wants to emit a procedure call
    it can use the closure values to fill in
    the missing arguments, use the call site
    provided arguments to fill in the rest of
    the argument list, and invoke the procedure.
    inc  ==>> \(x : Int = 1, y) => x + y

    int 2 ==>> \(x = 1, y = 2) => 1 + 2
          => 3

    in effect, inc is a wrapper around a
    call to add, where the first argument
    is bound to the definition of the wrapper
    within an object I name the closure, (formally
    the closure is this data in addition to a pointer to
    the function.)
    if the closed over name is a variable and
    not a constant, then the closure will need
    to live at least on the stack, to be
    available for modification, per instance of the
    call to the closure. if this wrapper/closure is returned
    by some callee, then it's closure must live in
    the heap, and be deallocated when we no longer
    have a refrence to the closure object. (it's name
    falls out of scope)

  */
}

Judgement Typechecker::getype(const UnopNode* const u, SymbolTable* env)
{

}
