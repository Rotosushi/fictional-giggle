
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

Judgement equivalent(const TypeNode* t1, const TypeNode* t2)
{
  /*
    we use structural equivalence here.
    the reasoning being correctness.
    for efficiency we are thinking about
    utilizing LLVM types as our representation,
    as then we inherit the property that
    type equivalence is both structural,
    and costs the same as pointer comparison.
    given the LLVM representation of types.
    this conservatively implies a speedup.

    structural equivalence is induction
    over the structure of the
    types. its essentially how a person
    would compare the types, given that
    you gave the person some physical
    representation of both types,
    perhaps a drawing of the tree like
    structure with typenames given for
    both aggregates and components.
    it is straightforward, given some
    [type] and some [type] we can say
    the types are equal if the represent the
    same primitive type.
    given some [type -> type] and some
    other [type -> type] we can say they
    are equal if and only if their
    left and right hand side types
    compare equal (according to the
    same two cases we just laid out,
    this is the process called induction.
    a.k.a. recursion)
    if we are given a [type] and a [type -> type]
    or visa-versa we can immediately conclude false.


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
                    + t1->to_string()
                    + "] to atomic type t2:["
                    + t2->to_string()
                    + "]";
      return Judgement(Location(), errstr);
    }
  }
  else
    throw "bad type node";
}

Judgement Typechecker::getype(const Ast* const a)
{
  if (!a)
    throw "bad ast node";

  /*
  oh wow, this code seems awfully repetitive,
  I wonder if there is some way to automate it's generation...
  */
  if (const EmptyNode* e = dynamic_cast<const EmptyNode*>(a); e != nullptr)
  {
    return getype(e);
  }

  else if (const VariableNode* v = dynamic_cast<const VariableNode*>(a); v != nullptr)
  {
    return getype(v);
  }

  else if (const CallNode* c = dynamic_cast<const CallNode*>(a); c != nullptr)
  {
    return getype(c);
  }

  else if (const BindNode* b = dynamic_cast<const BindNode*>(a); b != nullptr)
  {
    return getype(b);
  }

  else if (const BinopNode* b = dynamic_cast<const BinopNode*>(a); b != nullptr)
  {
    return getype(b);
  }

  else if (const UnopNode* u = dynamic_cast<const UnopNode*>(a); u != nullptr)
  {
    return getype(u);
  }

  else if (const CondNode* c = dynamic_cast<const CondNode*>(a); c != nullptr)
  {
    return getype(c);
  }

  else if (const EntityNode* e = dynamic_cast<const EntityNode*>(a); e != nullptr)
  {
    return getype(e);
  }

  else
  {
    throw "bad ast node type\n";
  }

}

Judgement Typechecker::getype(const EmptyNode* const e)
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

  return Judgement(PrimitiveType::Undef);
}

Judgement Typechecker::getype(const VariableNode* const v)
{
  /*
        id is-in FV(ENV)
      ------------------
    ENV |- id : type = value : type
  */
  if (!v)
    throw "bad variable node\n";

  Location dummy;
  // we query the current scope for the symbol,
  // which allows the typechecker to supply a new
  // scope as appropriate, and this code is none
  // the wiser. and, because each scope is tied to
  // it's enclosing scope, the operator[] actually
  // recursively traverses from the current scope to
  // the outermost scope in order to lookup the name.
  optional<unique_ptr<Ast>> bound_term = (*(scopes.top()))[v->id];

  if (bound_term)
  {
    Judgement bound_type = getype(bound_term->get());
    return Judgement(bound_type);
  }
  else
  {
    return Judgement(v->loc, "name undefined in environment");
  }
}

Judgement Typechecker::getype(const CallNode* const c)
{
  /*
  ENV |- term1 : type1 -> type2, term2 : type1
  --------------------------------------------
          ENV |- term1 term2 : type2


  also observe that given any length call expression,
  the top of the tree's right hand side will contain
  the rightmost argument in the application site.
  and the left hand side will be a pointer to the
  rest of the call expression.
  the rest of the call expression is always one of two
  cases; either:
    a) the node's right hand side contains a pointer to the next argument
        and the left hand side a pointer to the rest of the expression.
    b) the node's right hand side contains the leftmost argument,
        and the node's left hand side contains something which
        can be called (it's a literal procedure, or a variable
        which has been bound to a procedure, or its a memory
        cell containing the address of a procedure, or the like.)

  */
  if (!c)
    throw "bad call node\n";

  Judgement typeA = getype(c->lhs.get());

  // this is a dynamic lookup on the
  // memory contained within the Judgement
  // typeA. called Judgement.succeeded
  // this includes at least a read and
  // a comparison against the local stack space.
  if (typeA)
  {
    auto proctype = dynamic_cast<ProcType*>(typeA.u.jdgmt.get());

    // if the type of the lhs is not an
    // instance of a procedure type, then
    // there is no way we can continue
    // the judgement normally.
    // for how are we to extract a [type -> type]
    // from a single [type]?

    // whereas here the implicit conversion to a boolean
    // is along the lines of whether or not this
    // pointer points to nullptr or not. again, the
    // name itself is simply a stack local memory cell.
    // and in both cases we are testing the
    // 'validity' of the name. i.e. is it in a state
    // where the computation can continue, or do we
    // need to divert course?
    if (proctype)
    {

      if (proctype->is_poly_type())
      {
        /*
          okay, having thought about this some more,
          any given term which is being typed, when we
          type one of said terms subterms as polymorphic,
          we conclude that the type of the whole term is
          polymorphic. this essentially promotes the terms
          type from monomorphic to polymorphic.
          now, polymorphic terms are always introduced in
          such a way that they can then subsequently be bound
          to another type at a later point in the program.
          this means that while we are within the confines
          of the typechecking algortihm, when we observe that
          a subterm is polymorphic, we also know that we
          are in a subexpression ourselves and this subexpression
          is, by way of HasInstance, going to be typechecked again.
          at the time when this polymorphic name is being bound to
          some other type. given this knowledge, we choose to delay
          the actual typing of this expression. (for we cannot really
          compare Poly == Mono sensibly)
          this sort-of allows the programmer to sneak whatever code they
          want past the interpreter as long as it is polymorphic.
          but, once they go to execute said polymorphic procedure,
          unless they themselves introduced a type which satisfys
          the type constraints present in the expression, they
          will encounter a type error. a static type error, in fact.

        */
        return Judgement(PrimitiveType::Poly);
      }
      else
      {
        Judgement typeB = getype(c->rhs.get());

        if (typeB)
        {
          auto type1 = dynamic_cast<TypeNode*>(proctype->lhs.get());

          if (!type1)
            throw "bad typeA lhs type ptr\n";
          // does type1 equal typeB?
          if (equivalent(type1, dynamic_cast<TypeNode*>(typeB.u.jdgmt.get())))
          {
            auto type2 = dynamic_cast<TypeNode*>(proctype->rhs.get());

            if (!type2)
              throw "bad typeA rhs type ptr\n";

            return Judgement(type2);
          }
          else
          {
            string errstr = "target type ["
                          + type1->to_string()
                          + "] not equivalent to formal type ["
                          + typeB.u.jdgmt.get()->to_string()
                          + "]\n";
            return Judgement(TypeError(c->loc, errstr));
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
      ourselves, is when we see a polymorphic type,
      we can presume the name will be filled with some procedure
      type by the programmer upon calling the polymorphic procedure.
      as that is the only way in which we can make this make sense.
      but that is up to the programmer.
      thusly we can allow any polymorphic term
      which we see to typecheck, as we can implicitly
      make the assumption that when the programmer gives
      the procedure some actual type, we can subsequently
      type the procedure given this new type, and the usual
      typing rules apply.
      this in effect delays the actual typing of this term
      until we are typing within some call to HasInstance.
      this lets the body of some polymorphic procedure
      assume any form allowed by the grammar. but we
      still maintain the constraint that any term we actually
      evaluate be statically typeable. because the only
      way we can get an evaluatable form of any procedure
      is via a call to hasInstance, and hasInstance satisfies
      the typeing constraints strictly. that is, if the call
      fails, that counts as an actual error like a syntax error,
      and stops the interpretation process.
      */
      auto monotype = dynamic_cast<AtomicType*>(typeA.u.jdgmt.get());

      if (monotype && monotype->is_poly_type())
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
          name as a function type poly -> poly.

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
        return Judgement(PrimitiveType::Poly);
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
        // we cannot 'call' a non procedure term.
        string errstr = "cannot call non-procedure type ["
                      + monotype->to_string()
                      + "]\n";
        return Judgement(c->lhs.get()->loc, errstr);
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

Judgement Typechecker::getype(const BindNode* const b)
{
    /*
      ENV |- term2 : type2
  ---------------------------------
    ENV |- id := term2 : type2
    */
    Judgement type2 = getype(b->rhs.get());

    return type2;
}



Judgement Typechecker::getype(const CondNode* const c)
{
  /*
    ENV |- 'if' t1 : T1 'then' t2 : T2 'else' t3 : T3,
    if T1 has-type Bool, and T2 is-equal-to T3,
    -------------------------------------------------
    ENV |- ('if' t1 : T1 'then' t2 : T2 'else' t3 : T3) : T2

    if the term representing the condition has type Bool,
    and the terms representing the alternative expressions
    have the same type as one another, then we can conclude
    that the type of the entire expression is the type of
    the alternative expressions (which are the same type,
    so we arbitrarily select the first)
  */

  Judgement testtype = getype(c->test.get());

  if (!testtype)
    return testtype;

  unique_ptr<TypeNode> booltype = make_unique<AtomicType>(AtomicType(PrimitiveType::Bool), Location());

  Judgement test1 = equivalent(dynamic_cast<TypeNode*>(testtype.u.jdgmt.get()), booltype.get());

  if (!test1)
    return test1;

  Judgement firsttype = getype(c->first.get());

  if (!firsttype)
    return firsttype;

  Judgement secondtype = getype(c->second.get());

  if (!secondtype)
    return secondtype;

  Judgement test2 = equivalent(dynamic_cast<TypeNode*>(firsttype.u.jdgmt.get()), dynamic_cast<TypeNode*>(secondtype.jdgmt.get()));

  return test2;
}

Judgement Typechecker::getype(const WhileNode* const w)
{
  /*
  ENV |- 'while' t1 : T1 'do' t2 : T2,
  if T1 has-type Bool, and t2 : T2
  -------------------------------------------------
  ENV |- ('while' t1 : T1 'do' t2 : T2 ) : T2

  should we assign a meaning to?
  x := while (y < z) y <- f(y)
  */
  Judgement testtype = getype(w->test.get());

  if (!testtype)
    return testtype;

  unique_ptr<TypeNode> booltype = make_unique<AtomicType>(AtomicType(PrimitiveType::Bool), Location());

  Judgement test1 = equivalent(dynamic_cast<TypeNode*>(testtype.u.jdgmt.get()), booltype.get());

  if (!test1)
    return test1;

  Judgement type2 = getype(w->body.get());

  return type2;
}

/*
  entities act as an aggregate object,
  which simplifies using entities from
  an implementation perspective.
  we can treat physical things uniformly,
  and each algorithmic peice can be defined
  in terms of it's effect on these physical
  things. separating the connective tissue
  from the meat as it were.
*/
Judgement Typechecker::getype(const EntityNode* const e)
{
  switch(e->value_tag) {
    case EntityValueTag::Undef: {
      return Judgement(make_unique<EntityNode>(AtomicType(PrimitiveType::Undef, Location())));
      break;
    }

    case EntityValueTag::Type: {
      return getype(e->type.get());
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
      // wrapping the typing procedure in a
      // function here, improves readability
      // by allowing this function to focus on
      // dispatch, and the typeing judgement
      // code is focused on typeing.
      return getype(&e->procedure);
      break;
    }

    default:
      throw "malformed value_tag\n";
  }
}

Judgement Typechecker::getype(const Procedure* const p)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2

    so, presumably, we can rely on the type annotation
    within the arg_type pointer itself. as even if the
    user fails to provide a type, the parser fills in
    said type pointer with  the poly type. so in all cases,
    the annotation contains valid information.

    but we have a further complication in that
    a procedure can be a definition or a literal.
    given some literal, this procedure makes direct sense.
    given some definition, what do we conclude?
    well, a definition is provided in two cases,
    one: this procedure is polymorphic.
      meaning we can return the type as polymorphic,
      and via HasInstance at the application site
      we get a correct and viable control path which
      i think provides the right semantics.

    two: this procedure is monomorphic,
      but has been explicitly overriden
      by the programmer. which type
      do we return then?

      well, they only way this procedure
      would be called is in the case that
      we encounter a variable naming a
      procedure defined earlier in the
      program, and we somehow try and type
      the procedure without applying it.
      for if we go through application,
      we don't type a procedure directly, we see if
      the procedure contains an instance with
      the correct type via HasInstance.
      in the cases where a programmer wants to
      name a procedure without applying it
      they must be trying to obtain a function
      pointer or something? in those cases, we
      still eliminate the later application via
      a call to HasInstance, so maybe it's fine
      to treat a lone overriden procedure name,
      as an instance of it's defining form,
      until such time as we apply said procedure,
      where we use HasInstance to eliminate the
      instance of the procedure. meaning treating
      the procedure as an instance of any of
      it's particular forms is immaterial in this
      particular case?

      this is a choice which i am making because of
      logic i thought of. i have no idea what the formal
      approach to this is. I suppose i am saying all this
      to mark this location as a potential hazard.
  */
  if (p->contains_literal)
  {
    Judgement type1 = getype(p->u.literal.arg_type.get());

    if (type1)
    {
      scopes.push(&p->u.literal.scope);
      scopes.top()->bind(p->u.literal.arg_id, p->u.literal.arg_type.get());
      Judgement type2 = getype(p->u.literal.body.get());
      scopes.top()->unbind(p->u.literal.arg_id);
      scopes.pop();

      if (type2)
      {
        unique_ptr<TypeNode> lhstype = type1.jdgmt;
        unique_ptr<TypeNode> rhstype = type2.jdgmt;
        return Judgement(ProcType(lhstype, rhstype));
      }
      else
      {
          return type2;
      }
    }
    else
    {
      return type1;
    }

  }
  else
  {
    Judgement type1 = getype(p->definition.def.arg_type.get());

    if (type1)
    {
      scopes.push(&p->definition.def.scope);
      scopes.top()->bind(p->definition.def.arg_id, p->definition.def.arg_type.get());
      Judgement type2 = getype(p->definition.def.body.get());
      scopes.top()->unbind(p->u.definition.def.arg_id);
      scopes.pop();

      if (type2)
      {
        unique_ptr<TypeNode> lhstype = type1.jdgmt;
        unique_ptr<TypeNode> rhstype = type2.jdgmt;
        return Judgement(ProcType(lhstype, rhstype));
      }
      else
      {
        return type2;
      }
    }
    else
    {
      return type1;
    }
  }
}

Judgement Typechecker::getype(const AtomicType* const a)
{
  // judgeing a type is easy!
  // given i write the correct constructors.
  return Judgement(a);
}

Judgement Typechecker::getype(const ProcType* const p)
{
  return Judgement(p);
}

Judgement Typechecker::getype(const BinopNode* const b)
{
  /*
    the type of a binop node is dependent upon
    three peices of information.
    (well, the number
    is itself dependant upon how you slice the information,
    but lets go with three).
    the type of the operation itself.
    and the types of the left and right arguments.

    the type of the operation itself is always of
    the form
    [type1 -> type2 -> type3]

    and the type of the left and right arguments
    will always be some
    [type]

    we need to satisfy a few invariants about the
    forms of the types themselves for these three
    peices of information, namely

    the left hand side must have type1
    the right hand side must have type2.
    only then can we conclude that the
    type of the entire expression is type3

formally:
ENV |- fn (op) : T1 -> T2 -> T3, lhs : T1, rhs : T2
------------------------------------
        ENV |- lhs op rhs : T3



    we have a slight complication however,
    and it is along the lines of user-definitions
    and implementer-definitions.
    a.k.a. composite vs. primitive operations.
    namely, how do we best implement typechecking
    against both primitive and composite operations.
    primitive operations are going to
    be implemented in terms of the Ast, interpretively,
    and composite operations are going to be implemented
    atop regular procedure application.

    (
    in the compiled
    sense we have the same dichotimy, but different semantics
    behind primitive and composite operations, instead
    of primitive operations being in terms of the Ast,
    they will be some series of assembly instrctions
    operating over memory cells, and composite operations
    will be built atop function call semantics.
    )

    but, the typeing judgement is identical given either
    a primitive or a composite operation.
    so it would be nice to hide the details of primitiveness
    and compositeness from the typechecker. allowing
    the judgement itself to be more readible, and
    the invarients specified above still enforced.
    we can thusly separate the large dispatch code
    of checking against the known set of operations
    from the more generic dispatch code which utilizes
    the symboltable to find the definitions.


    so, user defined (composite) binops are saved within
    their defining environment. meaning we should use
    the usual lookup mechanism to find binops in at least
    one case. in the other case, primitive binops are defined
    by the langauge. this means that we need to maintain a list
    of binops definitions whose bodies instead operate upon
    the ast.
  */

  /*
    if binop is primitive
      using some sort of collection of
      operators and arguments we use the
      operator to find the right overload
      set and then look through the set for
      the matching left and right hand side types.
      once we have a match, we can return the
      result type of the operation as the
      result type. if no matches are found then
      while the operator was primitive, we couldn't
      find a matching overload for the given types.
    if binop is not primitive,
      we use the regular name lookup mechanisms
      to discover the term to evaluate and if we
      can discover the correct term we can return
      the result type of the term as the resulting
      type. if we do not find a match then either
      the binop is undefined, or the collection
      did not contain a definition acting on the
      right types.

      since both of these hinge on finding a match
      we can probably unify that semantics between them,
      but in the case of a primitive operation we
      eventually need to call a procedure acting on
      Asts. wheras if the operation is composite we
      can instead treat the term like a procedure call
      with the left and right hand sides as the arguments
      to the procedure.

      the unop semantics are identical,
      except for the number of arguments.

      so perhaps we can store a pointer to a procedure
      which can be used to eliminate primitive binops
      and return the resulting value, within the structure
      which saves binops, and in the case of a composite
      binop we instead store a pointer to the user
      defined procedure which can be used to eliminate the
      user defined operation, and return the resulting value.

      this isn't stored in the ast however. the Ast should
      remain the same, what needs to be added is a binop
      record which saves these things and can be looked
      up by the regular lookup mechanisms. then the
      elimination mechanisms should be able to be unifyed
      around these two abstractions.
  */
}

Judgement Typechecker::getype(const UnopNode* const u)
{
  /*
  a unop is an operation acting upon
  a single entity of a given type.
  this, plus the desire to overload symbols accross
  unary and binary forms, (ex: we want '-' to mean both
  unary negation, and binary subtraction, to align with
  common mathematical notation.)
   means we need to maintain separate
  dictionaries or unops and binops, and use the grammar
  rules to disambiguate.

  whenever I figure out how I want to actually typecheck
  binary operations, I am sure the unop typechecking
  algorithm will be similar.
  */
}
