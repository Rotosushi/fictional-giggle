/*
type system:
the type system of a programming language is
one third of the language, with the other third
being it's grammar, and the last third being its
semantics. typing is what makes sure that what is parsed
can be expressed semantically. acting almost as a bridge between
what statements can be composed (according to the grammar) and what
composed statements can be expressed (according to the semantics).
type systems are fundamentally inductive systems.

type systems are concerned with:

type equivalence

type compatability

describing new and more expressive types.
by means of:
  Sum, Product, and Refrence types.
  Subtyping
  Interfaces
  Modules
  Dependent types
  etc...

what can a type be in pink?

type:
    nil
  | type -> type
  | ( type )

formally, we only need to consider two types
in pink v0.0.1, nil and type -> type

in the future, we are going to have two type kinds
* and * -> *
which will represent the basic types in the language.
* meaning any type we can refrence with an identifier.
and * -> * being the type of functions. and maybe even
the type of polymorphic functions when we get there.

type equivalence:
	when can we say that two types are equivalent?
  for v0.0.1
    if type1 == Nil && type2 == Nil
    then they-are-equivalent
    else we need to recursively check that
         the function types are equal.


type compatibility:
  	when can we say that a particular usage of a type
  	is valid?

    this is where the typeing rules come in,
    we start by looking at the root of the tree,
    each node has an associated typing judgement,
    which determines when the node holds valid
    or invalid syntax, we type each subterm until we
    reach the bottom of the tree, and then
    we build up from equivalence, recursively
    building up typing succesive judgements, and reducing
    these judgements while walking out of the tree.
    essentially:
      in-order-traverse(ast, typechecker-node-actions);

type inference:
  	how do we deduce the type of an expression?
  	from it's contents and surrounding context.
    each element of the language which allows a widening
    of possible types valid within a given term
    has the possibility of making typeing undecidable.
    famously, a type-annotation free syntax of
    System F is untypable. the typeable
    subset of System F first discovered is the
    Hindley-Milner-Damas typesystem. now, within
    the languages specification we can observe the exact
    confounding factors. the question that is asked
    for type-inference is "given an expression and an
    environment, can I uniquely find a type that makes
    the expression typecheck.", and the key feature of
    system F is that the types of a polymorphic procedure
    can be bound to any type known to the language.
    if we consider these two facts together for some
    time we may come to observe that given the second
    fact, any polymorphic procedure can be
    bound by any monomorphic type. meaning that
    the polymorphic expression cannot be uniquely assigned
    a single type, merely by it's definition.
    (this is probably why it was called an embarrassingly
     open problem in computer science.)
    so, how does pink approach this?
    well, first by way of c++, in c++ parametric polymorphism
    is done by way of what are known as templates.
    these functions allow for something similar to functional
    parametric polymorphism, except that the programmer must explicitly
    syntactically request a monomorphic instance.
    as has been noted by programmers from lisp to modern c++,
    having to spend a large amount of time keying in
    type annotations makes programming far more laborious than
    it has to be, which defeats the purpose of a higher level language
    in the first place, because we want to avoid the mind-numbing tedium of
    assembly programming. not because the logic in unessential to the
    workings of the mchine, but because the tedium can be folded away
    into abstraction, and we can then focus on the unique logic
    of the overall assembly method.
    all this to say, we are fundamentally asking a different question
    than "given an expression and an environment can we deduce a type".
    Each time the programmer uses the abstraction is fundamentally
    different that the definition of the abstraction.
    the definition is a point of non-determinism, by it's
    nature, and each place of application is a
    specific deterministic choice by it's nature. so, instead of asking the above
    question, we instead ask "given a polymoprhic expression, does
    some particular application typecheck?" which provides
    full coverage of every useage of the abstraction, simply by
    tying it's definition to the usage, and it provides the same
    level of abstraction, because the parametric polymorphism is
    still provided by means of binding type-variables to
    monomorphic types. this is not in any sense of the phrase
    'type-inference', however it is an instance of parametric polymorphism.
    if we say that leaving out an explicit annotation equals definining
    a polymorphic abstraction, then we can provide what looks like
    type-inference based 'type-annotation-free' syntax, but
    we can sidestep using an actual type-inference algorithm.
    c++ requires explicit instanciation syntax, which becomes
    just as cumbersome as type annotations in LISP became.
    in order to avoid such issues, we simply elide explicit
    instanciation, and we instead observe that everytime
    we call the polymorphic procedure we must pass in some
    typeable value. by induction on the eager evaluation rules.
    if we imagine a polymorphic procedure being instanciated with
    another polymorphic procedure, then that builds up two
    arguments which must provide some type, in order to unroll those
    abstractions into an evaluatable expression. we also
    must observe that in order to pass something polymorphic
    as a parameter, that parameter must be treated as if
    it were callable in the body of the procedure. this
    must be true, or the typechecker would observe that a
    non-callable value is called syntactically, and reject that
    instanciation. this means that in all cases where
    a polymorphic name is instanced to a polymorphic value,
    that value must be called within the body. (the other
    valid option is returning the polymorphic value as the result.
    but that would leave the polymorphic name alone within the body,
    and not as a part of any expression.)



type equivalence:
	there are two kinds of type equivalence:
	structural equivalence, which is based on the
	content of type definitions.
	name equivalence: which is based on the lexical
	occurances of the type definition.

	structural equivalence between two types
	refers to the underlying composition of the
	two types. in the simplest case we can consider
	int and real.
	in both cases, pink specifies that the size of
	these two types is equivalent to the size
	of a single machine word. (meaning it supports 64 bit versions
  of each in a 64 bit architecture) so structurally speaking
	these two types are equivalent. simply because
	to cast between the two one only needs to treat
	the machine word as the different type.
  (this is essentially equivalent to what C calls casting.)
	if we consider the case of int -> real we can garuntee the
	cast will not change the state of the underlying representation.
	(we may still destroyinformation however,
	if we were to cast from real -> int for instance.)
	this is in fact the case in a language such as C,
	where you can cast any structurally equivalent type to any other.
  there have been bugs that I myself have written which
  occurred because of a miscast.

	a definition of structural equivalence will need to apply
	not only to every primitive type in the language, but must
	also extend to the types which we can define in the language.
	we must consider what it means to be structurally equivalent
	for algebraic data types, arrays, tuples, and the data primitives.

name equivalence between two types refers to the
	name of the two types. we say that two types are distinct
	based on the fact that their names are different.

	this issue primarily refers to type aliasing.
	in programming languages with type-definitions
	there is usually the ability to define trivially
	different types.
	say:
	type farenheit = real;

	where farenheit is being used to represent temperature.
	how much do we want the programmer to have to redefine
	the semantics of real numbers in the type definition of
	farenheit? when in reality any function that can take
	a real, would work when passed in a farenheit. in C, they
	leverage implicit casts, and structural equivalence to
	allow programmers to 'inherit' the semantics of the
	base type. (i say 'inherit' to distinguish it from
	what is programmatically known as inheritance, which is
	a different way of inheriting semantics.)

	so, my first stab at this is to say that if one defines
	a name equivalence through the algebraic data type mechanism
	we would expect the types to be distinct. if we define
	a new type to be the alias of the old type, we could expect
	to 'inherit' the semantics of the old type. to say that
	another way, the new type name would be semantically valid
	in any place where the old type name would be semantically valid.
  since the type is being used as an alias, we shouldn't consider it
  casting, because both types are equivalent. it's more like
  removing a layer of abstraction to get to the base type.
  but then, what about the degenerate recursive case?
  type a = int
  type b = a
  type c = b
  ...etc
  is 'b' an alias of 'a' or of int?
  is 'c' an alias of 'b' or of 'a' or of int?

  if it is a chain of aliases, the correct solution
  at first feels to me like we somehow remove all
  sense of aliasing an alias, and we only ever alias
  the base type. even in the degenerate case.



type compatibility:
	when can we say that a particular usage of a type
	is valid?

type inference:
	how do we deduce the type of an expression?
	from it's contents and surrounding context.



type casting and conversion:
	the processes of casting or converting one type to the other.

	there are three main cases when we can cast,
	1 - the types can be considered structurally equivalent.
	2 - the types represent two distinct sets of values
		and the values overlap somewhat.
    we could say that (range(type1) intersect range(type2) != empty)
		(such as signed and unsigned integers,
		 or a short and an int)
	3 - the types are not structurally equivalent, but
		we can conceive of a semantically meaninful
		conversion function that maps one type to the other.
		(such as real -> int, when one can accept the loss
		 of precision.)

	non-converting cast:
		a type cast that doesn't change the underlying
		representation, it merely reinterprets it as a value
		of the new type. one can see this in the case of
		memory allocation strategies. where the heap is itself an array
		of bytes, but portions of the array are reinterpreted
		as bookeeping structures or as user types.

	type promotion:
		type promotion is a particular kind of cast that is done
		under-the-hood to provide non-hardware supported types
		in the language. such as simulating a boolean value with
		a full machine word.

I especially don't like implicit type conversion because it
obfuscates the real workhorse of generic interfaces; polymorphism.
It also has the potential to silently destroy information if the
compiler is slightly too liberal in it's application of coercion.
(most compilers limit the process to any type reachable in one step
 for this exact reason.)
explicit casting is an entirely different story exactly because
the user is asking for the coercion to happen. which means that
it is:
  a) semantically meaninful to the code
  b) explicit in it's intent, presence, and execution
all this to say, maybe we look into casting/coercion primitives?

polymorphism:
  	abstraction over types,
  	there are three kinds of polymorphism we can consider.

Ad-Hoc Polymorphism:
    this is usually considered "operator overloading"
    as in, we use the same syntax to denote separate
    implementations which is distinguished by the type
    of the operators arguments.
    in example (3.14 * 5.08 ^ 2.0)
            vs (3 * 5 ^ 2)

    the compiler needs to generate a separate and unique
    sequence of instructions when multiplying or squaring
    floating point values vs integer values. yet the
    programmer uses the same "multiply" symbol to denote
    these two separate functions.

    which to me, sounds
    like what c++ calls template specialization.
    if we consider a polymorphic function definition to be
    a set constructor, where each member of the set
    is a copy of the body of the definition, just
    with the type variables instanciated with some
    valid type. then ad-hoc polymorphism could be implemented
    by allowing the user to provide their own
    member of this set, with the types instanced to some
    particular type, which is then picked by the compiler
    when the program makes a call providing that particular
    type.
    this is essentially providing an implementation
    of one of the functions denoted by the interface.
    note that the compiler need not create every member
    of this set, it only needs to generate and typecheck
    the members that are called by the user program,
    or instanced like above.


Parametric polymorphism: (what c++ calls templates)
  	that is polymorphism that exists in the parameter of a function.
    formally we allow the type of the parameter to range
    over types and then become specialized for some particular type
    when called with that type.
  	in such a case where the body of a polymorphic function only relies on
  	a certain aspect of any given type, so any type which has that
  	aspect can be correctly passed through the polymorphic parameter.
  	this kind of polymorphism is generally implemented at compile time.
  	where the polymorphic definition acts as a template for function
  	definitions, and a static definition for the function can be defined
  	for every type that is used with that function, each call location
    providing such a type can be correctly tied to the implementation
    of the function for said type by the compiler at each call location.

Subtype polymorphism: (some literature denotes it with "<:" )
  	whereby the code works with values of some base/root type, and new
  	types can be defined which extend the semantics of the original type.
  	the subtypes provide the same interface and interface semantics as
    the root/base type so a function which only relies on the semantics
    provided by the root/base can accept as an argument any subytype of
    the root/base type.
  	and can consistently rely on that/those aspects of the type.


    these three taken together, (or two if we define ad-hoc polymorphism
    in terms of parametric polymorphism.)
    make for a very expressive and typesafe language.
    the other goodies that i would like to bring in is
    parametric type definitions and with that polymorphic
    type definitions.




    ((
      in fact, we can consider the root of execution that is user defined
      to be a specialization of the root set that is understood by the
      compiler. we can define those sets with a condition that checks for
      redefinition, like the uniqueness property of variable definition
      (this implies the existance of function set specialization constraints,
       this could be the location which users can define and enforce
       type constraints!!!!)
    ))



*/
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "typechecker.h"
#include "ast.h"
#include "error.h"

/*
  we have to make all typeing judgements
  with respect to an environment. which
  in the literature is usually treated as
  a linked list of the symbol information,
  however, I like the idea of doing a hash
  table for the readability it gives to
  the typeing judgements. and the optimization
  in the search time. though it has to store
  about as much overhead as a singly-linked list
  in the hash-value-indexed-array-of-pointers-to-symbols implementation
  and is quite literally a list in the
  hash-value-lookup-in-a-linked-list-of-symbols implementation.
  (also known as ordered)
 */



Ast* typeofEntityType(Ast* type, Symboltable* env);
Ast* typeofEntityLiteralProcedure(Ast* lambda, Symboltable* env);
Ast* typeofEntityLiteral(Ast* literal, Symboltable* env);
Ast* typeofEntity(Ast* entity, Symboltable* env);
Ast* typeofId(Ast* id, Symboltable* env);
Ast* typeofCall(Ast* call, Symboltable* env);
Ast* typeofBind(Ast* bind, Symboltable* env);
Ast* typeofBinop(Ast* binop, Symboltable* env);
Ast* typeofUnop(Ast* unop, Symboltable* env);

bool is_polymorphic(Ast* type) {
  if (type == NULL) return false;

  if (type->tag == N_ENTITY) {
    if (type->u.entity.tag == E_TYPE) {
      if (type->u.entity.u.type.tag == T_POLY)
        return true;
      else if (type->u.entity.u.type.tag == T_NIL)
        return false;
      else if (type->u.entity.u.type.tag == T_PROC) {
        return is_polymorphic(type->u.entity.u.type.u.proc.lhs)
            || is_polymorphic(type->u.entity.u.type.u.proc.rhs);
      }
    }
    return false;
  }
  return false;
}

/*
  typeof will recur equal to the depth of the Ast passed

  the typeof some term is defined by the types of
  it's subterms, until we reach the base case
  which is either nil or some type -> type;
*/
Ast* type_of(Ast* term, Symboltable* env)
{
  if (term != NULL) {
      switch(term->tag) {
        case N_ID:     return typeofId(term, env);
        case N_ENTITY: return typeofEntity(term, env);
        case N_CALL:   return typeofCall(term, env);
        case N_BIND:   return typeofBind(term, env);
        case N_BINOP:  return typeofBinop(term, env);
        case N_UNOP:   return typeofUnop(term, env);
        default:  error_abort("malformed Ast node! aborting", __FILE__, __LINE__);
      }
  }
  else {
    printf("term NULL!\n");
    return NULL;
  }
}

Ast* typeofId(Ast* id, Symboltable* env)
{
  /*
        id is-in FV(ENV)
      ------------------
    ENV |- id : type = value : type
  */
  char* name = id->u.id;
  Ast* term = lookup(name, env);
  if (term != NULL) {
    Ast* type = type_of(term, env);
    DeleteAst(term);
    return type;
  }
  else {
    printf("id \"%s\" not in ENV!\n", name);
    return NULL;
  }
}

Ast* typeofBinop(Ast* binop, Symboltable* env)
{
  /*
      ENV |- fn (op) : T1 -> T2 -> T3, lhs : T1, rhs : T2
      ------------------------------------
              ENV |- lhs op rhs : T3

      (also notice how this is two applications of apbstraction
       by definition, we can avoid this if we definie all binops as
       taking a 2-tuple, then we can avoid the second application.
       though, this would make it impossible to provide half-applied
       binops al-la Haskell.)

      recall:
        T1 -> T2 -> T3
        -->> (-> T1 (-> T2 T3))
        (lisp syntax used here for disambiguation.)

        for the operator '->', all that is required is
        the the lhs and the rhs have a type, because
        the '->' operator is the type of a function entity.
        and functions are used to parametrize expressions.
        and expressions range over every term and type. which means
        any possible type can be the type of the parameter
        of a function. (which said another way, "you can pass
        any type through the parameter of a function",
        seems plainly obvious.)
        expressions are what we use to describe some particular
        sequence of execution of any given type.
        functions binding values of type Type (typenames)
         can parametrize type expressions.
        functions binding values of other types can parametrize
          expressions ranging over those types.
          what is important is that the types provided and returned
          are what is expected by the other entities in the expression.
          not what any particular entites type actually is in any particular
          expression. this connection is also a point of non-determinism,
          this is what it means for a variable to 'range' over values.
          it means that this algorithm works given any particular
          value. this is why we call polymorphism 'higher-order'
          because the values we are ranging over are the very
          entities that are used to range over regular values.
          meaning that we are appliying that non-determinism
          a level of abrstraction higher than before.


        if the operator is + then I can imagine appropriate
        behavior when the argument types are
          Int  -> Int  -> Int
          Real -> Real -> Real
          (addition)

          Text -> Text -> Text
          Char -> Text -> Text
          Text -> Char -> Text
          Char -> Char -> Text
          (concatenation)

          Type -> Type -> Type
          (sum type)


  */
  if (binop != NULL) {
    Ast *T1 = NULL, *Ti = NULL, *T2 = NULL, *T3 = NULL;
    Ast *LT = NULL, *RT = NULL;

    LT = type_of (binop->u.binop.lhs, env);

    if (!LT) {
      printf("binops lhs not typable.");
      return NULL;
    }

    RT = type_of (binop->u.binop.rhs, env);

    if (!RT) {
      printf("binops rhs not typeable.");
      DeleteAst(LT);
      return NULL;
    }

    if (strcmp(binop->u.binop.op, "->") == 0) {
      /*
        this line of code stinks to me.

        are we 110% sure that the operator '->'
        always makes sense no matter which two types
        we give it? it sure seems true on first thought.
        especially when the only types available are
        Nil and Type -> Type. and it even still seems
        to hold when we consider adding in any new
        primitive type, like Real, Char, Text, Bool, etc...
        but does it hold even against composite types
        and refrences? existential types? dependant types?
        we may have to burn this bridge more than once...
      */
      return CreateAstEntityTypeProc(LT, RT, NULL);
    } else {
      printf("binop not known. [%s]", binop->u.binop.op);
      DeleteAst(LT);
      DeleteAst(RT);
      return NULL;
    }
  }
  return NULL;
}

Ast* typeofUnop(Ast* unop, Symboltable* env)
{
  /*
      ENV |- fn (op) : T1 -> T2, term : T1
      ------------------------------------
              ENV |- op term : T2
  */
  if (unop != NULL) {
    return NULL; // there are no unops yet.
  }
  else {
    printf("unop NULL.");
    return NULL;
  }
}

Ast* typeofEntity(Ast* entity, Symboltable* env)
{
  if (entity != NULL) {
    switch (entity->u.entity.tag) {
      case E_TYPE:    return typeofEntityType(entity, env);
      case E_LITERAL: return typeofEntityLiteral(entity, env);
      default:
        error_abort("malformed entity tag! aborting", __FILE__, __LINE__);
    }
  }
  return NULL;
}


Ast* typeofEntityType(Ast* type, Symboltable* env)
{
  if (type != NULL) {
    /* ENV |- nil : Nil */
    if (type->u.entity.u.type.tag == T_NIL) {
      return CreateAstEntityTypeNil(NULL);
    }
    if (type->u.entity.u.type.tag == T_POLY) {
      return CreateAstEntityTypePoly();
    }
    /*
     build up the function type recursively.
      ENV |- :T1, :T2
      ------------------
      ENV |- T1 -> T2
    */
     else if (type->u.entity.u.type.tag == T_PROC) {
      Ast* t1 = typeofEntity(type->u.entity.u.type.u.proc.lhs, env);
      if (t1 != NULL) {
        Ast* t2 = typeofEntity(type->u.entity.u.type.u.proc.rhs, env);
        if (t2 != NULL) {
          return CreateAstEntityTypeProc(t1, t2, NULL);
        }
        else {
          printf("function type type2 NULL\n!");
          return NULL;
        }

      }
      else {
        printf("function type type1 NULL!\n");
        return NULL;
      }
    }
    else
      error_abort("malformed type tag! aborting", __FILE__, __LINE__);

  }
  else {
    printf("type NULL!\n");
    return NULL;
  }
  return NULL;
}

Ast* typeofEntityLiteral(Ast* literal, Symboltable* env)
{
  if (literal != NULL) {
    if (literal->u.entity.u.literal.tag == L_NIL) {
      return CreateAstEntityTypeNil(NULL);
    }
    else if (literal->u.entity.u.literal.tag == L_PROC) {
      return typeofEntityLiteralProcedure(literal, env);
    }
    else
      error_abort("malformed literal tag! aborting", __FILE__, __LINE__);
  }
  return NULL;
}

Ast* typeofEntityLiteralProcedure(Ast* lambda, Symboltable* env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */
  if (lambda != NULL) {
    Ast* type1 = type_of(lambda->u.entity.u.literal.u.proc.def.arg.type, env);
    if (type1 != NULL) {
      /*
        question:
          how do we get the body to typecheck against
          the parameter?

        solution:
          inject the parameter into the environment while
          we typecheck the body
      */
      bind(lambda->u.entity.u.literal.u.proc.def.arg.id, lambda->u.entity.u.literal.u.proc.def.arg.type, env);
      Ast* type2 = type_of(lambda->u.entity.u.literal.u.proc.def.body, env);
      unbind(lambda->u.entity.u.literal.u.proc.def.arg.id, env);
      if (type2 != NULL)
        return CreateAstEntityTypeProc(type1, type2, NULL);
        else {
          printf("lambda body not typeable!\n");
          return NULL;
        }
    }
    else {
      printf("lambda arg not typeable!\n");
      return NULL;
    }
  }
  else {
    printf("lambda NULL!\n");
    return NULL;
  }
}


Ast* HasInstance(ProcSet* set, Ast* type, Symboltable* env)
{

  /*
    either returns the procedure associated with the passed argument type.
    or returns NULL if there is no procedure associated with the argument type
    or if the procedure is polymorphic, we construct a monomorphic version
    and then typecheck that, we insert one copy of the monomorphic procedure
    into the ProcSet and return another copy as the result instance.


  */
  ProcInst* cur = set->set;
  if (set->polymorphic == true) {
    /*
      if the Set represents a polymorphic procedure
      then the defining occurance does not contain
      a typecheckable version of the procedure, (the typehecking
      algorithm deliberately does not typecheck the unbound
      version of the procedure, because it is not typeable.
      we cannot determine a type for precisely the same reason
      the unannotated version of system F cannot determine a type,
      the polymorphic procedure is by it's essence non-deterministic.)
      so we can only search the set of procedures which are instances
      or specializations of the polymorphic version,
      with the type bound to some given type, which is unique to the set.
      if we do not find the instance with a matching
      type, then we are free to construct a monomorphic instance
      with the given type. if the resulting instance typechecks
      then it's a valid instance.
      if the resulting instance fails to typecheck then
      we have no choice but to report an error.
      just like if we encounter a misuse of a monomorphic type in
      a monomorphic procedure. we have encountered the misuse
      of a monomorphic type within a polymorphic procedure.
    */
    while (cur != NULL) {
      if (typesEqual(cur->def.arg.type, type, env)) {
        return CreateAstEntityLiteralProc(strdup(cur->def.arg.id),    \
                                          CopyAst(cur->def.arg.type), \
                                          CopyAst(cur->def.body), NULL);
      }

      cur = cur->next;
    }

    // if we are here there wasn't already a valid monomorphic instance.
    ProcInst* inst     = (ProcInst*)malloc(sizeof(ProcInst));
    inst->next         = NULL;
    inst->def.arg.id   = strdup(set->def.arg.id);
    inst->def.arg.type = CopyAst(type);
    inst->def.body     = CopyAst(set->def.body);
    /*
    now we typecheck the new instance, which according to the
    procedure above, we already have type1, it is type.
    type2 is what we will use to tell if this is a valid instance.
    we need to temporarily bind the argument to the new type in the
    environment just long enough to make the judgment.
    then, if we constructed some valid type2,
    the body of the procedure is typeable if the argument has the
    passed type.
    */
    bind(inst->def.arg.id, inst->def.arg.type, env);
    Ast* T = type_of(inst->def.body, env);
    unbind(inst->def.arg.id, env);

    if (T != NULL) {
      /*
      insert the new instance into the set of procedures.
      return an evaluatable copy of the instance.
      */
      inst->next = set->set;
      set->set   = inst;
      return CreateAstEntityLiteralProc(strdup(inst->def.arg.id),    \
                                        CopyAst(inst->def.arg.type), \
                                        CopyAst(inst->def.body), NULL);
    }
    else {
      printf("polymorphic procedure not typeable with actual type [%s]", AstToString(type));
      free     (inst->def.arg.id);
      DeleteAst(inst->def.arg.type);
      DeleteAst(inst->def.body);
      free     (inst);
      return NULL;
    }
  }
  else {
    /*
      if the procedure is not polymorphic, then we cannot instanciate
      any new versions, and the actual argument type must appear as the
      formal argument type, or as the formal argument type of one of the overload set.
      otherwise this expression is not semantically meaningful.
    */
    if (typesEqual(set->def.arg.type, type, env)) {
      /*
        the defining occurance of the procedure
        has the matching type.
      */
      return CreateAstEntityLiteralProc(strdup(set->def.arg.id),    \
                                        CopyAst(set->def.arg.type), \
                                        CopyAst(set->def.body), NULL);
    }
    else while (cur != NULL) {
      // search the list of definitions and return a copy of the matching
      // one.
      if (typesEqual(cur->def.arg.type, type, env)) {
        return CreateAstEntityLiteralProc(strdup(cur->def.arg.id),    \
                                          CopyAst(cur->def.arg.type), \
                                          CopyAst(cur->def.body), NULL);
      }

      cur = cur->next;
    }

    // if we get here, there wasn't any valid procedure to call
    // given the type, so we report an error.
    printf("Passed type [%s] doesn't match any valid formal type", AstToString(type));
    return NULL;
  }
}

Ast* typeofCall(Ast* call, Symboltable* env)
{
  /*
  ENV |- term1 : type1 -> type2, term2 : type1
  --------------------------------------------
          ENV |- term1 term2 : type2
  */
  if (call != NULL) {
    Ast* term1 = call->u.call.lhs;
    Ast* typeA = type_of(term1, env);

    if (typeA != NULL) {
      Ast* term2 = call->u.call.rhs;
      Ast* typeB = type_of(term2, env);

      if (typeB != NULL) {
        if (typeA->u.entity.u.type.tag == T_PROC) {
              /*
                given some polymorphic function application,
                we simply do not have enough information
                without instanciating a version of the function
                with the type substituted in, which we can do here
                iff the call tree has the lhs node as the polymorphic
                function directly, if the function is specified by
                name, we would need to be able to evaluate the name
                to get the body, to be able to then typecheck that.
                so we do not have enough information in the general
                case to be able to typecheck a polymorphic procedure
                application. however, if we have a valid ProcInst
                in the procedure currently being typechecked,
                we then do actually have enough information
                to for real typecheck the call expression.
                so when we typecheck a call, we have to search the
                ProcSet for a potential matching argument type.
                if we find a matching argument that is actually exactly
                enough information to make the typing judgement then
                and there, if we do not find any overload, but the function
                itself is polymorphic then we have the freedom to
                create an instance of the function with the given
                type, and check to see if we can type the function
                given that type. if we can type the resulting function
                we store it into the ProcSet and return the resulting
                type.

                we want to typecheck as much as possible, but we cannot
                invent information or alignment from nowhere.
                especially when working within structure that
                already has logical alignment and existance.
                so we have to abort out of typechecking a polymorphic
                function definition. however, we still want to
                ensure the soundness of the program, so we wait
                to typecheck polymorphic procedures until application
                time, when we are given a value/type to pass to the procedure.
                so, when we typecheck that procedure we
                compare the formal argument type against the actual
                argument type, and if they match we can type this call
                as the result type. (which is called typeB here.
                and explicitly refers to the return type of
                the function literal.)
              */

              /*
                we can't factor out this check for polymorphism,
                because it is not semantically redundant to the
                underlying check for polymorphism. (within HasInstance)
                here we are merely dealing with types, and we do
                not evaluate the returned instance,
                whereas when the evaluator calls HasInstance,
                it means to evaluate the result immediately.
                HasInstance checks for polymorphism to because
                the algorithms for a polymorphic and monomorphic procedure
                are materially different. if we observe a polymorphic
                procedure within the typechecking algortithm we
                must avoid actual typechecking, because it doesn't really
                make sense to type a polymorphic version of the procedure.
                any arrangement of terms can be valid given the right context
                it is only when provided with some type that we can judge
                if that type is valid within the body of a term.
              */
              if (term1->u.entity.u.literal.u.proc.polymorphic == true) {
                DeleteAst(typeA);
                return typeB;
              } else {
                Ast* Inst = HasInstance(&(term1->u.entity.u.literal.u.proc), typeB, env);

                if (Inst == NULL) {
                  /*
                    error: proc set doesn't contain a proc with a matching
                           argument type. we can't type this expression.
                  */
                  DeleteAst(typeA);
                  DeleteAst(typeB);
                  printf("term1 doesn't have a member whose formal argument type matches the passed type: \"%s\"", AstToString(typeB));
                  return NULL;
                } else {
                  DeleteAst(Inst);
                  DeleteAst(typeA);
                  return typeB;
                }
              }
        }
        else {
          DeleteAst(typeA);
          DeleteAst(typeB);
          printf("term1 doesn't have function type! has type \"%s\"\n", AstToString(typeA));
          return NULL;
        }
      }
      else {
        DeleteAst(typeA);
        printf("term2 not typeable!\n");
        return NULL;
      }
    }
    else {
      printf("term1 not typeable!\n");
      return NULL;
    }
  }
  else {
    printf("call NULL!\n");
    return NULL;
  }
}

Ast* typeofBind(Ast* bind, Symboltable* env)
{
  /*
        ENV |- term2 : type2
    ---------------------------------
      ENV |- id := term2 : Nil
  */
  if (bind != NULL) {
    Ast* type2 = type_of(bind->u.bind.term, env);
    if (type2 != NULL) {
      return CreateAstEntityTypeNil(NULL);
    }
    else {
      printf("bind term not typeable!\n");
      return NULL;
    }
  }
  else {
    printf("bind NULL!\n");
    return NULL;
  }
}

/*
  some type t1 can only be equal to
  some type t2 if they are both
  nil, both infer, or they have the
  same types T1, T2 in the type T1 -> T2
  if they do not have the same type tag,
  they are not equal, and function types,
  need to compare recursively.

  this is essentially the inductive definition
  of equal types.
*/
bool typesEqual(Ast* t1, Ast* t2, Symboltable* env)
{
  if (t1 != NULL && t2 != NULL) {
    if  (t1->tag != N_ENTITY         \
      || t1->u.entity.tag != E_TYPE  \
      || t2->tag != N_ENTITY        \
      || t2->u.entity.tag != E_TYPE)
      error_abort("non-type ast cannot compare to type ast! aborting", __FILE__, __LINE__);
    if (t1->u.entity.u.type.tag == T_NIL && t2->u.entity.u.type.tag == T_NIL)
      return true;
    else if (t1->u.entity.u.type.tag == T_PROC && t2->u.entity.u.type.tag == T_PROC)
      return typesEqual(t1->u.entity.u.type.u.proc.lhs, t2->u.entity.u.type.u.proc.lhs, env) \
          && typesEqual(t1->u.entity.u.type.u.proc.rhs, t2->u.entity.u.type.u.proc.rhs, env);
    else
      return false;
  }
  error_abort("malformed type! aborting", __FILE__, __LINE__);
  return false;
}


/*
typeability

      # the type of nil is defined to be Nil
            ENV |- nil : Nil

      typeof(Ast => nil) -> Nil

      # if we can find a binding with some id
      # we can extract it's type and value.
             id is-in FV(ENV)
            ------------------
          ENV |- id : type = value

      typeof(Ast => id s) -> search(ENV, s).type

      # (recall that each term that has a type
      # is well typed, not being able to assign
      # a term some type is the error which will
      # be leveraged to enforce the motive behind
      # the type system, namely that every well
      # typed program has some valid semantics)

      # if we can find a type of term2 and
      # the id is not already bound in the ENV
      # we can say that the type of the binding
      # expression is the type of the term.
      # this enforces the uniqueness constraint
      # on identifiers.
      # it is implied that the value that the expression
      # is representing is also bound to the identifier.
      # this also means that whatever value the expression
      # is taking must be in the range of values that the
      # type can assume.
        ENV |- term2 : type2, id is-not-in FV(ENV)
          ---------------------------------
            ENV |- id := term2 : type2

      typeof(Ast => bind id term) -> if (search(ENV, id) == nil)
                                     then typeof(term);

      # the type of a function is type -> type
      # so the type of a particular function
      # is the type of it's argument type1,
      # arrow the type of its body type2
      # type1 -> type2
          ENV |- id : type1, term : type2
          --------------------------------
      ENV |- \ id : type1 => term : type1 -> type2

      typeof(Ast => lambda id type term) -> if (typeable(type) and (typeable(term)))
                                            then (type -> typeof(term))


      # if term1 has type type1 -> type2
      # and term 2 has type1,
      # then the type of the whole term is type2
      # if we bind some term to the value, we
      # will be binding a type2, because that is
      # the result/replacement type of the function.
    ENV |- term1 : type1 -> type2, term2 : type1
    --------------------------------------------
            ENV |- term1 term2 : type2

      typeof(Ast => call term1 term2) -> typeA = typeof(term1);
                                         typeB = typeof(term2);
                                         if (typeA = (type1 -> type2)) and typeB = type1)
                                         then type2
*/
