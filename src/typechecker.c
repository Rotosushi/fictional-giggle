/*
type system:
the type system of a programming language is
it's enforcement of type rules. mainly type systems are
concerned with:

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
    if type1 == nil && type2 == nil
    then they-are-equivalent
    else we need to recursively check that
         the function types are equal.


type compatibility:
  	when can we say that a particular usage of a type
  	is valid?

    this is where the typeing rules come in,
    we build up from equivalence, and recursively
    build up typing judgements by judging each
    smaller part of the syntax tree, then reducing
    these judgements while walking out of the tree.
    essentially
      in-order-traverse(ast, typecheck);

type inference:
  	how do we deduce the type of an expression?
  	from it's contents and surrounding context.



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
	of a single machine word. so structurally speaking
	these two types are equivalent. simply because
	to cast between the two one only needs to treat
	the machine word as the different type.
	if we consider the case of int -> real we can garuntee the
	cast will not change the state of the underlying representation.
	(we may still destroyinformation however,
	if we were to cast from real -> int for instance.)
	this is in fact the case in a language such as C,
	where you can castany structurally equivalent type to any other.

	a definition of structural equivalence will need to apply
	not only to every primitive type in the language, but must
	also extend to the types which we can define in the language.
	we must consider what it means to be structurally equivalent
	for algebraic data types, arrays, tuples, and the data primitives.
	when is a sum equivalent to another sum:

	when is a product equivalent to another product:

	when is an array equivalent to another array:

	when is a tuple equivalent to another tuple:


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
    when the program makes a call providing the particular
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
  	in this case to body of a polymorphic function only relies on
  	a certain aspect of any given type, so any type which has that
  	aspect can be correctly passed through the polymorphic parameter.
  	this kind of polymorphism is generally implemented at compile time.
  	where the polymorphic definition acts as a template for function
  	definitions, and a static definition for the function can be defined
  	for every type that is used with that function.

Subtype polymorphism: (pretty sure the literature denotes it with "<:" )
  	whereby the code works with values of some base/root type, and new
  	types can be defined which extend the semantics of the original type.
  	the subtypes provide the same interface and interface semantics as
    the root/base type so a function which only relies on the semantics
    provided by the root/base can accept as an argument any subytype of
    the root/base type.
  	can consistently rely on that/those aspects of the type.


    these three taken together, (or two if we define ad-hoc polymorphism
    in terms of parametric polymorphism.)
    make for a very expressive and typesafe language.
    the other goodies that i would like to bring in is
    parametric type definitions and with that polymorphic
    type definitions, though i think the literature hinted
    that those can be defined in terms of polymorphism as well!
    that is parametrized sum and product type constructors.




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
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>

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



Ast* typeofEntityType(Ast* type, symboltable* env);
Ast* typeofEntityLambda(Ast* lambda, symboltable* env);

Ast* typeofId(Ast* id, symboltable* env);
Ast* typeofEntity(Ast* entity, symboltable* env);
Ast* typeofCall(Ast* call, symboltable* env);
Ast* typeofBind(Ast* bind, symboltable* env);

/*
  typeof will recur equal to the depth of the Ast passed

  the typeof some term is defined by the types of
  it's subterms, until we reach the base case
  which is either nil or some type -> type;
*/
Ast* type_of(Ast* term, symboltable* env)
{
  if (term != NULL) {
      switch(term->tag) {
        case N_ID:     return typeofId(term, env);
        case N_ENTITY: return typeofEntity(term, env);
        case N_CALL:   return typeofCall(term, env);
        case N_BIND:   return typeofBind(term, env);
        default:  error_abort("malformed Ast node! aborting");
      }
  }
  else {
    printf("term NULL!\n");
    return NULL;
  }
}


Ast* typeofId(Ast* id, symboltable* env)
{
  /*
        id is-in FV(ENV)
      ------------------
    ENV |- id : type = value
  */
  if (id != NULL) {
    char* name = id->u.id.s;
    symbol* sym = lookup(name, env);
    if (sym != NULL)
      return type_of(sym->term, env);
      else {
        printf("Id is not typeable, Id <%s> not in ENV!\n", name);
        return NULL;
      }
  }
  else {
    printf("Id NULL!");
    return NULL;
  }
}

Ast* typeofEntity(Ast* type, symboltable* env)
{
  if (type != NULL) {
    switch (type->u.entity.tag) {
      case E_TYPE:   return typeofEntityType(type, env);
      case E_LAMBDA: return typeofEntityLambda(type, env);
      default:
        error_abort("malformed entity tag! aborting");
    }
  }
  return NULL;
}


Ast* typeofEntityType(Ast* type, symboltable* env)
{
  if (type != NULL) {
    /* ENV |- nil : Nil */
    if (type->u.entity.u.type.tag == T_NIL) {
      return CreateAstEntityTypeNil();
    }
    /*
     build up the function type recursively.
      ENV |- :T1, |- :T2
      ------------------
      ENV |- T1 -> T2
    */
     else if (type->u.entity.u.type.tag == T_LAMBDA) {
      Ast* t1 = typeofEntity(type->u.entity.u.type.u.rarrow.lhs, env);
      if (t1 != NULL) {
        Ast* t2 = typeofEntity(type->u.entity.u.type.u.rarrow.rhs, env);
        if (t2 != NULL) {
          return CreateAstEntityTypeFn(t1, t2);
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
      error_abort("malformed type tag! aborting");

  }
  else {
    printf("type NULL!\n");
    return NULL;
  }
  return NULL;
}

Ast* typeofEntityLambda(Ast* lambda, symboltable* env)
{
  /*
        ENV |- id : type1, term : type2
        --------------------------------
    ENV |- \ id : type1 => term : type1 -> type2
  */
  if (lambda != NULL) {
    Ast* type1 = type_of(lambda->u.entity.u.lambda.arg.type, env);
    if (type1 != NULL) {
      /*
        question:
          how do we get the body to typecheck against
          the parameter?

        solution:
          inject the parameter into the environment while
          we typecheck the body
      */
      bind(lambda->u.entity.u.lambda.arg.id.s, lambda->u.entity.u.lambda.arg.type, env);
      Ast* type2 = type_of(lambda->u.entity.u.lambda.body, env);
      unbind(lambda->u.entity.u.lambda.arg.id.s, env);
      if (type2 != NULL)
        return CreateAstEntityTypeFn(type1, type2);
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

Ast* typeofCall(Ast* call, symboltable* env)
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

        if (typeA->u.entity.u.type.tag == T_LAMBDA) {
            Ast* type1 = type_of(typeA->u.entity.u.type.u.rarrow.lhs, env);
            // Ast* type2 = type_of(typeA->u.type.u.rarrow.rhs)

            if (type1 != NULL) {

              if (typesEqual(type1, typeB, env))
                return typeB;
              else {
                printf("term2's type doesn't equal term1's arg type!\n");
                return NULL;
              }
            }
            else {
              printf("term1's type1 is not typeabel!\n");
              return NULL;
            }
        }
        else {
          printf("term1 doesn't have Function Type!\n");
          return NULL;
        }
      }
      else {
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

Ast* typeofBind(Ast* bind, symboltable* env)
{
  /*
        ENV |- term2 : type2
    ---------------------------------
      ENV |- id := term2 : Nil
  */
  if (bind != NULL) {
    Ast* type2 = type_of(bind->u.bind.term, env);
    if (type2 != NULL) {
      return CreateAstEntityTypeNil();
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
bool typesEqual(Ast* t1, Ast* t2, symboltable* env)
{
  if (t1 != NULL && t2 != NULL) {
    if  (t1->tag != N_ENTITY         \
      || t1->u.entity.tag != E_TYPE  \
      || t2->tag != N_ENTITY        \
      || t2->u.entity.tag != E_TYPE)
      error_abort("non-type ast cannot compare to type ast! aborting");
    if (t1->u.entity.u.type.tag == T_NIL && t2->u.entity.u.type.tag == T_NIL)
      return true;
    else if (t1->u.entity.u.type.tag == T_LAMBDA && t2->u.entity.u.type.tag == T_LAMBDA)
      return typesEqual(t1->u.entity.u.type.u.rarrow.lhs, t2->u.entity.u.type.u.rarrow.lhs, env) \
          && typesEqual(t1->u.entity.u.type.u.rarrow.rhs, t2->u.entity.u.type.u.rarrow.rhs, env);
    else
      return false;
  }
  error_abort("malformed type! aborting");
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
