
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "evaluator.h"
#include "ast.h"
#include "symboltable.h"
#include "error.h"

Ast* evaluate_type(Ast* type, symboltable* env);
Ast* evaluate_lambda(Ast* lambda, symboltable* env);

Ast* evaluate_id(Ast* id, symboltable* env);
Ast* evaluate_entity(Ast* val, symboltable* env);
Ast* evaluate_call(Ast* call, symboltable* env);
Ast* evaluate_bind(Ast* bind, symboltable* env);

void substitute(char* name, Ast** term, Ast* value, symboltable* env);
bool appears_free_in(char* name, Ast* term);
void rename_binding(Ast* lambda, Ast* value);


/*
  evaluate allocates new memory to describe the result of
  evaluation, instead of modifying the tree in place, as
  the latter stategy is harder w.r.t. avoiding introducing
  pointer cycles into the tree structure, which cause
  DeleteAst to seg-fault. instead, each function maintains
  it's own memory during evaluation, allocating
  and deallocating intermediate results, and subsequently
  returning the final result tree.
*/
Ast* evaluate(Ast* term, symboltable* env)
{
  /* dynamic type dispatch! */
  switch(term->tag) {
    case N_ID:     return evaluate_id(term, env);    /* return the bound term. */
    case N_ENTITY: return evaluate_entity(term, env); /* values are already in beta-normal form */
    case N_CALL:   return evaluate_call(term, env);  /* return the result of calling the fn. */
    case N_BIND:   return evaluate_bind(term, env);  /* evaluate the bind and return the value nil. */
    default: error_abort("malformed ast node tag! aborting");
  }
  return NULL;
}

Ast* evaluate_entity(Ast* entity, symboltable* env)
{
  /* values are what we use to evaluate
      they are not themselves evaluated.
      intuitively;
      we don't evaluate '3' we evaluate
      '3' + '4'. we dont evaluate
      \x=>x we evaluate (\x=>x)nil
      the former are beta-normal forms,
      the latter are reducable expressions.
  */
  return CopyAst(entity);
}

Ast* evaluate_id(Ast* id, symboltable* env)
{
  /*
   in order to evalute an id,
   we need to discover what entity
   it is bound to. this is done by
   looking up the symbol in the Environment
   and returning the bound term as the result.
   if we cannot find the binding in the environment,
   then the term is not typeable.

   ENV |- id : type = value
   -------------------------
       id -> value
  */
  if (id != NULL) {
    symbol* sym = lookup(id->u.id.s, env);
    if (sym != NULL) {
      return CopyAst(sym->term);
    }
    else {
      error_abort("cannot find name in environment! aborting");
    }
  }
  else {
    error_abort("cannot evaluate/lookup NULL id!");
  }
  // suppressing a warning...
  // id either is or isn't NULL, so why does clang warn here?
  // clang can't know that error_abort calls exit?
  return NULL;
}


Ast* evaluate_call(Ast* call, symboltable* env)
{
  /*
  in order to evaluate a call expression, we want to
  return the result of substituting
  the rhs throughout the body of the lhs for the bound name.


            term1 -> term1'
        ----------------------
      term1 term2 -> term1' term2

            term2 -> term2'
        ----------------------
      value1 term2 -> value1 term2'

      (\ id : type = term) (value2) -> [id -> value2]term
  */
  if (call != NULL) {
    /*
      Ast* lhs = (*call)->u.call.lhs;
      Ast* rhs = (*call)->u.call.rhs;
    */
    /*
      evaluate the lhs down to a lambda value
      ensuring that we only ever manipulate copies of
      the passed in structure.
    */
    Ast* tmp = NULL;
    Ast* lhs = CopyAst(call->u.call.lhs);
    while(lhs->tag != N_ENTITY) {
      tmp = lhs;
      lhs = evaluate(lhs, env);
      DeleteAst(tmp);
    }

    if (lhs->u.entity.tag != E_LAMBDA)
      error_abort("cannot call a non-lambda term! aborting");

    // evaluate the rhs down to a value.
    Ast* rhs = CopyAst(call->u.call.rhs);
    while(rhs->tag != N_ENTITY) {
      tmp = rhs;
      rhs = evaluate(rhs, env);
      DeleteAst(tmp);
    }
    /* we return a copy in case substitute needs to
       rename a subterm in order to execute the
       substitution, in order to avoid modifying the
       original term.
    */
    tmp = CopyAst(lhs->u.entity.u.lambda.body);
    substitute(lhs->u.entity.u.lambda.arg.id.s, \
               &tmp,                   \
               rhs,                    \
               env);
    DeleteAst(lhs);
    DeleteAst(rhs);
    return tmp;

  }
  else {
    error_abort("canot evaluate NULL call! aborting");
  }

  return NULL;
}

Ast* evaluate_bind(Ast* bind_ast, symboltable* env)
{
  /*
                        term -> term'
                  ---------------------------
                  id := term -> id := term'

       ENV |- term : type = value, id is-not-in FV(ENV)
        ----------------------------------------------
      id := value -> bind (id, (type, value)), ENV) : Nil
  */
  if (bind_ast != NULL) {

    Ast *term = bind_ast->u.bind.term, *tmp = NULL;
    while (bind_ast->u.bind.term->tag != N_ENTITY) {
      tmp = term;
      term = evaluate(term, env);
      DeleteAst(tmp);
    }

    bind (bind_ast->u.bind.id.s, bind_ast->u.bind.term, env);
    return CreateAstEntityTypeNil();

  }
  else {
    error_abort("cannot evaluate NULL bind");
  }
  return NULL;
}

void substitute(char* name, Ast** term, Ast* value, symboltable* env)
{
  if (name == NULL || term == NULL || *term == NULL || value == NULL)
    return;
  /* substitute {name} for {value} within {term} */
  switch((*term)->tag) {
    case N_ID: {
      /*
      {term} is a node containing the name we are looking for
      so we can replace {term} with {value}
      [id -> value2]id  := value2
      [id -> value2]id' := id'

      when term is a two star pointer it makes
      semantic sense to "replace" the term with the value.
      we can quite literally delete the old data and attatch
      a copy of the value.
      */
      if (strcmp(name, (*term)->u.id.s) == 0) {
        DeleteAst((*term));
        (*term) = CopyAst(value);
      }
    }

    case N_ENTITY: {
      if ((*term)->u.entity.tag == E_TYPE) {
        /*
          this is where typename substitution will happen.
        */
      }
      else if ((*term)->u.entity.tag == E_LAMBDA) {
        /*
        [id -> value2](\ id : type = term)  := (\ id : type = term)
        [id -> value2](\ id' : type = term) :=
          if (id' is-not-in(value2))
          then (\id' : type = [id ->value2]term)
          else (\[id' -> id'']id' : type = [id -> value2][id' -> id'']term)

          {term} is another lambda;
          it would be nice if we could simply pass the substitution
          operation into the body of the lambda, but there are two
          contravening cases. the argument of the lambda matches
          the name we are replacing for, or the argument of the lambda
          matches a free variable in the value we substituting.

          if the binding created by the lambda is the same
          as the binding we are trying to replace, then we
          do not substitute within the body of that abstraction.
          as the name we are looking for is already bound within
          the abstraction, and substitution would introduce an unintentional
          binding.
        */
        if (strcmp(name, (*term)->u.entity.u.lambda.arg.id.s) == 0) {
          return;
        }
        else {
          /*
           the binding does not match,
            however we need to aware of one more case,
            if a free variable in the body of the value
            we are substituting for happens to match the binding
            of the lambda, an unintentional binding will occur.
            (think through what happens when we call the function
             'lookup' on a free variable which occurs in the value
             being substituted into the body of a lambda, after
             execution of the substitution whose name matches the
             lambdas binding name;
             when lookup tries to resolve the name, it will encounter
             the definition of the argument binding before
             it will encounter the binding that existed at the
             time of {value}'s creation. hence, the unintentional
             binding.
             the generally accepted solutions to this problem are
             -renaming the argument of the lambda before substitution.
                such that no binding will occur.
             -making it so the evaluation of terms doesn't depend
              on the typed names. this can be achieved in two ways
                -De Bruijn notation
                -Combinator Calculus.

            because of the simplicity of implementation
            we will choose to rename the binding of
            the lamdba abstraction throughout it's body before
            we substitute for the value.

            this has two downsides pointed out by some literature
            -it can be hard to reason about correctness formally
            -it relies on string manipulation, which is less efficient
              than the integer manipulation that De Bruijn notation
              relies upon.

            we don't need to reason formally about the correctness
            of Pink, if we can test it's correctness.

            string manipulation is something that I am willing to
            spend cycles on. especially early on in the languages lifetime.

            I can see an argument for conversion to a DeBruijn manipulation
            of terms for a more final form of the interpreter.
            along with linearizing (i mean moving from using a naturally
            recursive style, to a tail-recursive style) the tree traversal algorithm,
            and separating the walking from the actions. as that would
            have an impact on every module of the code that can be written
            as an operation upon the tree. Which is a large portion of this program.





            how do we know if the name bound to the lambda occurs free
            in {value}? well,
            a possible solution would be to gather the free
            variables present in (value) into a list and search that list
            for the argument name. this is in fact the solution that imo
            most straghtforwardly flows from the formal definition of substitution
            and renaming.
            however, this would require at least
            as much work as searching the tree directly
            to build the list of free variables, plus
            the work required to then search the list.
            (not to mention all of the allocation and deallocation
            of the list itself.)
            it is more efficient to directly search the subtree
            for free variables of the conficting name. but this
            becomes another place where the algorithm is tied intimately
            to the structure of the Ast.

          */

          if (appears_free_in((*term)->u.entity.u.lambda.arg.id.s, value)) {
            Ast* lambda = CopyAst(*term);
            rename_binding(lambda, value);
            DeleteAst((*term));
            (*term) = lambda;
          }

          /*
          when the term is a two star ptr it makes direct semantic sense
          to simply apply substitution to the body of the lambda
          that is occupying term. the term itself remains what it was before
          and the substitution operation is free to modify deeper in the
          tree.
          */
          substitute(name, &((*term)->u.entity.u.lambda.body), value, env);
        }
        break;
      }
      else {
        error_abort("malformed entity tag! aborting");
      }
    }

    case N_CALL: {
      /*
        [id -> value2]lhs rhs := [id -> value2]lhs [id -> value2]rhs
        when we encounter a call Ast we pass the substitution
        along to it's subterms. and we do not modify the call
        term itself. (no names can appear inside of the call term)
        names can only appear as leafs from this node.
      */
      substitute(name, &((*term)->u.call.lhs), value, env);
      substitute(name, &((*term)->u.call.rhs), value, env);
      break;
    }

    case N_BIND: {
      /*
        [id -> value2]id' := term  :=  id' := [id -> value2]term

        when we encounter a bind we pass the substitution
        along to it's subterm, instances of the bound variable
        occuring in the subterm will be replaced with the value.
        what happens when the bind operation is binding the name
        we are substituting for? well that's a syntax error,
        as that name is already bound in the environment.
        that's how we are in an operation where we
        are replacing a name for some term, the name
        appears bound in some lambda above this term.
        so it shouldn't happen right?
      */
      substitute(name, &((*term)->u.bind.term), value, env);
      break;
    }
  }
}
      /*
       are type descriptors values?
       i argue yes. simply because then it becomes easy to
       compose types using named bindings, and expressions,
       instead of having a separate type naming process,
       like 'typedef', or 'type', etc. it makes more sense
       to consider there being a single unified 'naming'
       or 'binding' process, which is used to implement the
       binding of language entities to names so that
       those entities can then be refrenced again in the text
       of the program, it then makes no difference which entity
       is being bound, the binding only needs to consider
       that there is an entity being bound. the entity could have
       existance, like a local variable, or the entity
       could be a description of other entities physical
       representation, like a type.
       If types become 'first-class' then suddenly the mechanisms
       for describing new types become the same as describing
       new values and new kinds of entity. Operations on
       values of type, cannot be instanciated or manioulated at runtime.
       in most implementations, types are bound by special names
       which can bind types only and a special form of abstraction
       is used to bind type-variables to type-values, within the body
       of regular lambdas.
       essentially the language definition is repeated
       but only for types; this can be avoided if instead
       of 'lifting' the semantics to meet the type, we 'push'
       the type down to meet the semantics already present.
       following this, my observation
       is that we already use types to distinguish
       entities in our language, and we can select between
       operations based on the type of the arguments.
       so, if we consider a type to be something a name can
       be bound to (i.e. an entity) then, we can use the
       usual name binding and substitution mechanisms to
       define what is known as first order polymorphism.

       so, if we reimagine type operators as
       operators taking types as arguments, where the
       value of a type is it's description of that type.
       then type expressions can literally be just that,
       an expression composed of type entities in the
       exact same way as we conceive of an expression upon
       integer entities, or upon composite entities
       which have operators defined (overloaded) for them.
       instead we leverage the already existant
       binding operation, and type expressions
       are now just another kind of expression.
       operators on type describing new types
       work the same as an operator on value describing
       new values, because types are entities, just like values
       are entities, but types are not themselves values in the
       traditional sense. for instance we can not create an instance
       of a type, that makes no sense, as the type is a description of
       the encoding of some value. instead we can
       create instances of values according-to/of some type.

       (it makes some sense to say "i have some
       encoding(type), and I can create a new copy of the encoding,
       and apply that encoding elsewhere.
       however since the base types themselves are always constants
       we never need to create copies, we can just use pointers
       with no downsides, each can simply refer to the base types.)

       the difference between type entities and other entities which are
       useful to name in the program, is that
       essentially types are only useful to the compiler and the programmer,
       not the machine, so the runtime doesn't have a sense of type.
       there may be checks of tags, but that is a particular implementation
       of runtime type, there is no intrisic sense of type in the actual
       instructions that execute. whereas integers are a thing that the
       all three (programmer, compiler, machine) have a sense of, meaning
       it makes sense to have integers which exist during any stage of
       compilation. following along this thread, a type only
       really exists to communicate to the programmer and compiler,
       hence they only need to have a full manipulatable existance during
       programming and compilation.
       this doesn't tell the whole story
       however, as there are some cases in which the type needs to exist
       during the runtime, say in the case of a tagged union, the compiler
       used the type information to generate both the object in question,
       and functions which operate on objects of said type.
       however, when the code operates on some object which was created
       dynamically and needs to decide which function to call passing in
       this object, the runtime then needs to be able to distinguish
       between objects of the alternate types. this can be implemented using
       a tag for each of the possible alternates, an we can discover which
       of the alternates some particular object holds at runtime. this
       information can then be used to choose code paths during runtime.
       if we say that the runtime
       needs to have the same understanding of type that the compiler
       and programmer does, then the runtime would have a full conception
       of type and new types could presumably be created at runtime, this
       adds a lot of complexity to the runtime, so much so that it would
       seem to require that the full evaluation and typechecking facilities
       of the compiler be available during the runtime of some program. which
       would require the existance of the evaluator and typechecker
       in the executable generated by the compiler. so this seems like
       something that adds so much complexity that it is above and
       beyond what a systems language is designed for, which is describing
       programs who's job it is to interface with the hardware directly.
       so, maybe runtime types can exist as a library which can be requested,
       but the dependency is so great it more than likely outweighs any advantages.
       because, in the end, the language needs to be useful to programmers,
       and if you are writing code for an 8-bit microprocessor, you probably
       want to avoid putting in any extra data, as the storage facilities are
       highly limited compared to where the compiler is running.
       this would also introduce the temptation to optimize the compiler
       against running on restricted hardware, which is again not really
       the point of the language.

       we can deduce constraints around which features can go into
       the kernel this way.
       for instance, we now know that the full set of types that
       any particular program has knowledge of at compile time is
       strictly greater than or equal to the set of types that same
       program has conception of at runtime.
       if some future feature is proposed as an addition to the
       kernel, and the feature makes new types known to some program
       during the execution of said program, we can categorically refute
       that features inclusion into the kernel.
       (this says nothing about a library which provides this feature
        to the eager programmer.)

       the kernel of pink is trying to align with what is statically
       expressable by the runtime given any particular architecture.
       this means that the language kernel needs to avoid adding to much
       dynamic semantics to the kernel. tagged unions are fine
       because they are so useful, and they are knowable through static analysis
       alone. and even if you were going to provide
       an untagged union a-la c, it is still pragmatic to always use the
       untagged union in a tagged manner to avoid nasty runtime errors.
       this means that the programmer must implement the tag portion of the
       union themselves, while this is so basic as to be near trivial,
       it still invites human error into the equation, which just seems silly
       especially when the compiler can just provide a tagged union
       for the programmer to use. in this we can notice that problems
       never go away if they are not solved. we can do two things with
       problems, move them somewhere else, or solve them ourselves.
       (astute observers may recall a similar discussion of string
        manipulation, any way you slice it, the safe way of handling
        strings must, by it's very existance, take some overhead.
        if we ignore safe handling procedures, a'la C, we do not
        accomplish 'solving the problem' we accomplish 'making the problem
        the programmers problem and not ours', so we simply make programming
        in the language take some background knowledge in why particular
        patterns of use cause errors, which most of the time, breaks the
        facade that the language is giving you a layer of abstraction.
        (essentially; in learning why C arrays are unsafe, you are really
         learning about why assembly arrays are unsafe.))
        so, the work of checking the cell of memory containing the
        tag of the union is either the language designers problem
        or the programmers problem. or we can accept that some
        scentances in the language will both be accepted by the
        language as legal, and contain semantic errors.
        however, the more we leave to the programmer, the more
        viscosity we leave in the language. giving programmers
        a nice Api around a dynamic text object is something which
        takes the work it takes to do right and no less.
        if a programmer want's a version which eskews safety in
        some cases for speed then they should be forced to write
        it themselves, perhaps with manual management of
        dynamic arrays and character primitives.
        the case of a union is less simple, because if we must
        choose to make every union in the language tagged garuntees
        that overhead for the rest of the lifetime of the language.


        types are not
       values in the traditional sense of them being able to have
       existance, they are however a language entity which is
       useful to name for use in later parts of your program.
       we want to be able to name types for use in composing
       larger types, and we want to provide functions which
       work with more than one type, in order to provide
       polymorphic containers. (we also want to provide constraints
       around which types can be instanciated where, to allow for
        programatic type shrinking, to give programmers finer grained
        control over which types are allowed to be bound within some
        function.)
       from this perspective, types are providing a layer of indirection,
       and that indirection is being used to encode semantics.
       for Pink, the set of primitives is meant to model the
       actual hardware primitives, so that the programmer
       can 1) have some notion about the programs composition
       in the assembly, 2) the language can then lower the
       level of abstraction it works at, allowing the programmer
       to interact directly with the hardware using it's natural
       sizes. (where a c programmer want's a byte sized value
        the type 'char' is what they reach for, in Pink that
        is the purpose of the u8, or s8 primitive types. )
       we can consider '->' to literally be an operator
       that takes in two type-values T1, T2 as arguments and returns
       the type-value 'T1 -> T2'. we can consider sum and product
       types similarly, the operators being * and | usually.
       T1 + T2 ==>> {T1 + T2}
       but I see an argument for + and | respectively, as that conveys
       the summing or alternates meaning by leveraging the
       metaphorical implications of "+" adding two entities together,
       or selecting one sub-type or the other "|". this also
       free's up the symbol * which can then be used to
       signify the type of a variable that can assume a type
       expression *, or * -> *. i.e. what is the type of
       the name a after evaluation of the statement:
         a := Int ?
       Answer: type: *
       if the name 'a' subsequently appears in an expression
       where a type of the kind * was expected, we can substitute
       the bound type-value of 'Int' in place of the name 'a'
       this is semantically type-aliasing. if we imagine some
       lambda-value: (\x: * => \y: x -> x -> x => \z: x => \w: x => y z w)
       we could imagine some valid execution sequence for:
        (\x: * => \y: x -> x -> x => \z: x => \w: x => y z w) Int + 1 2
        ==>> 3
        (if we consider Int, +, 1, and 2, to have their usual meanings.)

      in fact, every binary operator could be described
      in terms of this type signature.

        what about
        (\y: x -> x -> x => \x: * => \z: x => \w: x => y z w)
        well, i argue this doesn't typecheck if we don't
        allow use-before-definition. i could accept it, if
        we treat the parameter list as a single semantic
        unit, which would allow the type binding to occur
        positionally different, in the same way that some
        languages allow you to call functions without considering
        the positions of the arguments. although then
        that will definetly throw a wrench into implementing
        multiple-dispatch. and multple-dispatch seems more useful
        in conjunction with parametric polymorphism.
        if we consider the two, we can bang out some particular
        implementation of a function with the types replaced throughout
        the body, then each location the function is called can correspond
        to the implementation with that particular type-signature. we can
        in some sense imagine a function whose parameter has no specific
        type as having whichever type some particular call site has
        specified for it. that particular call-site has (assumedly)
        supplied a parameter with some known type, which means
        that this call-site means to call the version of the
        function where the polymorphic parameters type has been
        substituted for this actual parameters type.
        in this sense the function \x=>x is not a single
        function literal, but a set of functions, each of
        which has every known type substituted in for the
        polymorphic parameter x.
        we can say something like
        (\x=>x) === { (\x:t => x) | t is-in Types_in(ENV) }

        (we assume here that Types_in(ENV) returns some collection
          usable with the boolean membership predicate 'is_in'
          such that t can select from any type which has been
          defined in the program text.
          and, i suppose when we allow definitions to appear after
          useage, the question will be
          rephrased to 't can select from any type that appears
          in the total set of source text')
        typechecking can be partially carried out during parsing
        but given mutually recursive definitions of functions,
        sometimes it is semantically convenient or syntactically
        required for some names to appear before any definition can
        be given to them.

        then when we apply the function to some argument
        (\x=>x)10
        we see that the argument to the polymorphic parameter
        has a type Int. so we can know that we want to call
        the member of the set { \x:t => x | t is-in Types_in(ENV) }
        whose type (t) is Int.

        going just one step further, if we adopt c++'s
        template function instanciation rule, which
        simply says that the compiler is not required to
        typecheck every member of the set { \x:t => x | t is-in Types_in(ENV) }
        it only has to typecheck those members which the
        text of the program requests be applied; we can
        allow more programs to typecheck, because not every
        polymorphic function needs to work when instanciated
        with every type available to the program, only the
        versions that are typed out by the programmer are
        required to typecheck.

        notice that this definition extends over the
        case of an overloaded function. if we have a function
        fn (+) a:(Int, Int), b:(Int, Int) => (fst(a) + fst(b), snd(a) + snd(b));
        and
        fn (+) a:(Real, Real), b:(Real, Real) => (fst(a) + fst(b), snd(a) + snd(b));

        these functions are both instances of the function described
        by the set
        { fn (+) a:(t, t), b:(t, t) => a + b | t is-in Types_in(ENV) }

        so, this leads me to think that there is a case for making each
        function defined by a 'type-erased' set, but in the case of
        there being no actual polymorphic parameter, we only allow the programmer
        to select between definitions whose parameters types exactly match, and there
        is no ability to generate new versions of the function.
        then, multiple dispatch can be done via accessing members of this set.


        aside:
        lambdas cannot be naturally recursive without
        a y-combinator being typeable in the language.
        (essentially, typeing the y-combinator does not
         converge, because the y-combinator itself does
         not converge. it recurs indefinetely.
        being able to type the y-combinator means adding
        some amount of lazy-execution to the language.)
        however
        i do know that the y-combinator is only truly
        necessary if you are unwilling to extend the
        calculus with a function abstraction which
        introduces a named function, and then this
        abstraction can provide the name of the function
        within the body of the function, thus allowing for
        c-style recursive functions.
        so, maybe we sidestep
        the issue a bit, and we will revist the
        y-combinator at a later point, when for
        some reason we want to have recursive lambdas.
        otherwise we could allow both styles of function literal to
        appear, and they can have separate semantics.
        additionally, recursion is used to implement looping
        in it's most base case, and Pink is already planning on
        providing a while loop to the base calculus, so
        lambdas can borrow the expressive power of
        Djikstra's three instead. and, mutual recursion is
        also available if the lambdas in question have been bound to
        names within the current environment.


       aside:
       if the compiler expects a * -> * and it is given a *
       that shortcuts to a semantic error right?
       we obviously cannot determine what behavior the syntax is
       going to request of the type before we have looked
       at the syntax, but just on type information alone,
       if the syntax expects a * -> * and is given an *.
       then my hypothesis is that that syntax will not
       typecheck. unless we can envision some type *
       what can be called like a function. but that
       does not make sense categorically, as every
       entity that can be called like a function
       must have type * -> *, as that is exactly
       what it means to be callable like a function.

      */








/*
compares (name) against all of the free variables
within (term) until either a free variable matches
(name), or all free variables have been searched.
then returning true or false respectively.

notice how there is essentially three kinds of
case when working with the AST, either
  - the node contains information we are interested
     in processing
  - the node contains more nodes to process
  - both.

so in any node we are either inspecting the nodes
data to do something with it, and-then/or, we
inspect to find more nodes to process.
if we observe the rules in conjuction with
the code, places where we observe some
(t -> t') correspond to places where
the node contains both a pointer to some other
node and we want to process that node, then
look to see if we can still process that node.
if we observe (ENV |- x : T) that corresponds
to conditions on the execution of the full judgement.
this particular string of characters would imply
that given the execution environment we should
be able to assign some type T to the variable name x.
in the context of the evaluator it shouldn't
matter which particular type, nor which particular name.
what matters is that the type can be constructed/observed
for the name, as when a term is typeable we gain certain
assurances about the semantics of the term, namely that
it will not generate runtime errors to the best of the
knowledge of the typechecker.

also notice:
  - each time we have an algorithm where we want
    to preform actions on nodes, there is a single
    entry point which preforms dispatch, single
    functions which preform actions when passed
    some node, each function only working on
    a single 'kind' of node. in the cases of
    boolean predicates it is usually enough to
    have the dispatch function also contain the
    algorithm, whereas with typechecking,
    breaking the algorithm into peices allows for
    some parts of the recursive algorithm to express
    different semantics, such as when type_of
    is typechecking a type expression it doesn't
    utilize the full dispatch, which in turn
    makes a spurrious non-type node appearing in
    a type expression into a semantic error.
    without writing the semantic error explicitly,
    just by making type dispatch smaller than
    the full dispatch function type-wise.
    (notice how the smaller-nesscorresponds
     to the subtyping relationship between the full Ast
     and the part of the Ast describing Types.
     it becomes a function which only operates on
     a particular subtype of the Ast instead of
     every subtype of the Ast.

     maybe there is a way to express the dispatch
     via overloading, then each module like the
     typechecker, and evaluator can provide a
     function to call against each type of node
     and when a rule calls the function passing in
     it's node, the compiler picks which function
     to call based on type.
    )
 */
bool appears_free_in(char* name, Ast* term)
{
  switch(term->tag) {


      case N_ID: {
        // name might appear in ID, better check.
        if (strcmp(name, term->u.id.s) == 0) {
          return true;
        } else {
          return false;
        }
      }

      case N_ENTITY: {
          if (term->u.entity.tag == E_TYPE) {
            // names cannot appear in types, yet...
            return false;
          }
          else if (term->u.entity.tag == E_LAMBDA) {
          if (strcmp(name, term->u.entity.u.lambda.arg.id.s) == 0) {
            // the name is bound by the lambda so
            // if the name appears in the body of the
            // lambda it will be associated with the
            // parameter binding.
            // i.e. the name appears bound in term, not free.
            return false;
          }
          else {
            // search the body of the lambda for instances of
            // the name.
            return (appears_free_in(name, term->u.entity.u.lambda.body));
          }
        }
        else {
          error_abort("malformed entity tag! aborting");
        }
      }

      case N_CALL: {
        return appears_free_in(name, term->u.call.lhs) \
            || appears_free_in(name, term->u.call.rhs);
      }

      case N_BIND: {
        // search the bound term for instances of the name
        // however the bound id is required to be != to
        // the name we are looking for, as the name
        // we are looking for is bound!
        // so something should have caught that
        // bug before this point, we assume.
        return appears_free_in(name, term->u.bind.term);
      }
      default: error_abort("malformed Ast node tag! aborting");
  }
}

/* this is a very simplistic version of
   renaming. it ensures
   the semantics, and should be kept separate
   from anything but evaluating a given Ast.
   we don't want to rename the bindings the user
   types in, because that would confuse the
   writer of the program.
    */
char* generate_name(int len)
{
  const char symset[]  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const int  symsetlen = 52;
  char* result = (char*)calloc(len + 1, sizeof(char));
  int i = 0;
  while (i < len) {
    result[i] = symset[rand() % symsetlen];
    i++;
  }
  return result;
}

void rename_binding_in_body(char* new_name, char* old_name, Ast* term)
{
  switch (term->tag)
  {
    case N_ID: {
      /*
        this could be the node we want to replace
      */
      if (strcmp(old_name, term->u.id.s) == 0) {
        /* we want to rename this node */
        free(term->u.id.s);
        term->u.id.s = strdup(new_name);
      }
      /* we don't want to rename this node */
    }
    case N_ENTITY: {
      /*
        if the body of the lambda we are replacing the name for
        is itself a lambda whose biding matches the name we are
        looking to replace, we do not replace, because that lambda
        is introducing that binding for it's own body, and that makes
        it a separate binding than the one we are looking for.
        if the binding is not the one that we are looking for, then
        we need to look for instances of the binding within the
        body of the lambda.
       */
      if (term->u.entity.tag == E_LAMBDA) {
        if (strcmp(old_name, term->u.entity.u.lambda.arg.id.s) == 0) {}
        else {
          rename_binding_in_body(new_name, old_name, term->u.entity.u.lambda.body);
        }
      }
      else if (term->u.entity.tag == E_TYPE) {
      /* names cannot yet appear in type expressions.
         there is no way that a name could appear in
         a type expression that we would need to replace.
       */
       return;
      }
      else {
        error_abort("malformed entity tag! aborting");
      }
    }
    case N_CALL: {
      rename_binding_in_body(new_name, old_name, term->u.call.lhs);
      rename_binding_in_body(new_name, old_name, term->u.call.rhs);
    }
    case N_BIND: {
      rename_binding_in_body(new_name, old_name, term->u.bind.term);
    }
    default: error_abort("malformed Ast node tag! aborting");
  }
}


void rename_binding(Ast* lambda, Ast* invalid_bindings)
{
  char* arg_name = strdup(lambda->u.entity.u.lambda.arg.id.s);
  char* new_name = NULL;
  do {
    if (new_name)
      free(new_name);

    new_name = generate_name(5);
  } while (appears_free_in(new_name, invalid_bindings));

  free(lambda->u.entity.u.lambda.arg.id.s);
  lambda->u.entity.u.lambda.arg.id.s = new_name;
  rename_binding_in_body(new_name, arg_name, lambda->u.entity.u.lambda.body);
  free(arg_name);
}



/*



  formally substitution is defined as:
  (\ id : type = term) (value2 : type) -> [id -> value2]term

  [id -> value2]term :=
    [id -> value2]id                    := value2
    [id -> value2]id'                   := id'
    [id -> value2]id' := term           := id' := [id -> value2]term
    [id -> value2]lhs rhs               := [id -> value2]lhs [id -> value2]rhs
    [id -> value2](\ id : type = term)  := (\ id : type = term)
    [id -> value2](\ id' : type = term) := if (id' is-not-in(value2))
                                           then \ id' : type = [id ->value2]term
                                           else (\ [id' -> id'']id' : type = [id -> value2][id' -> id'']term)

substitution is in essence the evaluation of a function call.
the language is call by value, so first the argument is evaluated
to some value, then that value is replaced for the arg name throughout
the body of the function, by substitution.
(and in terms of the Ast for interpretation,
we can apply substitution to the body, and return the resulting
body Ast structure after substitution and that can be considered
a step of evaluation. if we return something that is a value,
we will most likely end whatever outer step of the computation
brought the flow of control to this point, alternatively, if we
reduce to something that is not a value, the interpreter will
probably reduce that form.
of course, the program could result in a non-beta-normal form
however that is considered a syntax error (i think.))

Examples:

[x -> y]x
==>> y

[x -> y]y
==>> y

[x -> y]z
==>> z

[x -> y](\z => (z x) (z a))
=> [x -> y](z x) (z a)
=> [x -> y](z x) [x -> y](z a)
=> ([x -> y]z [x -> y]x) ([x -> y]z [x -> y]a)
==>> (z y) (z a)


[x -> (\z => z w)](\y => x)  // x is a free variable in (\y..., a.k.a. value2)
=> \y => [x -> (\z => z w)]x // so we can return the lambda-term resulting from
                             // substitution applied to the body term.
==>> (\y => (\z => z w))     // evaluating to...


[x -> y](\y => x)            // the value2 has a free variable (y) who is bound
                             // by the lambda term being substituted,
                             // since we do not want to modify the semantics
                             // by application of substitution, we do not apply
                             // substitution to the lambda term, and simply
                             // return it as the result of substitution.
                             // to say that another way, since the lambda abstraction
                             // binds the name, we know that the occurances of
                             // the name within the body of that abstraction
                             // are meant to refer to the binding created by the
                             // abstraction, and if we were to substitute the
                             // occurances of the free variable (y) within the
                             // body, we would be changing the meaning of the
                             // passed in function, because a free variable being
                             // looked up will find the binding created by the
                             // lambda being substituted into (the term).
                             // so we preform a alpha-conversion on the function.
=> [x -> y](\w => x)
==>> (\w => y)

[x -> y](\x => x z)
==>> (\x => x z)

[x -> (y z)](\y => x y)    // y is-in FreeVariables(value2)
=> [x -> (y z)](\w => x w) // rename the name of the binding consistently in the body (alpha-conversion)
==>> (\w => y z w)         // execute the usual substitution operation.




  which is to say, when substituting a name for some value
  within the body of a lambda term we first look at what
  kind of term it is.

  the first rule simply states that
  when we encounter the name we replace the name with
  value. in terms of our ast, when we encounter an
  Ast node id, if the id stored in the node is
  identical to the ide we are searching for we can
  replace the Ast node id with the term. if the
  id doesn't match, then it is some other bound
  or free variable and we leave it alone.

  The second rule simply states that if we encounter
  a name that isn't the name we are looking for, we do nothing.

  the third rule states that if we encounter a call term,
  we substitute for the name within the left and right hand sides.
  and leave the call node itself unevaluated, returning it.

  the fourth rule states that if the name we are replacing matches
  the name that a lambda term binds, then we do nothing. because
  the binding in that lamdba is separate that what we are replacing.
  in terms of the ast, we can return the term itself, because the
  term already represents itself.

  the last rule is the most complex, because it has to contend
  with name conflicts arising as a result of the replacement.
  the problem starts because of the algorithm which looks-up
  name's in the environment. because of the assumption that
  every name in the current environment be unique, is not carried
  into the declarations of lambdas (because lambdas introduce their
  own binding context, in the parameter list and body, in which
  bindings can be created which 'shadow' bindings in outer contexts,
  essentially rebinding the name within the local context.
  however these both refer to different memory cells, and the
  latter will also exist in a separate stack frame while running
  natively.) so, in an instance of the problem itself. we are in
  a situation where we are either interpreting the evaluation of
  a function call in an interactive environment, or during compile-time
  execution, or a function call in native code. since this first version
  is leaning on the lambda calculus to type and evaluate functions, we
  represent evaluation by a function called substitution, and denoted
  formally as:

  (\ id : type = term) (value2 : type) -> [id -> value2]term

  if we are in a call to the substitution function we are going
  to reduce a call Ast node with the result of substituting the parameter
  for the parameters name throughout the body of the LHS function.
  if we are substituting a lambda value for the name, and within the body of
  that lambda value there is a free variable that matches the name
  of the parameter of the lambda being looked at, then when the name-resolution
  function operates and looks up the name in the function lambda
  when evaluating within the substituted-lambda the search will conclude
  at the wrong variable, we will observe the binding of the argument,
  before we reach the free variable that the programmer meant.
  resulting in a semantic change depending on what we substitute.
  to avoid that, the argument name within the function being substituted into
  shall be changed, before the substitution action occurs.

  currently, i am reading { https://arxiv.org/pdf/0905.2539v3.pdf,
  https://en.wikipedia.org/wiki/Explicit_substitution#cite_ref-7,
  }
  which describes a lambda calculus extended with an explicit
  substitution method. this explicit substitution method is
  essentially a fomal implementation of the substitution method
  described in beta-reduction in the lambda calculus literature.
  which, as is rightly pointed out by the author of the paper,
  exists as a meta-operation to be carried out atomically,
  and thus sidesteps any conversation of the difficulties in
  implementing this operation, and leaving that difficulty
  essentially 'up to the reader', their mental 'abstract machine'
  is what preforms the substitution so you have to calibrate that right?
  obviously this is a great
  resource in learning how to implement the substituion operation
  on my lambda terms. interestingly, because the substitution operation
  is explicit, this means that programmers could potentially write
  subtitutions themselves.

  here is a super usefull inductive definition of free and bound variables
  i first encountered here { https://arxiv.org/pdf/0905.2539v3.pdf }
  I have since encountered it elsewhere.

  terms of the the lambda calculus we are currently considering

    t := x            variable
       | \x => t      function literal
       | t t'         application
       | t[x -> t']    substitution

  substitution and the function literal, both
  bind free occurances of x within the body of t.
  with the function, that value can be bound by
  application, whereas with substitution, the replacement
  term appears in the body of the expression.

  consider some term t, consisting of scentances in the lambda calculus.
  the free variables in the term denoted FV(t) is the set constructed
  by the following inductive relations:
  FV(t) : Set :=
    FV(x)         := {x}
    FV(\x => u)   := FV(u) minus/compliment {x}
    FV(u v)       := FV(u) union FV(v)
    FV(u[x -> v])    := (FV(u) minus {x}) union FV(v)

  the term BV(t) stands for the set of bound variables in
  some term t. and can be described by the following set of
  inductive relations:
  BV(t) :=
    BV(x)       := {}
    BV(\x => u) := BV(u) union {x}
    BV(u v)     := BV(u) union BV(v)
    BV(u[x -> v])  := BV(u) union {x} union BV(v)


  inductive relations translate relatively naturally
  into if () then () else if () then () else if () then () ... else ()
  blocks of code. they translate more naturally into
  functions representing each lemma, and invoked depending
  on the type of the current tree node being looked at.
  especially if we fenagle the data-structures such that
  we can manipulated them in the same way as within the lemmas.

  { another source:  K. H. Rose, Explicit Substitution – Tutorial & Survey, BRICS LS-96-3, September 1996 }
  the result of renaming all free occurances of y in M to z
  is written M[y -> z]
  it can be defined by induction on terms in the lambda calculus thus:
    (M, N ranges over valid scentances,
     x, y, x', ... range over valid variable names,
     )

    M[y -> z] :=
      (variables)
      x[y -> z]           := z if x = y
      x[y -> z]           := x if x != y

      (procedures/functions/abstractions)
      (\x => M)[y -> z]   := \x' => M[x -> x'][y -> z]
                             where we select x' such that
                             x' is-not-in (FV(\x => M) union {y, z})

      (M N)[y -> z]       := (M[y -> z]) (N[y -> z])

      (composition of substitutions)
      (M[x -> N])[y -> z] := M[x -> x'][y -> z] [x' -> N[y -> z]]
                             where we select x' such that
                             x' is-not-in (FV(\x => M) union {y, z})


  two terms are alpha equivalent if they share the same form
  minus the consideration of the names of the bound variables.
  (the literature says "modulo alpha-equivalence/variable renaming"
    where modulo refers to the (%) operator whitch returns the remainder after division.
    which i suppose is a metaphor for what is left after
    removing/dividing alpha-equivalence from the fomulae?)
  that is \x => x         is alpha equivalent to \y => y
          \z => z x       is alpha equivalent to \k => k x
          \a => \b => a b is alpha equivalent to \c => \d => c d

  this can also be defined inductively over terms using the alpha-equivalence
  operator which I will denote (a=)
    x         a= x
    \x => M   a= \y => N     if (M[x -> z] a= N[y -> z])
                             where we select z such that
                             z is-not-in FV(M) union FV(N)

    M N       a= P Q         if ((M a= P) and (N a= Q)

    M[x -> N] a= P[y -> Q]   if ((N a= Q) and (M[x -> z] a= P[y -> z]))
                             where we select z such that
                             z is-not-in (FV(M) union FV(P))
*/
