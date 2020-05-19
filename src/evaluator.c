
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "evaluator.h"
#include "ast.h"
#include "symboltable.h"
#include "error.h"

void evaluate_type(Ast** type, symboltable* env);
void evaluate_id(Ast** id, symboltable* env);
void evaluate_lambda(Ast** lambda, symboltable* env);
void evaluate_call(Ast** call, symboltable* env);
void evaluate_bind(Ast** bind, symboltable* env);

void substitute(char* name, Ast** term, Ast* value, symboltable* env);
bool appears_free_in(char* name, Ast* term);
void rename_binding(Ast* lambda, Ast* value);


/*


  so, the general usage of evaluate should take in
  an Ast describing the syntax of the language.
  and act like the abstract machine that evaluate's
  Asts, which reduces the tree down to a new tree
  representing the value original expression.
  so, the 'result' of a name is it's value.
  the 'result' of a type is itself.
  the 'result' of a lambda is itself.
  the 'result' of a call is the application of substitution of the lhs
    throughout the body of the rhs for the name of the parameter.
  the 'result' of a bind is that the environment binds a name
    to some term as it's value.

    i think that the evaluator should be written
    to construct a result tree from evaluating the passed
    tree, and should not modify the passed tree.
    that gets tricky when reducing terms we create
    during previous reductions, when do we deallocate
    them, if we can't tell the difference between a node
    that was created during reduction (an intermediate term)
    or one that was created by the parser. (and we can't tell that easily)
    the process of in place creation could be emulated by making a local copy,
    and preforming the reductions in place on the local copy.
    but then, does this scope own that memory, and
    then we return the new memory which the outer scope now needs to know
    to deallocate, or does that make it
    make more sense for the outer scope
    to own the new copy, which then we are back to
    modifying the passed tree in place, and it
    "just happens" to be a copy.
    this is a very common cyclical issue with memory management.
    the most sensible solution is to try as much as possible to
    delete the memory you allocate yourself, and to pass
    dynamic allocations as return values as little as possible.
    this obviously has an effect on the shape of the code that
    you write.

    honestly, we are going to modify in place. from now on, evaluate
    own's and consumes the passed term data.
*/
void evaluate(Ast** term, symboltable* env)
{
  /* dynamic type dispatch! */
  switch((*term)->tag) {
    case N_TYPE:   evaluate_type(term, env);   break;   /* return the type as a value. */
    case N_ID:     evaluate_id(term, env);     break;   /* return the bound term. */
    case N_LAMBDA: evaluate_lambda(term, env); break; /* return the lambda as a value. */
    case N_CALL:   evaluate_call(term, env);   break;  /* return the result of calling the fn. */
    case N_BIND:   evaluate_bind(term, env);   break;   /* evaluate the bind and return the value nil. */
    default: error_abort("malformed ast node tag! aborting");
  }
}


void evaluate_type(Ast** type, symboltable* env)
{
  /*
  when we allow names to be bound to type terms
    it only seems natural to allow typenames to then
    occur in type expressions. however this is beyond
    the scope of v0.0.1.
    maybe v0.0.2.
    we currently do not allow names to appear in the
    bodies of type expressions, so there is no way
    for any term bound to a type value to need any
    amount of computation (the programmer cannot express
    any type that would need replacing). however, eventually
    standard variable replacement can be used in the
    same way it is used by the other rules, just replacing
    the bound term in place by pointer assignment.
    it just so happens that the name is bound to a type
    expression only legal in a type expression. just
    as an int is only legal in int expressions, and so forth.
    this is really an extension of the idea that nil is the
    literal expressing the singular value of type Nil, and
    nil can appear in expressions, just as the names of
    types can appear in so-called type expressions.
    however, a type expression would consist of the typename Nil
    appearing, and not the literal nil. this is subtle and
    if the programmer is not careful with capitalization
    they will easily generate a bug. hopefully something
    we can discover and label a compiler error.
  */

}

void evaluate_id(Ast** id, symboltable* env)
{
  /*
   in order to evalute an id,
   we need to discover what value
   it is bound to. this is done by
   looking up the symbol in the Environment
   and returning the bound term as the result.
   if we cannot find the binding in the environment,
   then the term is not typeable.

   ENV |- id : type = value
   -------------------------
       id -> value
  */
  if (id != NULL && *id != NULL) {
    symbol* sym = lookup((*id)->u.id.s, env);
    if (sym != NULL) {
      // free the memory originally associated with this node.
      free (*id);
      // replace the original term with the value bound to the symbol
      (*id) = sym->term;
    }
    else {
      error_abort("cannot find name in environment! aborting");
    }
  }
  else {
    error_abort("cannot evaluate/lookup NULL id!");
  }
}

void evaluate_lambda(Ast** lambda, symboltable* env)
{
  /* a lambda is a value, so it can itself
      be the valid result of execution.
      that is, beta-reduction stops once the
      term is in normal form, and the normal-form
      is any value. which again a lambda term is
      a valid value form. so we don't need to
      evaluate lambda terms on their own.

      think of it from the perspective of some caller,
      we are calling evaluate, which dispatched
      to evaluate_lambda on some term,
      that term is either the root, or we are one
      step away from the caller node, and are visiting a
      leaf node, of either call or bind.
      1) the user entered a lambda value on it's own,
        -> the lambda is a value, so it makes sense to print
            the lambda-literal as the result of execution.
      2) the lambda was returned as the result of some execution
        -> the lambda is a value, so it makes sense to print
            the lambda-literal as the result of execution.
   */
}

void evaluate_call(Ast** call, symboltable* env)
{
  /*
  in order to evaluate a call expression, we want to
  replace the call node with the result of substituting
  the rhs throughout the body of the lhs.
            term1 -> term1'
        ----------------------
      term1 term2 -> term1' term2

            term2 -> term2'
        ----------------------
      value1 term2 -> value1 term2'

      (\ id : type = term) (value2) -> [id -> value2]term
  */
  if (call != NULL && *call != NULL) {
    /*
      Ast* lhs = (*call)->u.call.lhs;
      Ast* rhs = (*call)->u.call.rhs;
    */
    // evaluate the lhs down to a lambda value
    while((*call)->u.call.lhs->tag != N_LAMBDA) {

      if ((*call)->u.call.lhs->tag == N_TYPE) {
        error_abort("cannot evaluate a type to a lambda value! aborting");
      }
      else if ((*call)->u.call.lhs->tag == N_BIND) {
        error_abort("cannot evaluate a bind to a lambda value! aborting");
      }

      evaluate(&((*call)->u.call.lhs), env);
    }

    // evaluate the rhs down to a value.
    while(1) {
        NodeTag rhs_tag = (*call)->u.call.rhs->tag;
      if (rhs_tag == N_LAMBDA)
        break;
      else if (rhs_tag == N_ID)
        evaluate(&((*call)->u.call.rhs), env);
      else if (rhs_tag == N_CALL)
        evaluate(&((*call)->u.call.rhs), env);
      else if (rhs_tag == N_BIND)
        evaluate(&((*call)->u.call.rhs), env);
      else if (rhs_tag == N_TYPE)
        break;
      else error_abort("malformed ast node tag! aborting");
    }

    substitute((*call)->u.call.lhs->u.lambda.arg.id.s, \
               &((*call)->u.call.lhs->u.lambda.body), \
               (*call)->u.call.rhs, \
               env);

    Ast* result_of_substitution = CopyAst((*call)->u.call.lhs->u.lambda.body);
    /*
      because substitute preforms replacement via ptr assignment,
      the ptr that the second argument holds will change to
      the ptr that was passed as the rhs, therefore when we
      delete the lhs we also delete the rhs as well, because
      well, the body of the lambda literally becomes the rhs.
      then the algorithm which is correct becomes incorrect because
      this program creates a refrence loop.

      so, how should substitution work in order to avoid this?

      assign a copy of the rhs?
        does that enforce the right semantics?
    */
    AstDelete((*call));
    (*call) = result_of_substitution;
  }
  else {
    error_abort("canot evaluate NULL call! aborting");
  }
}

void evaluate_bind(Ast** ast, symboltable* env)
{
  /*
                        term -> term'
                  ---------------------------
                  id := term -> id := term'

       ENV |- term : type = value, id is-not-in FV(ENV)
        ----------------------------------------------
      id := value -> bind (id, (type, value)), ENV) : Nil
  */
  if (ast != NULL && *ast != NULL) {

    NodeTag term_tag = (*ast)->u.bind.term->tag;

    while (1) {
      if (term_tag == N_LAMBDA)
        break;
      else if (term_tag == N_TYPE)
        break;
      else if (term_tag == N_ID)
        evaluate(&((*ast)->u.bind.term), env);
      else if (term_tag == N_BIND)
        evaluate(&((*ast)->u.bind.term), env);
      else if (term_tag == N_CALL)
        evaluate(&((*ast)->u.bind.term), env);
      else
        error_abort("malformed ast node! aborting");
    }

    bind ((*ast)->u.bind.id.s, (*ast)->u.bind.term, env);
    AstDelete((*ast));
    (*ast) = CreateAstTypeNil();

  }
  else {
    error_abort("cannot evaluate NULL bind");
  }
}

void substitute(char* name, Ast** term, Ast* value, symboltable* env)
{
  switch((*term)->tag) {
    case N_ID: {
      /*
      [id -> value2]id  := value2
      [id -> value2]id' := id'
      */
      if (strcmp(name, (*term)->u.id.s) == 0) {
        AstDelete((*term));
        (*term) = CopyAst(value);
      }
      // the name doesn't match, so we do nothing.
      break;
    }

    case N_LAMBDA: {
      /*
      [id -> value2](\ id : type = term)  := (\ id : type = term)
      [id -> value2](\ id' : type = term) :=
        if (id' is-not-in(value2))
        then (\id' : type = [id ->value2]term)
        else (\[id' -> id'']id' : type = [id -> value2][id' -> id'']term)

        this is the case that the body of a lambda
        is itself another lambda, so we need to decide wether to
        pass the substitution on to the body of that lambda.

        if the binding created by the lambda is the same
        as the binding we are trying to replace, then we
        do not substitute within the body of that abstraction.
        as the name we are looking for is already bound within
        the abstraction.
        if the bound variable of the lambda
        does not match the name we are trying to replace then
        we can pass the substituion operation into the body,
        however there is another possible issue that could
        arise, and that is in the case that the variable
        bound by the lambda appears in the Free Variables of
        the value we are substituting in. in this case we
        must rename the bound variable of the lambda abstraction
        before we can pass the substituion into the body of the lambda.
        in the case that the bound variable does not appear free in
        in the value we can just perform the substitution.
      */
      char* arg_name = (*term)->u.lambda.arg.id.s;
      if (strcmp(name, arg_name) == 0) {
        return;
      }
      else {
        /*
         the binding does not match, so now we need to
          perform the substitution within the body of
          the lambda abstraction.

          however we need to aware of one more case,
          if a free variable in the body of the value
          we are substituting for happens to match the binding
          of the lambda, an unintentional binding will occur.
          (think through what happens when we call the function
           lookup on a free variable which occurs in the value
           being substituted into the body of a lambda, whose
           name matches the lambdas binding name.
           when lookup resolves the name, it will encounter
           the definition of the argument binding before
           it will encounter the binding that the free
           variables author intended, hence the unintentional
           binding.)
          in this case we need to rename the binding of
          the lamdba abstraction throughout it's body before
          we substitute for the value.

          a possible solution would be to gather up the free
          variables present in (value) into a list and search that list
          for the argument name. this would require at least
          as much work as searching the tree directly
          to build the list of free variables, plus
          the work required to then search the list.
          (not to mention all of the allocation and deallocation
          of the list itself.)
          it is more efficient to directly search the subtree
          for free variables of the conficting name.

        */
        if (appears_free_in(arg_name, value)) {
          /* renames the argument of (*term)
             to a random string of characters
             throughout the body of (*term),
             being careful to avoid names which
             occur free in value.
          */
          rename_binding((*term), value);
        }

        substitute(name, &((*term)->u.lambda.body), value, env);
      }
      break;
    }

    case N_CALL: {
      //   [id -> value2]lhs rhs := [id -> value2]lhs [id -> value2]rhs
      // when we encounter a call Ast we pass the substitution
      // along to it's subterms.
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

    case N_TYPE: {
      break;
    }
  }
}
      /*
       are type descriptors values?
       i argue yes. then it becomes easy to
       compose types using named bindings, instead of
       having a separate type naming process.
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
       new values and new kinds of entity.
       normally types are bound by special variables
       which can bind types only and a special form of abstraction
       is used to bind type-variables to type-values,
       essentially the language definition is repeated
       but only for types; this can be avoided if instead
       of 'lifting' the semantics to meet the type, we 'push'
       the type down to meet the semantics.
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
       of a type, that makes no sense as the type is a description of
       the encoding of some value. instead we can
       create instances of values of some type.

       (it makes some sense to say "i have some
       encoding(type), and I can create a new copy of the encoding,
       however since the base types themselves are always constants
       we never need to create copies, we can just use pointers
       with no downsides)
       essentially types are only useful to the compiler and the programmer,
       not the machine, so the runtime doesn't have a sense of type.
       there may be checks of tags, but that is a particular implementation
       of runtime type, there is no intrisic sense of type in the actual
       instructions that execute.

        types are not
       values in the traditional sense of them being able to have
       existance, they are however a language entity which is
       useful to name for use in later parts of your program.
       we want to be able to name types for use in composing
       larger types, and we want to provide functions which
       work with more than one type, in order to provide
       polymorphic containers. (we also want to provide constraints
       around which types can be instanciated where.)
       from this perspective, types are providing a layer of indirection,
       and that indirection is being used to encode semantics.
       we can consider '->' to literally be an operator
       that takes in two type-values T1, T2 as arguments and returns
       the type-value 'T1 -> T2'. we can consider sum and product
       types similarly, the operators being * and | usually.
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
        the positions of the arguments.

        aside:
        lambdas cannot be naturally recursive without
        a y-combinator being typeable in the language.
        (essentially, typeing the y-combinator does not
         converge, because the y-combinator itself does
         not converge. it recurs indefinetely.)
         I still don't fully understand how the solutions to
         typing the y-combinator actually work. however
        i do know that the y-combinator is only truly
        necessary if you are unwilling to extend the
        calculus with a function abstrcation which
        introduces a named function, and then this
        abstraction can provide the name of the function
        within the body of the function, thus allowing for
        c-style recursive functions.
        so, maybe we sidestep
        the issue a bit, and we will revist the
        y-combinator at a later point, when for
        some reason we want to have recursive lambdas.
        otherwise we could allow both function literals to
        appear, and they can have separate semantics.
        additionally, recursion is used to implement looping
        in it's most base case, and Pink is already planning on
        providing a while loop to the base calculus, so
        lambdas will have to borrow the expressive power of
        Djikstra's three instead.


       aside:
       if the compiler expects a * -> * and it is given a *
       that should be a semantic error right?
       we cannot determine what behavior the syntax is
       going to request of the type before we have looked
       at the syntax, but just on type information alone,
       if the syntax expects a * -> * and is given an *.
       then my hypothesis is that that syntax will not
       typecheck. unless we can envision some type *
       what can be called like a function.

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
      case N_TYPE: {
        // names cannot appear in types
        return false;
      }

      case N_ID: {
        // name might appear in ID, better check.
        if (strcmp(name, term->u.id.s) == 0) {
          return true;
        } else {
          return false;
        }
      }

      case N_LAMBDA: {
        if (strcmp(name, term->u.lambda.arg.id.s) == 0) {
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
          return (appears_free_in(name, term->u.lambda.body));
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
    case N_LAMBDA: {
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
      if (strcmp(old_name, term->u.lambda.arg.id.s) == 0) {}
      else {
        rename_binding_in_body(new_name, old_name, term->u.lambda.body);
      }
    }
    case N_TYPE: {
      /* names cannot yet appear in type expressions.
         there is no way that a name could appear in
         a type expression that we would need to replace.
       */
    }
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
  char* arg_name = lambda->u.lambda.arg.id.s;
  char* new_name = NULL;
  do {
    if (new_name)
      free(new_name);

    new_name = generate_name(5);
  } while (appears_free_in(new_name, invalid_bindings));

  free(lambda->u.lambda.arg.id.s);
  lambda->u.lambda.arg.id.s = new_name;
  rename_binding_in_body(new_name, arg_name, lambda->u.lambda.body);
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
