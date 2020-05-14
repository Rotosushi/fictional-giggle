
#include <stdbool.h>

#include "evaluator.h"
#include "ast.h"
#include "symboltable.h"
#include "error.h"

Ast* evaluate_type(Ast* type, symboltable* env);
Ast* evaluate_id(Ast* id, symboltable* env);
Ast* evaluate_lambda(Ast* lambda, symboltable* env);
Ast* evaluate_call(Ast* call, symboltable* env);
Ast* evaluate_bind(Ast* bind, symboltable* env);

Ast* substitute(char* name, Ast* term, Ast* value, symboltable* env);
bool is_in_dom_of(char* name, Ast* term);


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
    case N_TYPE:   return evaluate_type(term);   /* return the type as a value. */
    case N_ID:     return evaluate_id(term);     /* return the bound term. */
    case N_LAMBDA: return evaluate_lambda(term); /* return the lambda as a value. */
    case N_CALL:   return evaluate_call(term);   /* return the result of calling the fn. */
    case N_BIND:   return evaluate_bind(term);   /* evaluate the bind and return the value nil. */
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
    nil can appear in regular expressions, just as the names of
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
   then to term is not typeable.
  */
  if (id != NULL && *id != NULL) {
    symbol* sym = lookup((*id)->u.id.s, env);
    if (sym != NULL) {
      // free the memory originally associated with this node.
      free (*id);
      (*id) = sym->term;
    }
    else {
      printf("name {%s} not bound in env!", (*id)->u.id.s);
    }
  }
  else {
    printf("cannot evaluate/lookup NULL id!");
  }
}

Ast* evaluate_lambda(Ast** lambda, symboltable* env)
{
  /* a lambda is a value, so it can itself
      be the valid result of execution.
      that is, beta-reduction stops once the
      term is in normal form, and the normal-form
      is any value. which again a lambda term is
      a valid value form. so we don't need to
      evaluate lambda terms on their own.
   */
  if (lambda != NULL) {
    return lambda;
  }
  else {
    printf("cannot evaluate NULL lambda!");
    return NULL;
  }
}

Ast* evaluate_call(Ast** call, symboltable* env)
{
  if (call != NULL) {
    Ast* lhs = call->u.call.lhs;
    Ast* rhs = call->u.call.rhs;

    // evaluate the lhs down to a lambda term
    switch(lhs->tag) {

    }

    /*
    switch (rhs->tag) {
      case N_TYPE: {
        printf ("type expressions cannot be bound to an argument");
        return NULL;
      }

      case N_ID: {
        Ast* result = evaluate_id(rhs, env);
        AstDelete(rhs);
        call->u.call.rhs = result;
        break;
      }
      case N_LAMBDA: {
        // lambdas area already valid values
        break;
      }
      case N_CALL: {
        Ast* result = evaluate_call(rhs, env);
        AstDelete(rhs);
        call->u.call.rhs = result;
      }
      case N_BIND: {
        Ast* result = evaluate_bind()
      }
      default:
    }
    */

  }
  else {
    printf("canot evaluate NULL call!");
    return NULL;
  }
}

Ast* evaluate_bind(Ast** ast, symboltable* env)
{
  if (ast != NULL) {
    Ast* term = ast->u.bind.term;

    while (term->tag != N_VALUE) {
      term = evaluate(term);
    }

    bind (ast->id, term);
    return CreateAstTypeNil();
  }
  else {
    printf("cannot evaluate NULL bind");
    return NULL;
  }
}

Ast* substitute(char* name, Ast** term, Ast* value, symboltable* env)
{
  switch(term->tag) {
    case N_ID: {
      /*
      [id -> value2]id  := value2
      [id -> value2]id' := id'
      */
      if (strcmp(name, term->u.id.s) == 0) {
        // this is an Ast node which is an ID
        // that matches the ID we are replacing
        // so we replace. we know for a fact,
        // that the term in a part of the tree
        // maybe it is one size of a call term,
        // or a bind term, either way, that term
        // stores a pointer to the id node pointed
        // to by term,
        return value;
      }
      // the name doesn't match, so we can simply return
      // the non-matching name.
      return term;
    }

    case N_CALL: {
      // when we encounter a call Ast we pass the substitution
      // along to it's subterms.
      Ast *lhs = term->u.call.lhs, *rhs = term->u.call.rhs;
      term->u.call.lhs = substitute(name, term->u.call.lhs, value, env);
      term->u.call.rhs = substitute(name, term->u.call.rhs, value, env);
      /*
        if the substitution operation replaced the term with
        the value, then the address will have changed, because those
        two Ast nodes will have different memory locations. if it
        is a pointer to the same lhs or rhs node, then there was no
        operation applied and the address will be the same, because
        we returned the node we were passed. (or, the operation took
        place wthin the node below, and again, that same node is attached.)
        (think about the case of
        the rhs of the call node being a ptr to an Ast id node
        containing the bound variable.
        the above assignment operation will have replaced the lhs
        ptr with the value ptr by means of the 'return value;'
        expression in the 'case N_ID:' above.) only in the case in which
        the call to substitute preformed a replacement do we need to
        free the memory allocated to the now old lhs/rhs.
        if the node wasn't replaced, then we don't want to free
        what is there, as it's correct.
        !!!DANGEROUS CODE!!!
        it's usually always a bad idea to rely on ptr comparisons,
        but what other strategy gives the correct semantics?
        Garbage Collection, ...
        deleting the subterm when and where we decide to replace?
        i.e. in the case N_ID above.
        but then how do we communicate that to the above
        callers? (two-star ptr), make substitution return void.
        only ever operate on the tree in place from the two star ptr
      */
      if (lhs != term->u.call.lhs) AstDelete(lhs);
      if (rhs != term->u.call.rhs) AstDelete(rhs);
      return term;
    }

    case N_BIND: {
      // when we encounter a bind we pass the substitution
      // along to it's subterm, instances of the bound variable
      // occuring in the subterm will be replaced with the value.
      // what happens when the bind operation is binding the name
      // we are substituting for?
      Ast* st = term->u.bind.term;
      st = substitute(name, term->u.bind.term, value, env);
      return CreateAstBind(term->u.bind.id, st);
    }

    case N_TYPE: {
      // handle nil literal here,
      // are type descriptors values?
      // i argue yes. then it becomes easy to
      // compose types using named bindings, instead of
      // having a separate type naming process.
      // like 'typedef', or 'type', etc.
      // instead we leverage the already existant
      // binding operation, and type expressions
      // are now just another kind of expression.
      // operators on type describing new types
      // work the same as an operator on value describing
      // new values. except the value is always referring
      // to some type, which can then be used in other
      // type expressions. in a parallel sense to how
      // an integer value can be passed in and out of
      // functions on integers, and in each case the
      // result is itself an integer that can be further
      // manipulated.
    }

    case N_LAMBDA: {
      // ...
    }



  }
/*



  formally substitution is defined as:
  (\ id : type = term) (value2 : type) -> [id -> value2]term

  [id -> value2]term :=
    [id -> value2]id                    := value2
    [id -> value2]id'                   := id'
    [id -> value2]id := term            :=
    [id -> value2]lhs rhs               := [id -> value2]lhs [id -> value2]rhs
    [id -> value2](\ id : type = term)  := (\ id : type = term)
    [id -> value2](\ id' : type = term) := if (id' is-not-in(id))
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
  FV(t) :=
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
}



bool is_in_dom_of(char* name, Ast* term)
{
  switch(term->tag) {
      case N_TYPE: {
        return
      }

      case N_VALUE: {

      }

      case N_ID: {
        if (strcmp(name, term->u.id.s) == 0) {
          return true;
        } else {
          return false;
        }
      }

      case N_LAMBDA: {
        if (strcmp(name, term->u.lambda.arg.id) == 0) {
          // the name gets rebound in the lambda so
          // if the name appears in the body of the
          // lambda it will be associated with the
          // parameter binding, and no mistake is possible.
          return false;
        }
        else {
          // search the body of the lambda for instances of
          // the name.
          return (is_in_dom_of(name, term->u.lambda.body));
        }
      }

      case N_CALL: {
        return is_in_dom_of(name, term->u.call.lhs) \
            || is_in_dom_of(name, term->u.call.rhs);
      }

      case N_BIND: {
        return strcmp(name, term->u.bind.id) == 0)
      }
  }
}
