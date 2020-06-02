
/*
execution

  # this codifies the sense of name replacement in the
  # rules of the language.
      ENV |- id : type = value
      -------------------------
          id -> value


    # so a step of computation is represented
    # as the valid replacement of terms from one
    # to another. (notated as: term -> term')
    # such as reducing a function value
    # down to a single integer via evaluation.
    # or replacing a name with it's value.
    # by these following rules, we reduce a function application
    # by first reducing the first term down to some
    # function value, then reducing the functions argument
    # down to a value, then we can apply the final
    # computation by replacing the value2 for the
    # arguments name within the body of the function.
    # we then end up with a new term, which is the
    # function body after the replacement, and
    # preform another step of computation.

          term1 -> term1'
      ----------------------
    term1 term2 -> term1' term2

          term2 -> term2'
      ----------------------
    value1 term2 -> value1 term2'

    (\ id : type = term) (value2 : type) -> [id -> value2]term


    # we want to bind the identifier
    # to the result of evaluating the rhs term.
    # we can consider a branch of execution finished
    # when we reduce to a value that can reduce the
    # current term-being-reduced.
    # this is the essence of call-by-value.
    # this first rule allows the rhs to be evaluated,
    # then, when we evaluate to the final value and
    # we can see that the id is not already bound,
    # we can evaluate the binding.

          term2 -> term2'
    ---------------------------
      id := term2 -> id := term2'

  ENV |- term2 : type2 = value2, id is-not-in FV(ENV)
    ----------------------------------------------
    id := value2 -> bind (id, (type2, value2)), ENV) : Nil




so the overal goal of the evaluate function is to take
an ast and reduce it down to a value.
(then we print it and bam! a REPL.)
So, the evaluator needs to operate until the ast
represents some valid language value.
which is either a function, or nil.

*/

#ifndef EVALUATOR_H
#define EVALUATOR_H

#include "ast.h"
#include "symboltable.h"

/*

*/
Ast* evaluate(Ast* term, Symboltable* env);


#endif
