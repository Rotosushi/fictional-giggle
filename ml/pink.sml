

(*
Pink: Formal Specification
V0.1 language, just the basics of the basics.

Major Components:

  Name-Binding-Context a.k.a the Environment

  ENV := (name, (type, value)) list

  the environment supports two basic operations,
  bind and FV, bind adds a new name to the environment,
  and FV returns a list of the variables bound in
  the environment. we could also write this dom(ENV)
  to mean the domain of the environment.

  bind (name, (type, value), ENV) -> nil;

  FV (ENV) -> (name, (type, value)) list;



----------------------------------------------------------------
grammar

  pink := %empty
        | expr

  expr := term expr

  term := nil
        | id
        | lambda
        | call
        | bind

  nil := "nil"

  id  := [a-zA-Z][a-zA-Z0-9_-']*

  lambda := '\\' id (: type)? '->' term

  call := term term

  bind := id ':=' term

  type := Nil
        | type -> type
        | '(' type ')'

  value := nil
         | '\\' id : type '->' term


---------------------------------------------------------------
typeability


      # the type of nil is defined to be Nil
            ENV |- nil : Nil


      # if we can find a binding with some id
      # we can extract it's type and value.
             id is-in FV(ENV)
            ------------------
          ENV |- id : type = value


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



      # the type of a function is type -> type
      # so the type of a particular function
      # is the type of it's argument type1,
      # arrow the type of its body type2
      # type1 -> type2
          ENV |- id : type1, term : type2
          --------------------------------
      ENV |- id : type1 -> term : type1 -> type2




      # if term1 has type type1 -> type2
      # and term 2 has type1,
      # then the type of the whole term is type2
      # if we bind some term to the value, we
      # will be binding a type2, because that is
      # the result/replacement type of the function.
    ENV |- term1 : type1 -> type2, term2 : type1
    --------------------------------------------
            ENV |- term1 term2 : type2





---------------------------------------------------------------
execution

  # this codifies the sense of name replacement in the
  # rules of the language. if we are calling some function
  # we can find a natural value to replace for the arguments
  # name, it is given in the parameter. however if we are
  # trying to evaluate some function and we come across
  # a name that is not in the parameter list, then
  # we search the ENV for some binding of that name.
  # then we can replace for the bound value.

      ENV |- id : type = value
      -------------------------
          id -> value


    # so a step of computation is represented
    # as the valid replacement of terms from one
    # to another. such as reducing a function value
    # down to a single integer via evaluation.
    # by these following rules, we reduce a function application
    # by first reducing the first term down to some
    # function value, then reducing the functions argument
    # down to it's value, then we can apply the final
    # computation by replacing the value2 for the
    # arguments name within the body of the function.

          term1 -> term1'
      ----------------------
    term1 term2 -> term1' term2

          term2 -> term2'
      ----------------------
    value1 term2 -> value1 term2'

    (\ id : type = term) (value2) -> [id -> value2]term


    # we want to bind the identifier
    # to the result of evaluating the rhs term.
    # this first rule allows the rhs to be evaluated,
    # then, when we evaluate to the final value and
    # we can see that the id is not already bound,
    # we can evaluate the binding.

          term2 -> term2'
    ---------------------------
      id := term2 -> id := term2'

  ENV |- term2 : type2 = value2, id is-not-in FV(ENV)
    ----------------------------------------------
    id := value2 -> bind (id, (type2, value2)), ENV)

 *)

(*
  Here is where we use ML-yacc to describe the
  LALR language of pink! or with some minor syntactic 
  changes.


*)







































(**)
