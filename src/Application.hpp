#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
/*
  okay, so this is of special interest.

  given our current language definition,
  we have lost the ability to partially apply
  procedures with no special effort on part
  of the programmer.

  we now have multiple argument procedures.
  which means that an application term is
  in and of itself treated differently.
  instead of representing an application in
  much the same way our sequence operator
  is defined, namely only storing a left
  and right hand side, and letting this
  application tree take a special form where
  the lhs is always a procedure, and the
  rhs is always the argument at that position.
  thus, to even reach the term containing two
  variables being abstracted over, we
  nessecarily peel away two layers of abstraction.
  now, instead, each layer of abstraction has
  the ability to support multiple arguments
  being substituted into the body simultaneously.
  so why the heck doesn't this work anymore:

  func := \x => \y => x + y

  func 3 4

  instead, this 'pealing' has been thrust
  in the programmers face, forcing them to
  code

  (func 3) 4

  arg!! the tree didn't go away! it just left the
  language spec, and entered the source code!

  whereas before, application was -always- this tree,
  meaning partial application was essentially the
  mechanism with which we 'supported' multiple
  argument procedures. (more like they fell into
  our laps by way of the definition of the langauge.)

  so, okay, what if we
  loop application when the number of formal arguments
  is smaller than the number of actual arguments?
  and we report an error if we encounter a situation
  in which the lhs becomes not a lambda, (which implies
  a mismatch in the number of expected arguments to
  the number of provided arguments.)
  this also implies we need to loop typing?
  essentially, accept partial application as a
  special case of application that the interpreter
  knows how to handle, (because we know what the
  'proper' semantics are from a single argument procedure
  perspective. we strive to maintain those semantics)
  and ditto for the assembly version of the language.

  which, truly, in my opinion, fuck.
  it would be simpler to define formal procedures as
  over a 'mulple-argument' (curried) lambda body.
  keeping the original, already working semantics
  of the original, than to design myself out of this
  hole. (especially given the more complicated current
  mechanism.) in a more example driven sense,
  the statement \x,y=>x+y literally translates to
  \x => \y => x + y under the hood.
  the statement
  fn my_fn (x,y) =>
  {
    x + y
  }

  also literally translates to

  my_fn := \x => \y => {x + y}

  (except that we don't go through the
  usual binding mechanism to allow for
  overloading of formal procedures.)

  then we provide a consistent calling mechanism
  which only ever has to deal with either single
  or multiple argument procedures. and we have a
  fairly stick out case with the 'returning' a procedure
  body as the result. because this implies a smaller
  number of actual arguments than provided arguments.
  which means constructing a closure around the evaluation
  and returning that. (because the type of the result
  would also reflect the fact that a procedure like thing
  is being returned. (a lot of this depends upon the
  'eager' or 'strict' nature of evaluation in this language.))

*/
class Application : public Ast
{
public:
  shared_ptr<Ast> lhs;
  shared_ptr<Ast> rhs;

  Application(shared_ptr<Ast> l, shared_ptr<Ast> r, const Location& loc)
    : Ast(loc), lhs(l), rhs(r) {}

  Application(const Application& other)
    : Ast(other.location), lhs(other.lhs), rhs(other.rhs)
  {

  }

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment& env) override;
  virtual EvalJudgement evaluate_internal(Environment& env) override;
  virtual void substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env) override;
  virtual bool appears_free_internal(string& var) override;
  virtual void rename_binding_internal(string& old_name, string& new_name) override;
};
