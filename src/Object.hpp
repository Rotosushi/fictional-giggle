#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;

#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"

/*
  Objects are never evaluated.
  this means that they correspond to
  beta normal forms when we think of
  reducing the tree by evaluation.
  (from the lambda calculus perspective.)
  if this is confusing, think of it this way:
    we don't evaluate the number 3,
    we can consider the number 3 in the abstract,
    and we can perform steps of evaluation given
    an expression containing the number 3, say:
      3 + 1
    this is the core difference between what are called
      redexes (for: 'red'ucable 'ex'pressions)
      i.e. entities which can be evaluated.
      and the above named 'beta-normal forms'
      which is the catagory for every language
      entity which takes up memory
      (minus/modulus procedures, as the memory footprint
      is calculated differently depending on monomorphic
      procedures vs polymorphic procedures.)
      this is because when we 'consider a form' we
      can interpret that as being a construction of
      some particular form with some particular attributes.
      say Int(4) which is an integer whose value is 4.
      this number, should we need it at runtime
      -has- to take up space at runtime to exist!
      this is because information takes up spacetime.
      if we use it (a number) in a calculation that can be performed
      at compile time, then there is no reason to keep
      it (the number) within the runtime, because the information is
      no longer -required- at runtime. however during compile time
      it did take up spacetime! and during runtime, the result
      needs to take up spacetime.
      well, doesn't the runtime calculation
      x + y take up memory? yes! it most certainly does.
      the instruction which carries out the actual addition,
      and any instructions which manipulate information into
      specific cells each take up memory.
      however, the compiler need emit no instructions
      for a computation like 3 + 4, and the amount and kind of
      instructions would differ if the expression were
      3 + x. all of this means that what we are really
      focused on here is two things, state and behavior.
      we need to describe both using text, so we instead
      assign type and grammar structures specific
      semantics, such that the language entities play well together.
      when the compiler encounters Objects, we as programmers
      are describing state. when the compiler encounters
      procedures, we as programmers are describing behavior.
      (this simple english can become confusing once we
       consider that state and behavior are two sides of
       the same coin within a programming language.
       given that we can encode one into the other.
      (church numerals, function pointers))

*/

class Object
{
public:
  Object();
  virtual ~Object();
  virtual unique_ptr<Object> clone() = 0;
  virtual string to_string() = 0;
  virtual TypeJudgement getype(Environment env) = 0;

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) = 0;
  virtual void rename_binding(string old_name, string new_name) = 0;
  virtual bool appears_free(string name) = 0;
protected:
};
