#pragma once
#include <string>
using std::string;
#include <list>
using std::list;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;
using std::unique_ptr;


#include "Ast.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"

class Object
{
public:
  virtual ~Object() {}
  virtual unique_ptr<Object> clone() = 0;
  virtual string to_string() = 0;
  virtual TypeJudgement getype(Environment env) = 0;
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
  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) = 0;
  virtual void rename_binding(string old_name, string new_name) = 0;
  virtual bool appears_free(string name) = 0;
protected:
};

/*
  types, but as first class values.
  these are an implementation secret,
  only used for typechecking monomorphic
  procedures.
  someday, maybe I can come up with an argument
  for the validity of terms like
  mytype := <type> "type-op" <type>

  and maybe even
  sndtype := mytype "type-op" <type>

  further:
  typeproc := \x:Type => x "type-op" <type>

  which we probably want to restrict to compile time
  execution, because I think allowing the program
  to modify itself on the fly is not a static language
  feature. (this could very well be a place where the language
  could branch here.)
*/
class TypeLiteral : public Object
{
public:
  shared_ptr<Type> value;

  TypeLiteral(shared_ptr<Type> t) : value(t->clone()) {}
  TypeLiteral(const TypeLiteral& other) : value(other.value->clone()) {}

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
protected:

};

class Nil : public Object
{
public:
  Nil() {}
  Nil(const Nil& other) {}

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};

class Integer : public Object
{
public:
  int value;

  Integer(int v) : value(v) {}
  Integer(const Integer& other) : value(other.value) {}

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};

class Boolean : public Object
{
public:
  bool value;

  Boolean(bool v) : value(v) {}
  Boolean(const Boolean& other) : value(other.value) {}

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};

class Lambda : public Object
{
public:
  //vector<pair<string,Type>> args;
  string arg_id;
  shared_ptr<Type> arg_type;
  shared_ptr<SymbolTable> scope;
  shared_ptr<Ast> body;
  shared_ptr<list<string>> cleanup_list;

  Lambda(const string& a_id, const shared_ptr<Type>& a_type,
         shared_ptr<SymbolTable> enclosing_scope, const shared_ptr<Ast>& bd,
         shared_ptr<list<string>> cl)
    : arg_id(a_id), arg_type(a_type), scope(enclosing_scope), body(bd), cleanup_list(cl) {}

  Lambda(const Lambda& other)
    : arg_id(other.arg_id), arg_type(other.arg_type->clone()), scope(other.scope), body(other.body->clone()), cleanup_list()
  {
    for (const string& id : (*other.cleanup_list))
    {
      cleanup_list->push_back(id);
    }
  }

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};

/*
  we can probably rewrite this data
  structure thusly:

  class PolyLambda : public Object
  {
  Lambda def;
  list<shared_ptr<Type>> instances;
  ...
  }
  where we construct instances within HasInstance anyways!

  it works!
  the memory footprint of PolyLambda's just went down by like
  a lot.
*/
class PolyLambda : public Object
{
public:
  Lambda def;
  list<shared_ptr<Type>> instances;

  PolyLambda(Lambda& def)
    : def(def), instances() {}

  PolyLambda(PolyLambda& other)
    : def(other.def), instances(other.instances) {}

  EvalJudgement HasInstance(shared_ptr<Type> target_type, Environment env);

  virtual void substitute(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual void rename_binding(string old_name, string new_name) override;
  virtual bool appears_free(string name) override;
  virtual unique_ptr<Object> clone() override;
  virtual string to_string() override;
  virtual TypeJudgement getype(Environment env) override;
};

/*
  we know that each entity has a location,
  so we can simply factor that into the
  Ast itself.
*/
class Entity : public Ast
{
public:
  unique_ptr<Object> literal;

  Entity(void* v, const Location& loc)
    : Ast(loc), literal((new Nil())) {}

  Entity(int i, const Location& loc)
    : Ast(loc), literal((new Integer(i))) {}

  Entity(bool b, const Location& loc)
    : Ast(loc), literal((new Boolean(b))) {}

  Entity(unique_ptr<Lambda> l, const Location& loc)
    : Ast(loc), literal(move(l)) {}

  Entity(unique_ptr<PolyLambda> l, const Location& loc)
    : Ast(loc), literal(move(l)) {}

  Entity(shared_ptr<Type> l, const Location& loc)
    : Ast(loc), literal(unique_ptr<Object>(new TypeLiteral(l))) {}

  Entity(unique_ptr<Object> literal, const Location& loc)
    : Ast(loc), literal(move(literal)) {}

  Entity(const Entity& other)
    : Ast(other.location), literal(other.literal->clone()) {}

  Entity& operator=(const Entity& rhs)
  {
    literal = rhs.literal->clone();
    return *this;
  }

protected:
  virtual void substitute_internal(string var, shared_ptr<Ast>* term, shared_ptr<Ast> value, Environment env) override;
  virtual bool appears_free_internal(string var) override;
  virtual void rename_binding_internal(string old_name, string new_name) override;
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment env) override;
  virtual EvalJudgement evaluate_internal(Environment env) override;
};
