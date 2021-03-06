#pragma once
#include <string>
using std::string;
#include <memory>
using std::make_unique;
using std::unique_ptr;
#include <utility>
using std::move;
#include <list>
using std::list;
using std::get;
#include <vector>
using std::vector;
#include <optional>
using std::optional;

#include "llvm/IR/LLVMContext.h"
using llvm::LLVMContext;
#include "llvm/IR/Type.h"
using llvm::Type;
#include "llvm/IR/DerivedTypes.h"
using llvm::IntegerType;

#include "Location.hh"
#include "SymbolTable.hh"

class Procedure;
/*
  the abstract syntax tree is the main
  data structure of this program. each
  component is written against this tree,
  and each component preforms it's work
  as a specialization of a tree walking
  algorithm.
  if we can write compoenents such that they can
  act without caring about the structure of the
  tree, then we should get more leeway in changing
  or adding structures to the tree in the future.
*/


/*
  this is the main data-type
  which will be used to allow
  us to describe the Ast in terms
  of a type-heirarchy instead of
  an explicit tagged union.

  this should allow more conveinent
  syntax within traversal actions.
  because of the single argument which
  can be polymorphic.

  formally, we have nodes to capture
  each of the main language grammar terms,
  variables, entities, calls, binds,
  binops, unops, and finally the if conditional.
  (and in the immediate next version the
  sequence (';') and iteration (while) primitives)
  where entities encapsulate the language literals,
  nil, Booleans, Integers, and Procedures.
*/
class Ast {
public:
  Location loc;

  Ast() : loc() {}
  Ast(const Location& loc) : loc(loc) {}
  virtual ~Ast() = default;

protected:
  virtual Ast* clone_internal() { return new Ast(); };
  virtual string to_string_internal() { return ""; };
  /*
  in order to support copying of Ast objects
  we use the "clone" pattern. whereby we deep-copy
  unique_ptrs, then we get a new copy of the entire
  tree. we absolutely do not want to use shared_ptrs
  for evaluation or typechecking. (i think...)

  basically, when we call clone on some arbitrary
  Ast, we dispatch to the clone of the right
  derived class, which calls the copy constructor, then returns
  the copy through the Ast return type..
  (this function "clone_interal"
  is extended through covariance, as it's
  return type can vary by derived class.
  and that is okay precisely because it is
  extended over derived classes, which means
  every return type is an Ast,
  and each differing return type is unique to
  it's class, each of which can have only one
  overriding definition, per each derived class.)

  */
public:
  virtual unique_ptr<Ast> clone() { return unique_ptr<Ast>(clone_internal()); }
  virtual string to_string() { return to_string_internal(); }
};

/*
  Empty, for when the user enters an empty term.
  i.e. they enter no characters where a term was
       expected.
*/
class EmptyNode : public Ast {
public:

  EmptyNode() {};
  EmptyNode(const Location& loc) : Ast(loc) {}
  EmptyNode(const EmptyNode& e) : Ast(e.loc) {}

protected:
  virtual EmptyNode* clone_internal() override
  {
    return new EmptyNode(*this);
  }
  virtual string to_string_internal() override
  {
    return "";
  }
};

/*
  variables are one of the essential
  components of Abstraction, and thusly
  our programming language.
*/
class VariableNode : public Ast {
public:
  string id;

  VariableNode(const string& str) : Ast(), id(str) {}
  VariableNode(const string& str, const Location& loc) : Ast(loc), id(str) {}
  VariableNode(const VariableNode& r) : Ast(r.loc), id(r.id) {}

protected:
  virtual VariableNode* clone_internal() override { return new VariableNode(*this); }

  virtual string to_string_internal() override {
    return id;
  }
};

// Application is another essential component
// of Abstraction, here we name it a Call, as
// that matches our intuition of this expression
// representing a procedure call in the
// assembly representing this expression.
// this mental/internal alignment is central to the language.
class CallNode : public Ast {
public:
  unique_ptr<Ast> lhs;
  unique_ptr<Ast> rhs;

  CallNode(unique_ptr<Ast> l, unique_ptr<Ast> r) : Ast(), lhs(move(l)), rhs(move(r)) {}
  CallNode(unique_ptr<Ast> l, unique_ptr<Ast> r, const Location& loc) : Ast(loc), lhs(move(l)), rhs(move(r)) {}
  CallNode(const CallNode& c) : Ast(c.loc), lhs(c.lhs->clone()), rhs(c.rhs->clone()) {}

protected:
  virtual CallNode* clone_internal() override { return new CallNode(*this); }

  virtual string to_string_internal() override {
    /*
      given some call:
      a b c d
      i am very torn on how to display some
      arbitrary call expression.
      when do parens make things more explicit?
      wrapping parens around the LHS of the
      call expression recursively groups expressions
      like:
      (((a) b) c) d

      whereas wrapping around the RHS of the call
      expression recursively groups expressions
      like:
      a (b) (c) (d)

      wrapping around the whole expression recursively
      groups like:
      (((a b) c) d)

      i have flip flopped a lot with this.
    */
    string result;
    result  = "((";
    result += lhs->to_string();
    result += ") ";
    result += rhs->to_string();
    result += ")";
    return result;
  }
};

// a bind expression is an essential component
// in making the lambda calculus usable.
// it is secretly innate in the definition of the
// calculus; however it has traditionally been
// considered a 'meta-operation' to be
// performed by the human-computer evaluating
// the mathematical expression of the lambda calculus.
// but this action is so basic (bind a name to some
// value), that it is in fact one of the central
// actions of the calculus itself (arguments).
// so why early lambda calculus didn't consider
// name binding formally eludes me (other than
// plain human lazyness, but that isn't very charitable.)
class BindNode : public Ast {
public:
  string          id;
  unique_ptr<Ast> rhs;

  BindNode(const string& str, unique_ptr<Ast> r) : Ast(), id(str), rhs(move(r)) {}
  BindNode(const string& str, unique_ptr<Ast> r, const Location& loc) : Ast(loc), id(str), rhs(move(r)) {}
  BindNode(const BindNode& rhs) : Ast(rhs.loc), id(rhs.id), rhs(rhs.rhs->clone()) {}

protected:
  virtual BindNode* clone_internal() override { return new BindNode(*this); }

  virtual string to_string_internal() override {
    string result;
    result  = id;
    result += " := ";
    result += rhs->to_string();
    return result;
  }
};

/*
class TupleNode : public Ast {
public:
    vector<unique_ptr<Ast>> members;

    TupleNode() {}
    TupleNode(vector<unique_ptr<Ast>>& membs)
    {
      auto len = membs.size();
      for (int i = 0; i < len; i++)
      {
        members[i] = move(membs[i]);
      }
    }
    TupleNode(const TupleNode& rhs)
    {
      auto len = rhs.members.size();
      for (int i = 0; i < len; i++)
      {
        members[i] = rhs.members[i]->clone();
      }
    }

protected:
    virtual TupleNode* clone_internal() override
    {
      return new TupleNode(*this);
    }

    virtual string to_string_internal() override
    {
      string result;
      result = "(";
      if (members.size() != 0)
      {
        for (int i = 0; i < members.size(); i++)
        {
          if (i < (members.size() - 1))
          {
            result += members[i]->to_string();
            result += ", ";
          }
          else
          {
            result += members[i]->to_string();
          }
        }
      }
      result += ")";
      return result;
    }
};
*/
/*
 a binop is one component of
 affix/infix expressions, which
 are in my opinion another essence
 of our programming language.
 where abstraction allows one to
 take some expression and abstract
 over it, expressions themselves
 are composed together via binary
 and unary operators. (Binops, and Unops
 respectively.)
 in order to make abstraction useful (one
 may beg the question, abstraction was proven
 turing complete, why do we need anything more?
 basically, so the language doesn't become
 a turing tar-pit. getting things done is the
 constant and pressing goal, clarity and
 communication help to facilitate that goal.
 just because everything "can" be done in one
 way, doesn't provide enough reason that is
 "should" be done in that way and no other.
 in fact, as we consider scaling, clarity of
 design and of communication become paramount
 in my opinion.),
 we must be given some other abstraction for
 specifying expressions to then subsequently
 abstract over. now, given the complicated
 nature of what one must specify in some
 programming language, one needs a powerful
 abstraction to specify programs. luckily we
 already know what it is. and again, it has classically
 not been considered formally, mathematitions
 essentially do the equivalent of
 "#include basic-arithmetic"
 into formal lambda calculus definitions. to allow them
 the freedom to write numers and addition and such.
 in reality the abstraction of an infix expression
 is very versatile and mathematically sound, what
 makes it a slam dunk for me is if we consider two
 features, operator overloading and operator declarations.
 these two make infix expressions very expressive indeed.
 if we consider then, subtyping within our type definition
 mechanisms, we have something that is approaching
 production quality. all that would be required
 to push such a language over the top is modules,
 and a solid standard library. (all of which is very
 non-trival.)
*/
class BinopNode : public Ast {
public:
  string          op;
  unique_ptr<Ast> lhs;
  unique_ptr<Ast> rhs;

  BinopNode(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r) : Ast(), op(str), lhs(move(l)), rhs(move(r)) {}
  BinopNode(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), lhs(move(l)), rhs(move(r)) {}
  BinopNode(const BinopNode& rhs) : Ast(rhs.loc), op(rhs.op), lhs(rhs.lhs->clone()), rhs(rhs.rhs->clone()) {}

protected:
  virtual BinopNode* clone_internal() override { return new BinopNode(*this); }

  virtual string to_string_internal() override {
    string result;
    result  = lhs->to_string();
    result += " ";
    result += op; // op already 'is-a' string :)
    result += " ";
    result += rhs->to_string();
    return result;
  }
};


/*

  unary operations are another third of
  affix expressions, they comprise operations
  which are valid on single entities.
  these can include lots of actions,
  negation of natural numbers, taking a
  square root of a number, taking the
  refrence of an object and derefrencing
  to retrieve some object are all unary
  operations.

  the last third of affix/infix expressions
  which are currently absent from the language.
  why? because they are secretly binary operations,
  the array-derefrencing operator of c is a two
  argument function. the tuple derefrencing operation
  is a two argument function. what makes these operations
  different from regular binary operators is the
  way they interact with precedence. when a programmer
  types A + B[C] the expectation is that the derefrence
  happens before the addition, so that we can
  conveinently write operations upon cells within the
  array structure. so, postfix operations are similar
  to unary operations, in that they have very high
  precedence. such that we always expect them to happen
  before any binary/infix operator is applied.
  now, since postfix operators can be thought of
  as very high precedence binary operators,
  why make them their own class of physical thing?
  why not just define them in terms of the more
  primitive binary operation?
  the tricky part of this way of thinking is noticing
  that postfix operations traditionally have
  two symbols which enclose the second argument to
  the function. how do we allow a programmer to
  define a new postfix operation and extend the
  parser accordingly? with unary and binary operations
  the choice is obvious, we can conceive of both
  in terms of a single unit of text which disambiguates
  operations, (with overloading handling operations
  which are valid upon more than one type) ((in this
  sense operators are a special kind of procedure.))
  perhaps the formal definition of a postfix operation
  requires two strings which will correspond to the
  beginning token of the postfix operation and
  the ending token. but given that the argument
  itself must by definition allow any other term
  to appear, (to allow the programmer to prefix any
  postix operation with other operations.)
  how do we properly extend the parser such that we
  allow two arbitrary strings to direct the parse
  of the postfix operation. we could imagine a
  mapping which stores the pair of strings for each
  postfix operator, and we can use the beginning token
  direct the parser to a sub-parsing function which then
  looks up the pair of symbols and is responsible
  for parsing until we see (or do not see)
  the ending token. (how do we conceive of allowing
  arbitrary text appear as the ending token of
  any given term. because the current parser has the
  full set of tokens which can appear as the end of a term
  known a-priori. more particularly, the procedure
  is_ender(Token t) would seem to require a formal
  parameter modification, or, perhaps, we utilize
  the fact that operators are parsed as
  Token::Operator, and call an auxillary procedure
  which then can assume the full responsibility of
  comparing the text which was parsed as an operator
  and seeing if that appears as a valid ender for
  some postfix procedure call.

  we may be able to write the postfix operator
  parser subroutine atop a multimap. whereby
  we can associate the beginning string denoting
  some binary operation with the ending string
  denoting the end of the term by looking up the
  strings in the map, and using it's existance in
  the map structure to validate.
  we could use a multimap and set the keys to be
  the beginning and end tokens, but why not
  make it a map from the beginning token to the
  ending token + the rest of the important information?
  then the postfix procedure can 'parse' against
  the ending token, regardless of what specific text is associated
  with the operator is. which is precisely what allows for extension.
  (we then also need a way to lookup the operation,
   so which name do we use? maybe we do the c++ way, and
   the name of the postfix operation is the beginning and
   ending tokens concatenated together?)

   something to note is that postfix operators take precedence
   over unary operators, such that postfix operators retrieve
   values within some structure of types, which is then passed
   into the unop procedure to be acted upon.

   (the argument being based on the 'gut-check'
    evaluation of a statement like (-array[index])
    which presumably returns the negation of the
    index'th cell within array.
    procedure application doesn't look like it
    does in regular languages, so it isn't a
    postfix operation, like in c where f(x)
    is procedure application. instead pink
    spells it f x. to parse c style application
    we could parse application as a postfix operation.
    with '(' ')' being the begin and end tokens.
    and the list of arguments appearing as a literal
    comma separated list of names, instead of
    pink's application which simply acts over
    adjacent terms.
    instead of '[' ']'. these are the only procedure
    postfix operations I can think of?
    although... if we consider the actions that
    [] gives us, we could instead provide those
    actions via an overloaded '.' operator.)
    {data-type} '.' {integer-term}
    can serve as tuple and array lookup.
    {data-type} '.' {identifier}
    can serve as record lookup
    and:
    {data-type} '.' {string-term}
    can serve as hash-table lookup, and such.
    however, notice how the '.' operator is not
    a postfix operator, but a binary operator.
    with it's single token, and two terms being
    acted upon. which according to language
    precedence rules would apply the unop to
    the first term before applying the injection
    operation. this breaks the nice semantic meaning
    of a statement like
    'unop' {data-term} '.' {injection-argument}
    meaning apply the unary operation to some submember of
    this data according to the argument.
    which we do have, if the injection is done via
    'unop' {data-term} '[' {injection-argument} ']'

   which to me means that we want to
   ensure that we query against the set of postfix operations
   before we look for the next binary operation when we
   parse some given primary. which is the essence of
   higher precedence.
   in this way, if the beginning token of some postfix
   operation is also a member of the binary operators,
   the parser will always assume that the token
   predicts a postfix operation first, and then
   would need to backtrack if we found that not
   to be the case. (because we have to unparse the
   term between the predictive operator token and
   whatever token occurred to falsify this result.
   in order to predict a
   different path from the beginning. this is because
   we cannot know if this is a valid postfix operation
   until we either parse the ending token, or we fail
   to see the ending token. if you start to nest
   the postfix operations, this gets hairy. which is
   why they currently remain unimplemented.)
   additionally yes, like block comments and scopes,
   this parsing strategy greedily puts all tokens
   after the beginning token into the same term.
*/
class UnopNode : public Ast {
public:
  string op;
  unique_ptr<Ast>   rhs;

  UnopNode(const string& str, unique_ptr<Ast> r) : Ast(), op(str), rhs(move(r)) {}
  UnopNode(const string& str, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), rhs(move(r)) {}
  UnopNode(const UnopNode& rhs) : Ast(rhs.loc), op(rhs.op), rhs(rhs.rhs->clone()) {}

protected:
  virtual UnopNode* clone_internal() override { return new UnopNode(*this); }

  virtual string to_string_internal() override {
    string result;
    result  = op;
    result += rhs->to_string();
    return result;
  }
};

/*
  conditionals, when considered with
  sequence and iteration, again form
  another essence of this language.
  these three abstractions together
  make it easy to express some programs,
  and if we add in assignment,
  form something that is turing-complete.
  (assignment does most of the heavy lifting
   in getting to turing-completeness)
  iteration and recursion are two sides
  of the same coin, though it is a funky coin.
  as they aren't mirrors of each other.
  these primitive abstractions, I think
  are expressive enough to grow alongside
  the rest of the language. this allows
  for functional programming alongside
  imperitive programming.
  this is part of the essence of ML, we
  are just making it the main focus.
  simply choice plus assignment
  is turing complete, the lambda calculus
  is turing complete. and infix expressions
  are highly expressive, the features of
  overloading and the ability to define new
  operators gives the programmer the ability to
  place arbitrary code behind an operator,
  and to define new operations on their defined
  types. this is the growth that I speak of.
  and we need not consider a new binop as
  anything other than a new two argument function
  behind the scenes.
  affix/infix expressions, plus polymorphism,
  operator definitions, operator overloading,
  and subtyping allow for programmers to build up
  any number of layers on top of one another in
  many different logical ways. each expressing
  different parts of the basic affix expression.
  when we consider a body of executable code, we
  consider at some level, sequencing, selection,
  and iteration over basic entities
  in the language.
  we then have ways of composing and decomposing
  entities from/into their consituent parts.
  subtyping interferes with overloading,
  but if some object of a subtype is valid
  in all instances of where its supertype is valid,
  as subtyping would seem to imply to me, does it
  not make sense to simply pass the subtype to
  some function expecting a supertype?
  or does each subtype need it's own procedure
  instance.
  the only manipulations
  which exist (by this i mean the set of things
  that can be done to some type or done with more
  than one of some type),
  are either some primitive procedure,
  or some composite procedure, itself composed of
  primitive procedures, then the only way we could
  expect some type to be a subtype of another would
  be to say that the relationship is encapsulated by
  the set of procedures which can be validly applied
  to the supertype are also valid on the subtype.
  hence, any function i can call
  on the supertype, i can call on any subtype, and
  expect the body of that very procedure to itself
  validly manipulate the subtype object. now,
  this may be facilitated behind the scenes by way of
  more than one sequence of assembly instructions,
  or more than one instance of the procedure in the
  interpreter.
  but that is alright because the programmer need not
  be concerned with their correctness,
  and, given the above subtyping relationship,
  if we have some set of procedures which
  operates upon type A, and we say that some
  type B is a subtype of A, (written B <: A)
  then the compiler could infer that every function
  the programmer has written to operate upon
  A will be valid upon the type B, and therefore
  should any of these procedures be polymorphic,
  we could have the compiler generate the code
  for each procedure by means of the polymorphic
  mechanisms. and if these procedures are not
  polymorphic, we could have the compiler issue
  a warning or an error to the programmer, and
  we can do it for each procedure which is valid
  upon the supertype but not yet the subtype.
  essentially using one set of procedure overloads
  to infer the set of procedure overloads that need
  to be generated for a given subtype.

  in this same way, we can allow interoperation
  between program code and handrolled assembly,
  as long as the programmer is aware of how
  things are passed, they can write some function
  in assembly and call it directly within the
  program. the assumed type signature of the assembly
  would need to be
  associated with the assembly code by some mechanisms
  which would by nessecity alert the linker to these
  activities.
  the assumed type of the assembly is such that
  when i set up the calling context like so, i can
  issue a jump instruction to such and such location and
  expect to be jumping into somewhere that continues
  progress and preservation, i.e. well-formed behavior.
  and will presumably return to this location of
  execution whereupon we can resume the current
  state of affairs.
  this is in essence the same set of assumptions
  which are set up for each procedure call.

*/


class CondNode : public Ast {
public:
  unique_ptr<Ast> test;
  unique_ptr<Ast> first;
  unique_ptr<Ast> second;

  CondNode(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s) : Ast(), test(move(t)), first(move(f)), second(move(s)) {}
  CondNode(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s, const Location& loc) : Ast(loc), test(move(t)), first(move(f)), second(move(s)) {}
  CondNode(const CondNode& rhs) : Ast(rhs.loc), test(rhs.test->clone()), first(rhs.first->clone()), second(rhs.second->clone()) {}

protected:
  virtual CondNode* clone_internal()  override
  {
    return new CondNode(*this);
  }

  virtual string to_string_internal() override
  {
    string result;
    result  = "if ";
    result += test->to_string();
    result += " then ";
    result += first->to_string();
    result += " else ";
    result += second->to_string();
    return result;
  }
};

class WhileNode : public Ast {
public:
  unique_ptr<Ast> test;
  unique_ptr<Ast> body;

  WhileNode(unique_ptr<Ast> t, unique_ptr<Ast> b, const Location& loc)
    : Ast(loc), test(move(t)), body(move(b)) {}

  WhileNode(const WhileNode& rhs)
    : Ast(rhs.loc), test(rhs.test->clone()), body(rhs.body->clone()) {}

protected:
    virtual WhileNode* clone_internal() override
    {
      return new WhileNode(*this);
    }

    virtual string to_string_internal() override
    {
      string result;
      result += "while ";
      result += test->to_string();
      result += " do ";
      result += body->to_string();
      return result;
    }
};

enum class AtomicType {
  Undef,
  Poly,
  Nil,
  Bool,
  Int,
};

enum class TypeTag {
  Undef,
  Atomic,
  Procedure,
  //Tuple,
};

/*
  a type is either:
    and atomic type
    a polymorphic type
    a procedure type
    a tuple type
*/
class TypeNode : public Ast {
public:
  TypeTag tag;
  union U {
    AtomicType atomictype;
    struct {
      vector<unique_ptr<TypeNode>> args;
      unique_ptr<TypeNode> result_type;
    } proctype;
    //vector<unique_ptr<TypeNode>> tupletype;

    U() { atomictype = AtomicTag::Undef; }
    ~U() {}
    U(AtomicType tag) { atomictype = tag; }
    U(vector<unique_ptr<TypeNode>> as, unique_ptr<TypeNode> r)
    {
      int length = as.length();
      for (int i = 0; i < length; i++)
      {
        proctype.args[i] = move(as[i]);
      }
      proctype.result_type = move(r);
    }
    /*
    U(vector<unique_ptr<TypeNode>> rhs)
    {
      for (auto&& memb : rhs.tupletype)
      {
        tupletype.push_back(move(memb));
      }
    }
    */
  } u;

  TypeNode() : Ast(), tag(TypeTag::Undef), u() {}
  ~TypeNode() {}

  TypeNode(AtomicType atomictype, const Location& loc)
    : Ast(loc), tag(TypeTag::Monomorphic), u(atomictype) {}

  TypeNode(vector<unique_ptr<TypeNode>> as, unique_ptr<TypeNode> r, const Location& loc)
    : Ast(loc), tag(TypeTag::Procedure), u({as, r}) {}

  //TypeNode(vector<unique_ptr<TypeNode>> tpl, const Location& loc)
  //  : Ast(loc), tag(TypeTag::Tuple), u(tpl) {}

  TypeNode(const TypeNode& rhs)
    : Ast(rhs.loc), tag(rhs.tag)
  {
    switch(tag)
    {
      case TypeTag::Undef:
        break;

      case TypeTag::Atomic:
        u.atomictype = rhs.atomictype;
        break;

      case TypeTag::Procedure:
        for (auto&& arg : rhs.u.proctype.args)
        {
          u.proctype.args[i] = make_pair(get<0>(arg), get<1>(arg)->clone());
        }
        u.proctype.result_type = rhs.u.proctype.result_type->clone();
        break;

      /*
      case TypeTag::Tuple:
        for (auto&& memb : rhs.u.tupletype)
          u.tupletype.push_back(memb->clone());
        break;
      */
      default:
        throw "bad type tag";
    }
  }

  bool is_poly_type()
  {
    switch(tag)
    {
      case TypeTag::Undef:
        return false;

      case TypeTag::Atomic:
        if (u.atomictype == AtomicTag::Poly)
          return true;
        else
          return false;

      case TypeTag::Procedure:
      {
        for (auto&& arg : u.proctype.args)
        {
          if (get<unique_ptr<TypeNode>>(arg)->is_poly_type())
            return true;
        }
        if (u.proctype.result_type->is_poly_type())
          return true;
        else
          return false;
      }

      /*
      case TypeTag::Tuple:
      {
        for (auto&& memb : u.tupletype)
          if (memb->is_poly_type())
            return true;

        return false;
      }
      */

      default:
        throw "bad type tag";
    }
  }

protected:
  virtual TypeNode* clone_internal() override
  {
    return new TypeNode(*this);
  }

  virtual TypeNode* clone_internal() const override
  {
    return new TypeNode(*this);
  }

  virtual string to_string_internal() override
  {
    switch (tag)
    {
      case TypeTag::Undef:
        return "Undef";

      case TypeTag::Atomic:
      {
        switch(atomictype)
        {
          case AtomicTag::Undef:
            return "Undef";
          case AtomicTag::Poly:
            return "Poly";
          case AtomicTag::Nil:
            return "Nil";
          case AtomicTag::Int:
            return "Int";
          case AtomicTag::Bool:
            return "Bool";
          default:
            throw "bad atomic type";
        }
      }

      case TypeTag::Procedure:
      {
        string result;
        int length = u.proctype.args.length();
        /* ex:
        (\x : int, y: int => x + y * 2)
          : int -> int -> int
        */
        for (int i = 0; i < length; i++)
        {
          result += (get<unique_ptr<TypeNode>>(u.proctype.args[i]))->to_string();
          result += " -> ";
        }
        result += u.proctype.result_type->to_string();
        return result;
      }

      /*
      case TypeTag::Tuple:
      {
        string result;
        result += "(";
        int len = u.tupletype.size();
        for (int i = 0; i < len; i++)
        {

          (t1, t2, t3, ..., tn-1, tn)

          every element of the tuple except the
          last needs to have a comma afterwards.

          if (i < (len - 1))
          {
            result += u.tupletype[i]->to_string();
            result += ", ";
          }
          else
          {
            result += u.tupletype[i]->to_string();
          }
        }
        result += ")";
        return result;
      }
      */
      default:
        throw "bad type tag";
    }
  }

  virtual string to_string_internal() const override
  {
    switch (tag)
    {
      case TypeTag::Undef:
        return "Undef";

      case TypeTag::Atomic:
      {
        switch(atomictype)
        {
          case AtomicTag::Undef:
            return "Undef";
          case AtomicTag::Poly:
            return "Poly";
          case AtomicTag::Nil:
            return "Nil";
          case AtomicTag::Int:
            return "Int";
          case AtomicTag::Bool:
            return "Bool";
          default:
            throw "bad atomic type";
        }
      }

      case TypeTag::Procedure:
      {
        string result;
        int length = u.proctype.args.length();
        /* ex:
        (\x : int, y: int => x + y * 2)
          : int -> int -> int
        */
        for (int i = 0; i < length; i++)
        {
          result += (get<unique_ptr<TypeNode>>(u.proctype.args[i]))->to_string();
          result += " -> ";
        }
        result += u.proctype.result_type->to_string();
        return result;
      }

      /*
      case TypeTag::Tuple:
      {
        string result;
        result += "(";
        int len = u.tupletype.size();
        for (int i = 0; i < len; i++)
        {
          if (i < (len - 1))
          {
            result += u.tupletype[i]->to_string();
            result += ", ";
          }
          else
          {
            result += u.tupletype[i]->to_string();
          }
        }
        result += ")";
        return result;
      }
      */
      default:
        throw "bad type tag";
    }
  }

};


/*
  procedures:
  this is the trickiest part of the language
  when we consider the semantic translation
  from our syntax to the syntax of x86 and LLVM.
  procedures have a lot of subtlety to their
  correct implementation, and in my mind, this
  is where the language lives or dies.
  the bodies of procedures need to exist in
  the assembly translation, and,
  in order to satisfy all invariants,
  i would guess we need one assembly procedure
  body per monomorphic instance of any given
  polymorphic procedure, in addition to an instance per
  explicit overload of any procedure. ditto
  for all operators used (they are 'just' procedures
  themselves.). and any procedure that is
  stored as a value will actually need to be stored
  as a pair of pointers, one to the function,
  and one to the capture/closure of the 'local'(lexically)
  variables that said procedure captures from it's
  syntactic definition. the body of the procedure
  will be written such that it is assumed that
  any captured variables will be accessable through
  the capture/closure ptr, which will be implicitly
  passed as a hidden first argument to the procedure.
  and, since it is a refrence, the compiler gets the
  choice of where to allocate said closure,
  depending on which funarg problem is being solved.
  (down or up) now, here is a question that still
  needs answering, does the same closure work
  for any single given procedure? if the procedure is
  polymorphic, then any given instance the compiler
  generates will have the exact same capture list,
  as the defining location of the polymorphic procedure
  is the defining location of every monomorphic
  instance of that procedure (by definition.)
  however, if the user can write a new overload
  anywhere else in the program text, which would
  be the most conveinent implementation imo,
  then we couldn't expect that overload to have
  the same capture list. so within the compiler
  we need to be careful with our implementation
  of closures. even though a shared_ptr makes a
  lot of sense given some polymorph (as each
  monomorphic instance has the same capture/closure,
  by definition they capture the same locals.)
  it makes less sense for any given explicit overload.
  as each defining occurance has the ability to
  capture different locals. and, much later, we
  could imagine picking up explicit overloads from
  separate compilation units, especially when we
  consider overloads of operators against different
  programmer defined objects. the conveinent definition
  location is with the object you are overloading for.
  which by best practices is a separate module and
  file, which allows for a myriad of different ways
  of closing over different entities.
  so internally, procedures need to have their
  own capture/closure object (and hence, each
  assembly representation will have different instruction
  sequences for accessing the entities which were captured),
  but in the case of polymorphs we can,
  as an optimization, share the same closure/capture
  between the monomorphic instances.

  how do we solve the problem of multiple
  definition, which is what polymorphism and
  overloading both require. well, instead of
  considering a procedure to be a singular
  entity, we consider a procedure definition
  to actually define a set of procedures.
  then, when we want to call a polymorphic
  procedure, we can use the set accessor method
  to satisfy the typing invariants of the language,
  and return back a monomorphic instance of the
  set. or, when we see more than one definition
  of the same monomorphic procedure,
  we know that we can add that definition to the set.
  and when we call
  said procedure we can use the set accessor
  procedure to satisfy the typing requirements.
  (did you give me an argument list that I have
   a valid definition for? is the same question
   regardless of polymorphism or monomorphic overloads.)
   ((and, explicit monomorphic overloads of a
     polymorphic procedure acts as what c++ calls,
     partial template specialization))

*/

class ProcedureLiteral {
public:
  vector<pair<string, unique_ptr<Ast>>> args;
  unique_ptr<Ast> body;
  SymbolTable     scope;

  ProcedureLiteral() = default;
  ProcedureLiteral(const string& str, unique_ptr<Ast> at, unique_ptr<Ast> b, SymbolTable& s)
    : args(make_pair(str, move(at))), body(move(b)) {
      scope = s;
    }

  ProcedureLiteral(vector<pair<string, unique_ptr<Ast>>> as, unique_ptr<Ast> b, SymbolTable& s)
    : body(move(b))
  {
    for (auto&& arg : as)
    {
      args.push_back(make_pair(get<0>(arg), get<1>(arg)->clone()));
    }
    scope = s;
  }

  ProcedureLiteral(const ProcedureLiteral& p)
    : body(p.body->clone()) {
      // args = rhs.args.clone()
      scope = p.scope;
    }

  ~ProcedureLiteral() = default;

  ProcedureLiteral& operator=(const ProcedureLiteral& rhs)
  {
    arg_id   = rhs.arg_id;
    arg_type = rhs.arg_type->clone();
    body     = rhs.body->clone();
    scope    = rhs.scope;
    return *this;
  }

  string to_string()
  {
    string result;
    int length = args.length();
    result  = "\\";
    for (int i = 0; i < length; i++)
    {
      if (i < (length - 1))
      {
        result += get<string>(args[i]);
        result += ": ";
        result += (get<unique_ptr<Ast>>(args[i]))->to_string();
        result += ", ";
      }
      else
      {
        result += get<string>(args[i]);
        result += ": ";
        result += (get<unique_ptr<Ast>>(args[i]))->to_string();
      }
    }
    result += " => ";
    result += body->to_string();
    return result;
  }
};

class ProcedureDefinition {
public:
  ProcedureLiteral def;
  vector<ProcedureLiteral> set;
  bool poly;

  ProcedureDefinition() = default;
  ProcedureDefinition(const ProcedureLiteral& def)
    : def(def), set()
  {
    // if any type in the procedures signature is polymorphic,
    // the entire term is polymorphic.
    bool p = false;
    for (auto&& arg : def.args)
    {
      TypeNode* arg_type = dynamic_cast<TypeNode*>(*(get<1>(arg)));
      if (arg_type != nullptr)
      {
        if (arg_type->is_poly_type())
          p = true;
      }
      else
      {
        throw "bad type ptr";
      }
    }

    if (def.result_type->is_poly_type())
      p = true;

    poly = p;
  }
  ProcedureDefinition(const ProcedureDefinition& ProcDef)
    : def(ProcDef.def), set(ProcDef.set)
  {
    poly = ProcDef.poly;
  }

  ~ProcedureDefinition() = default;

  ProcedureDefinition& operator=(const ProcedureDefinition& rhs)
  {
    def = rhs.def;
    set = rhs.set;
    poly = rhs.poly;
    return *this;
  }

  string to_string()
  {
    string result;
    result = def.to_string();
    return result;
  }

};

/*
  HasInstance needs to live somewhere,
  the issue is that is coallates
  information from many different
  sources and needs to convey information
  to two distict use cases.

  within the typechecker, we care about the
  side-effect of HasInstance, where it
  adds new valid instances to the ProcedureDefinition.
  and we care about the return value being
  filled or not. in order to judge if this
  is or isn't some valid form of procedure application
  at the call site.

  and within the evaluator, we care about
  the return value being filled, we also
  need to use the returned value to
  start the next step of evaluation,
  we evaluate the body of the returned
  procedure.

  by these constraints, we can satisfy
  characterizing success/failure with
  an optional type. and in the filled
  case we return the Procedure to be
  evaluated. this satisfys most of the
  above constraints.

  in order to check if there is an instance,
  we must utilize the typechecker.
  there is not way around this, we must
  typecheck the resulting instance of
  polymorphic procedures in order to
  confirm their validity.
*/


class Procedure {
public:
  bool contains_literal;
  union U {
    ProcedureLiteral literal;
    ProcedureDefinition definition;

    U() : literal() {}
    U(const ProcedureLiteral& rhs) : literal(rhs) {}
    U(const ProcedureDefinition& rhs) : definition(rhs) {}
    ~U() {};
  }u;

  Procedure() = default;
  Procedure(const ProcedureLiteral& lit)
    : contains_literal(true), u(lit) {}
  Procedure(const ProcedureDefinition& def)
    : contains_literal(false), u(def) {}

  Procedure(const Procedure& rhs)
  {
    if (rhs.contains_literal)
    {
      u.literal = rhs.u.literal;
    }
    else
    {
      u.definition = rhs.u.definiti
      caon;
    }
  }

  Procedure& operator=(const Procedure& rhs)
  {
    if (rhs.contains_literal)
    {
      u.literal = rhs.u.literal;
    }
    else
    {
      u.definition = rhs.u.definition;
    }
    return *this;
  }

  string to_string()
  {
    if (contains_literal)
    {
      return u.literal.to_string();
    }
    else
    {
      return u.definition.to_string();
    }
  }

};




/*
  an entity is (simply put)
  some form considered with it's type.
  the thing i name entity also happens to
  correspond exactly with beta-normal
  forms when looking from a lambda calculus
  perspective, and values held within cells,
  when looking from an assembly perspective.
  again, this mental concept / language concept
  alignment is important, it is one of the
  pilliars of Pink. this alignment i think
  helps people learn the language. it also
  goes deeper, and lets people grok why certain
  sequences of assembly represent peices of
  algorithms and data structures.

  we use a tagged union approach because I want
  entities to represent each of the different
  'things in the language that can be acted upon'.
  as unifying this semnatic location, allows for
  a clean separation between state and behavior
  nodes within the Abstract Syntax Tree.

  theoretically, we could have each of the
  members of the union be a derived class
  right? my brain is just balking and
  having to rethink the implementation logic
  that much during the rewite.
  this way will look more like c, to make
  porting cognitively easier
  but i think
  that could be a good possible future refactor, maybe then,
  we gain some property that makes it easier
  to extend the language with more entities.
  i don't really know. I code to much in c to
  really know. i'm honestly just happy having
  function overloading :)
  i don't want to change much from c in terms
  of implementation of this language, as the
  full brunt of OO and FP are not what we
  are going for within the first kernel, we want a happy medium
  between C and ML, to begin.
  if you view object orientation as simply,
  more complex rules about/surrounding
  composite types. then we are object oriented.
  which is to say, we are not object oriented.
  we are simply borrowing ideas which fit.

*/

enum class EntityValueTag {
  Undef,
  Type,
  Nil,
  Int,
  Bool,
  Proc,
};

class EntityNode : public Ast {
public:
  // the type tag is used to tell if
  // this entity is some actual type,
  // or a polymorphic entity, or an
  // entity whose type is undefined
  // for one of a few reasons,
  // we cannot type said entity at construction time,
  // as is the case of procedures and
  // will be the case of other composite literals.
  // it is the type of an invalid expression.
  // we may use it to reparse expressions
  // in order to support name-use-before-definition.
  // but we need to separate an actual error
  // from a known case, like name-use-before-definition
  // vs. a missing primary expression.
  // vs. a missing terminal token within some term.
  unique_ptr<TypeNode> type;
  // the value tag is to differenciate
  // between union members
  EntityValueTag value_tag;
  union U {
    char nil;
    int  integer;
    bool boolean;
    Procedure procedure;

    U() : nil('\0') {}
    U(const char& c) : nil(c) {}
    U(const int&  i) : integer(i) {}
    U(const bool& b) : boolean(b) {}
    U(const Procedure& ps) : procedure(ps) {}
    U(const ProcedureLiteral& pl) : procedure(pl) {}
    U(const ProcedureDefinition& pd) : procedure(pd) {}
    ~U() {};
  } u;

  ~EntityNode() = default;
  EntityNode()
    : Ast(), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Undef))), value_tag(EntityValueTag::Undef), u(){}

  EntityNode(TypeNode* t, const Location& loc)
    : Ast(loc), value_tag(EntityValueTag::Type), u()
  {
    if (AtomicType* at = dynamic_cast<AtomicType*>(t); at != nullptr)
    {
      type = at->clone();
    }
    else if (ProcType* pt = dynamic_cast<ProcType*>(t); pt != nullptr)
    {
      type = pt->clone();
    }
    else
    {
      throw "bad type pointer\n";
    }
  }

  EntityNode(const AtomicType& t, const Location& loc)
    : Ast(loc), type(t.clone()), value_tag(EntityValueTag::Type) {}

  EntityNode(const ProcType& t, const Location& loc)
    : Ast(loc), type(t.clone()), value_tag(EntityValueTag::Type) {}

  EntityNode(const void* c, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Nil))), value_tag(EntityValueTag::Nil), u('\0')
  {}

  EntityNode(const int& i, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Int))), value_tag(EntityValueTag::Int), u(i)
  {}

  EntityNode(const bool& b, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Bool))), value_tag(EntityValueTag::Bool), u(b)
  {}

  EntityNode(const Procedure& p, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Undef))), value_tag(EntityValueTag::Proc), u(p)
  {}

  EntityNode(const ProcedureLiteral& p, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Undef))), value_tag(EntityValueTag::Proc), u(p)
  {}

  EntityNode(const ProcedureDefinition& p, const Location& loc)
    : Ast(loc), type(make_unique<TypeNode>(AtomicType(PrimitiveType::Undef))), value_tag(EntityValueTag::Proc), u(p)
  {}

  EntityNode(const EntityNode& rhs)
    : Ast(rhs.loc), type(rhs.type->clone()), u()
  {
    // Undef, Type, Nil, Int, Bool
    value_tag = rhs.value_tag;
    switch(value_tag) {
      case EntityValueTag::Type: {
        break;
      }

      case EntityValueTag::Nil: {
        u.nil = rhs.u.nil;
        break;
      }

      case EntityValueTag::Int: {
        u.integer = rhs.u.integer;
        break;
      }

      case EntityValueTag::Bool: {
        u.boolean = rhs.u.boolean;
        break;
      }

      case EntityValueTag::Proc: {
        u.procedure = rhs.u.procedure;
        break;
      }

      default:
        throw "bad entity value tag\n";
    }
  }

protected:
  virtual EntityNode* clone_internal() override { return new EntityNode(*this); }

  virtual string to_string_internal() override {
    string result;
    switch(value_tag) {
      case EntityValueTag::Undef: {
        result = "Undef";
        break;
      }

      case EntityValueTag::Type: {
        result = type->to_string();
        break;
      }

      case EntityValueTag::Nil: {
        result = "nil";
        break;
      }

      case EntityValueTag::Int: {
        result = std::to_string(u.integer);
        break;
      }

      case EntityValueTag::Bool: {
        if (u.boolean == true) {
          result = "true";
        } else {
          result = "false";
        }
        break;
      }

      case EntityValueTag::Proc: {
        result = u.procedure.to_string();
        break;
      }

      default:
        throw "bad entity value tag\n";
    }
    return result;
  }
};
