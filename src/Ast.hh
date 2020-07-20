#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
#include <list>
using std::list;
using std::get;

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

// for holding string locations.
class Location {
public:
  int first_line;
  int first_column;
  int last_line;
  int last_column;


  Location() {
    first_line   = 0;
    first_column = 0;
    last_line    = 0;
    last_column  = 0;
  }

  Location(int fl, int fc, int ll, int lc) {
    first_line   = fl;
    first_column = fc;
    last_line    = ll;
    last_column  = lc;
  }

  Location(const Location& loc) {
    first_line   = loc.first_line;
    first_column = loc.first_column;
    last_line    = loc.last_line;
    last_column  = loc.last_column;
  }

  Location& operator=(const Location& rhs) {
    first_line   = rhs.first_line;
    first_column = rhs.first_column;
    last_line    = rhs.last_line;
    last_column  = rhs.last_column;
    return *this;
  }

  ~Location() {
    first_line   = 0;
    first_column = 0;
    last_line    = 0;
    last_column  = 0;
  }
};


/*
  this is the main data-type
  which will be used to allow
  us to describe the Ast in terms
  of a type-heirarchy instead of
  an explicit tagged union.

  this should allow more conveinent
  syntax within traversal actions.

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
  for evaluation or typechecking.

  basically, when we call clone on some arbitrary
  Ast, we dispatch to the clone of the right
  derived class, which deep copies, then returns
  the copy through the auto return type..
  when we later copy constructors, we
  get a deep-copy of the tree instead of
  a shared refrence. (this function "clone_interal"
  is extended through covariance, as it's
  return type can vary by derived class.
  and that is okay precisely because it is
  extended over derived classes, which means
  every return type is at least an Ast.)

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
class Empty : public Ast {
public:

  Empty() {};
  Empty(const Location& loc) : Ast(loc) {}
  Empty(const Empty& e) : Ast(e.loc) {}

protected:
  virtual Empty* clone_internal() override { return new Empty(*this); }
  virtual string to_string_internal() override {
    return "";
  }
};

/*
  variables are one of the essential
  components of Abstraction, and thusly
  our programming language.
*/
class Variable : public Ast {
public:
  string id;

  Variable(const string& str) : Ast(), id(str) {}
  Variable(const string& str, const Location& loc) : Ast(loc), id(str) {}
  Variable(const Variable& r) : Ast(r.loc), id(r.id) {}

protected:
  virtual Variable* clone_internal() override { return new Variable(*this); }

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
class Call : public Ast {
public:
  unique_ptr<Ast> lhs;
  unique_ptr<Ast> rhs;

  Call(unique_ptr<Ast> l, unique_ptr<Ast> r) : Ast(), lhs(move(l)), rhs(move(r)) {}
  Call(unique_ptr<Ast> l, unique_ptr<Ast> r, const Location& loc) : Ast(loc), lhs(move(l)), rhs(move(r)) {}
  Call(const Call& c) : Ast(c.loc), lhs(c.lhs->clone()), rhs(c.rhs->clone()) {}

protected:
  virtual Call* clone_internal() override { return new Call(*this); }

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
    result  = "(";
    result += lhs->to_string();
    result += " ";
    result += rhs->to_string();
    result += ")";
    return result;
  }
};

// a bind expression is an essential component
// in making the lambda calculus usable.
// it is innate in the definition of the
// calculus, however it has traditionally
// considered a 'meta-operation' to be
// performed by the human-computer evaluating
// the mathematical expression of the lambda calculus.
// but this action is so basic (bind a name to some
// value), that it is in fact one of the central
// actions of the calculus itself (arguments).
// so why early lambda calculus didn't consider
// name binding formally eludes me (other than
// plain human lazyness, but that isn't very charitable.)
class Bind : public Ast {
public:
  string          id;
  unique_ptr<Ast> rhs;

  Bind(const string& str, unique_ptr<Ast> r) : Ast(), id(str), rhs(move(r)) {}
  Bind(const string& str, unique_ptr<Ast> r, const Location& loc) : Ast(loc), id(str), rhs(move(r)) {}
  Bind(const Bind& rhs) : Ast(rhs.loc), id(rhs.id), rhs(rhs.rhs->clone()) {}

protected:
  virtual Bind* clone_internal() override { return new Bind(*this); }

  virtual string to_string_internal() override {
    string result;
    result  = id;
    result += " := ";
    result += rhs->to_string();
    return result;
  }
};

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
 in my opinion,.),
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
class Binop : public Ast {
public:
  string          op;
  unique_ptr<Ast> lhs;
  unique_ptr<Ast> rhs;

  Binop(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r) : Ast(), op(str), lhs(move(l)), rhs(move(r)) {}
  Binop(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), lhs(move(l)), rhs(move(r)) {}
  Binop(const Binop& rhs) : Ast(rhs.loc), op(rhs.op), lhs(rhs.lhs->clone()), rhs(rhs.rhs->clone()) {}

protected:
  virtual Binop* clone_internal() override { return new Binop(*this); }

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
  parser subroutine by way of a map. whereby
  we can associate the beginning string denoting
  some binary operation with the ending string
  denoting the end of the term by looking up the
  strings in the map, and using it's existance in
  the map structure to validate.
  we could use a multimap and set the keys to be
  the beginning and end tokens, but why not
  make it a map from the beginning token to the
  ending token? then the procedure can 'parse' against
  the ending token, regardless of what the text associated
  with the operator is. which is what allows for extension.
  (we then also need a way to lookup the operation,
   so which name do we use? maybe we do the c++ way, and
   the name of the postfix operation is the beginning and
   ending tokens concatenated together?)

   something to note is that postfix operators take precedence
   over binary operators, which to me means that we want to
   ensure that we query against the set of postfix operations
   before we look for the next binary operation when we
   parse some given primary. which is the essence of
   higher precedence.
*/
class Unop : public Ast {
public:
  string op;
  unique_ptr<Ast>   rhs;

  Unop(const string& str, unique_ptr<Ast> r) : Ast(), op(str), rhs(move(r)) {}
  Unop(const string& str, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), rhs(move(r)) {}
  Unop(const Unop& rhs) : Ast(rhs.loc), op(rhs.op), rhs(rhs.rhs->clone()) {}

protected:
  virtual Unop* clone_internal() override { return new Unop(*this); }

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
  of the same coin.
  these primitive abstractions, I think
  are expressive enough to grow alongside
  the rest of the language. this allows
  for functional programming alongside
  imperitive programming.
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
  and iteration over basic entities in the language.
  we then have ways of composing and decomposing
  entities from/into their consituent parts.
  subtyping seems to interfere with overloading,
  but if some object of a subtype is valid
  in all instances of where its supertype is valid,
  as subtyping would seem to imply to me, does it
  not make sense to simply pass the subtype to
  some function expecting a supertype?
  because the only manipulations
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


class Cond : public Ast {
public:
  unique_ptr<Ast> test;
  unique_ptr<Ast> first;
  unique_ptr<Ast> second;

  Cond(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s) : Ast(), test(move(t)), first(move(f)), second(move(s)) {}
  Cond(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s, const Location& loc) : Ast(loc), test(move(t)), first(move(f)), second(move(s)) {}
  Cond(const Cond& rhs) : Ast(rhs.loc), test(rhs.test->clone()), first(rhs.first->clone()), second(rhs.second->clone()) {}

protected:
  virtual Cond* clone_internal()  override { return new Cond(*this); }

  virtual string to_string_internal() override {
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


/*
  The type heirarchy of Pink is
  intentionaly simple. some type is
  either monomorphic, polymorphic,
  or a procedure type. if we are
  comparing two types, we can
  only truly consider exact matches
  here. two monomorphic types are only
  equivalent if they represent the same
  monomorphic type. two entities of polymorphic type
  are never equivalent. if we are comparing two
  procedure types, we must compare both sides,
  and both sides must be equivalent.
*/

class Type {
public:
  Type() {}
  virtual ~Type() = default;

protected:
  virtual Type* clone_internal() { return new Type(); }
  virtual string to_string_internal() { return ""; }

public:
  auto clone() { return unique_ptr<Type>(clone_internal()); }
  string to_string() { return to_string_internal(); }
};

/*  this is an internal type, which is
    the type of entities that are not
    immediately typeable by the parser,
    which is only procedures in this version.
    this is anything that isn't atomically
    some type. and literal is typeable
    once it has been parsed.
*/
class UndefType : public Type {
public:
  UndefType() : Type() {}

protected:
  UndefType* clone_internal() override {
    return new UndefType(*this);
  }

  string to_string_internal() override {
    return "Undef";
  }

public:
  auto clone() { return unique_ptr<Type>(clone_internal()); }
  string to_string() { return to_string_internal(); }
};

enum class AtomicType {
  Nil,
  Int,
  Bool,
};

class MonoType : public Type {
public:
  AtomicType type;

  MonoType() = delete;
  MonoType(AtomicType pt) : Type(), type(pt) {}
  MonoType(MonoType& mt) : Type(), type(mt.type) {}

protected:
  virtual MonoType* clone_internal() override { return new MonoType(*this); }

  virtual string to_string_internal() override {
    string result;
    switch(type) {
      case AtomicType::Nil: {
        result = "Nil";
        break;
      }

      case AtomicType::Int: {
        result = "Int";
        break;
      }

      case AtomicType::Bool: {
        result = "Bool";
        break;
      }

      default:
        throw "malformed MonoType tag.\n";
    }
    return result;
  }
};

class PolyType : public Type {
public:
  PolyType() {}
  PolyType(PolyType& pt) : Type() {}

protected:
  virtual PolyType* clone_internal() override { return new PolyType(*this); }

  virtual string to_string_internal() override {
    string result;
    result = "Poly";
    return result;
  }
};

class ProcType : public Type {
public:
  unique_ptr<Type> lhs;
  unique_ptr<Type> rhs;

  ProcType() {}
  ProcType(ProcType& pt) : Type(), lhs(pt.lhs->clone()), rhs(pt.rhs->clone()) {}
  ProcType(unique_ptr<Type> l, unique_ptr<Type> r) : Type(), lhs(move(l)), rhs(move(r)) {}

protected:
  virtual ProcType* clone_internal() override { return new ProcType(*this); }

  virtual string to_string_internal() override {
    string result;
    result  = "(";
    result += lhs->to_string();
    result += " -> ";
    result += rhs->to_string();
    result += ")";
    return result;
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
*/
class Procedure {
public:
  string          arg_id;
  unique_ptr<Ast> arg_type;
  unique_ptr<Ast> body;

  Procedure() {}
  Procedure(const string& str, unique_ptr<Ast> at, unique_ptr<Ast> b)
    : arg_id(str), arg_type(move(at)), body(move(b)) {}

  Procedure(const Procedure& p)
    : arg_id(p.arg_id), arg_type(p.arg_type->clone()), body(p.body->clone()) {}

  Procedure& operator=(const Procedure& rhs)
  {
    arg_id   = rhs.arg_id;
    arg_type = rhs.arg_type->clone();
    body     = rhs.body->clone();
    return *this;
  }
};

/*
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
class ProcSet {
public:
  Procedure def;
  list <Procedure> set;
  bool polymorphic;

  ProcSet() {};
  ProcSet(const Procedure& p, bool poly) : def(p), set(), polymorphic(poly) {}
  ProcSet(const Procedure& p, bool poly, const Location& loc) : def(p), set(), polymorphic(poly) {}
  ProcSet(const ProcSet& ps) : def(ps.def), set(ps.set), polymorphic(ps.polymorphic) {}

  ProcSet& operator=(const ProcSet& rhs)
  {
    def = rhs.def;
    set = rhs.set;
    polymorphic = rhs.polymorphic;
    return *this;
  }
};

enum class EntityTag {
  Type,
  Nil,
  Int,
  Bool,
  Proc,
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
  alignment is important.

  we use a tagged union approach because I want
  entities to represent each of the different
  'things in the language that can be acted upon'.
  as unifying this semnatic location, allows for
  a clean separation between state and behavior.

  theoretically, we could have each of the
  members of the union be a derived class
  right? my brain is just balking and
  having to rethink the implementation logic
  that much during the rewite.
  this way will look more like c, but i think
  that could be a good refactor, maybe then,
  we gain some property that makes it easier
  to extend the language with more entities.
  i don't really know. I code to much in c to
  really know. i'm honestly just happy having
  function overloading :)
  i don't want to change much from c in terms
  of implementation of this language, as the
  full brunt of OO and FP are not what we
  are going for, we want a happy medium
  between C and ML, to begin.
*/
class Entity : public Ast {
public:
  unique_ptr<Type> type;
  EntityTag        tag;
  union U {
    char nil;
    int  integer;
    bool boolean;
    ProcSet procedure;

    U(const char& c) : nil(c) {}
    U(const int&  i) : integer(i) {}
    U(const bool& b) : boolean(b) {}
    U(const ProcSet& ps) : procedure(ps) {}
    ~U() {};
  } u;

  ~Entity() = default;
  Entity() : u('\0') {}
  Entity(unique_ptr<Type> t, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Type), u('\0') {}

  Entity(unique_ptr<Type> t, const char& c, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Nil), u(c) {}

  Entity(unique_ptr<Type> t, const int& i, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Int), u(i) {}

  Entity(unique_ptr<Type> t, const bool& b, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Bool), u(b) {}

  Entity(unique_ptr<Type> t, const Procedure& p, bool poly, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Proc), u((*(new ProcSet(p, poly)))) {}

  Entity(unique_ptr<Type> t, const ProcSet& p, const Location& loc)
    : Ast(loc), type(move(t)), tag(EntityTag::Proc), u(p) {}

  Entity(const Entity& rhs)
  : Ast(rhs.loc), type(move(rhs.type->clone())), u('\0') {
    tag = rhs.tag;
    switch(tag) {
      case EntityTag::Type: {
        break;
      }

      case EntityTag::Nil: {
        u.nil = rhs.u.nil;
        break;
      }

      case EntityTag::Int: {
        u.integer = rhs.u.integer;
        break;
      }

      case EntityTag::Bool: {
        u.boolean = rhs.u.boolean;
        break;
      }

      case EntityTag::Proc: {
        u.procedure = rhs.u.procedure;
        break;
      }

      default:
        throw "malformed entity tag!";
    }
  }

protected:
  virtual Entity* clone_internal() override { return new Entity(*this); }

  virtual string to_string_internal() override {
    string result;
    switch(tag) {
      case EntityTag::Type: {
        result = type->to_string();
        break;
      }

      case EntityTag::Nil: {
        result = "nil";
        break;
      }

      case EntityTag::Int: {
        result = std::to_string(u.integer);
        break;
      }

      case EntityTag::Bool: {
        if (u.boolean == true) {
          result = "true";
        } else {
          result = "false";
        }
        break;
      }

      case EntityTag::Proc: {
        ProcSet& p = u.procedure;
        result  = "\\ ";
        result += p.def.arg_id;
        result += " : ";
        result += p.def.arg_type->to_string();
        result += " => ";
        result += p.def.body->to_string();
        break;
      }
    }
    return result;
  }
};
