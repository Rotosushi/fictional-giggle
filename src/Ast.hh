#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
#include <list>
using std::list;
#include <variant>
using std::variant;
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

// variables are one of the essential
// components of Abstraction, and thusly
// our programming language.
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
    result += " ";
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
  and we need not consider a new operator as
  anything other than a new two argument function
  behind the scenes.
  affix/infix expressions, plus polymorphism,
  operator definitions, and operator overloading,
  and subtyping allow for programmers to build up
  any number of layers on top of one another in
  many different logical ways. each expressing
  different parts of the basic affix expression.
  when we consider a body of executable code, we
  consider at some level, sequencing, selection,
  and iteration over basic entities in the language.

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
  monomorphic type. two possible polymorph types
  are only equivalent if both actually are
  polymorphic. if we are comparing two
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

enum class PrimitiveType {
  Nil,
  Int,
  Bool,
};

class MonoType : public Type {
public:
  PrimitiveType primtype;

  MonoType() = delete;
  MonoType(PrimitiveType pt) : Type(), primtype(pt) {}
  MonoType(MonoType& mt) : Type(), primtype(mt.primtype) {}

protected:
  virtual MonoType* clone_internal() override { return new MonoType(*this); }

  virtual string to_string_internal() override {
    string result;
    switch(primtype) {
      case PrimitiveType::Nil: {
        result = "Nil";
      }

      case PrimitiveType::Int: {
        result = "Int";
      }

      case PrimitiveType::Bool: {
        result = "Bool";
      }

      default:
        throw "malformed MonoType tag.\n";
    }
    return result;
  }
};

class PolyType : public Type {
public:
  PolyType() : Type() {}
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

  ProcType() = delete;
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

class Procedure {
public:
  string           arg_id;
  unique_ptr<Ast> arg_type;
  unique_ptr<Ast>  body;

  Procedure();
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

class ProcSet {
public:
  Procedure def;
  list <Procedure> set;
  bool polymorphic;

  ProcSet();
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

  we use a tag in addition to a type field so that I can
  write a ToString function without having to
  call the typechecker. and the Type field is
  used by the typechecker and evaluator.

  theoretically, we could have each of the
  members of the variant be a derived class
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
  of implementation of this language.
*/
class Entity : public Ast {
public:
  Type    type;
  EntityTag tag;
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
  Entity() = delete;
  Entity(const Type& t, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Type), u('\0') {}

  Entity(const Type& t, const char& c, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Nil), u(c) {}

  Entity(const Type& t, const int& i, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Int), u(i) {}

  Entity(const Type& t, const bool& b, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Bool), u(b) {}

  Entity(const Type& t, const Procedure& p, bool poly, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Proc), u((*(new ProcSet(p, poly)))) {}

  Entity(const Type& t, const ProcSet& p, const Location& loc)
    : Ast(loc), type(t), tag(EntityTag::Proc), u(p) {}

  Entity(const Entity& rhs)
  : Ast(rhs.loc), u('\0') {
    tag = rhs.tag;
    switch(tag) {
      case EntityTag::Nil: {
        u.nil = rhs.u.nil;
      }

      case EntityTag::Int: {
        u.integer = rhs.u.integer;
      }

      case EntityTag::Bool: {
        u.boolean = rhs.u.boolean;
      }

      case EntityTag::Proc: {
        u.procedure = rhs.u.procedure;
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
        result = type.to_string();
      }
      case EntityTag::Nil: {
        result = "nil";
      }

      case EntityTag::Int: {
        result = std::to_string(u.integer);
      }

      case EntityTag::Bool: {
        if (u.boolean == true) {
          result = "true";
        } else {
          result = "false";
        }
      }

      case EntityTag::Proc: {
        ProcSet& p = u.procedure;
        result  = "\\ ";
        result += p.def.arg_id;
        result += " : ";
        result += p.def.arg_type->to_string();
        result += " => ";
        result += p.def.body->to_string();
      }
    }
    return result;
  }
};
