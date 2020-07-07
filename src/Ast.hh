
#include <string>
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
#include <list>
using std::list;

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

  ~Location() {
    first_line   = 0;
    first_column = 0;
    last_line    = 0;
    last_column  = 0;
  }
};

class AstVisitor;

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
  Ast(Location& loc) : loc(loc) {}
};

// variables are one of the essential
// components of Abstraction, and thusly
// our programming language.
class Variable : public Ast {
public:
  string id;

  Variable(const string& str) : Ast(), id(str) {}
  Variable(const string& str, const Location& loc) : Ast(loc), id(str) {}
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
};

// a bind expression is an essential component
// in making the lambda calculus usable.
// it is innate in the definition of the
// calculus, however it has traditionally
// considered a 'meta-operation' to be
// performed by the human-computer evaluating
// the mathematical expression in the lambda calculus.
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
 and basically, so the language doesn't become
 a turing tar-pit. getting things done is the
 constant and pressing goal, clarity and
 communication help to facilitate that goal.
 in fact, as we consider scaling, clarity of
 design and of communication become paramount
 in my opinion.), we
 must be given some other framework for
 specifying expressions to then subsequently
 abstract over. now, given the complicated
 nature of what one must specify in some
 programming language, one needs a powerful
 abstraction to specify programs. luckily we
 already know what it is. and again, it has classically
 not been considered formally, mathematitions
 essentially do the equivalent of "#include basic-arithmetic"
 into lambda calculus definitions. to allow them
 the freedom to include numers and addition and such.
 in reality the abstraction of the infix expression
 is very versatile and mathematically sound, what
 makes it a slam dunk for me is if we consider two
 features, operator overloading and operator declarations.
 these two make infix expressions very expressive indeed.
 if we consider then, subtyping within our type definition
 mechanisms, we have something that is approaching
 production quality. all that would be required
 to push such a language over the top is modules,
 and a solid standard library. (all of which is
 non-trival.)
*/
class Binop : public Ast {
public:
  string          op;
  unique_ptr<Ast> lhs;
  unique_ptr<Ast> rhs;

  Binop(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r) : Ast(), op(str), lhs(move(l)), rhs(move(r)) {}
  Binop(const string& str, unique_ptr<Ast> l, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), lhs(move(l)), rhs(move(r)) {}

};

class Unop : public Ast {
public:
  string op;
  unique_ptr<Ast>   rhs;

  Unop(const string& str, unique_ptr<Ast> r) : Ast(), op(str), rhs(move(r)) {}
  Unop(const string& str, unique_ptr<Ast> r, const Location& loc) : Ast(loc), op(str), rhs(move(r)) {}

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
*/


class Cond : public Ast {
public:
  unique_ptr<Ast> test;
  unique_ptr<Ast> first;
  unique_ptr<Ast> second;

  Cond(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s) : Ast(), test(move(t)), first(move(f)), second(move(s)) {}
  Cond(unique_ptr<Ast> t, unique_ptr<Ast> f, unique_ptr<Ast> s, const Location& loc) : Ast(loc), test(move(t)), first(move(f)), second(move(s)) {}

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
class MonoType;
class PolyType;
class ProcType;

class Type {
public:
  Location loc;

  Type() = delete;
  Type(const Location& loc) : loc(loc) {}
};

enum class PrimitiveType {
  Nil,
  Int,
  Bool,
};

class MonoType : public Type {
public:
  PrimitiveType primitivetype;

  MonoType() = delete;
  MonoType(PrimitiveType pt) : loc(), primitivetype(pt) {}
  MonoType(PrimitiveType pt, const Location& loc) : loc(loc), primitivetype(pt) {}
  MonoType(MonoType& mt) : loc(mt.loc), primitivetype(mt.primitivetype) {}
};

class PolyType : Type {
public:
  PolyType() : loc() {}
  PolyType(const Location& loc) : loc(loc) {}
  PolyType(PolyType& pt) : loc(pt.loc) {}
};

class ProcType : Type {
public:
  unique_ptr<Type> lhs;
  unique_ptr<Type> rhs;

  ProcType() = delete;
  ProcType(ProcType& pt) : loc(pt.loc), lhs(pt.lhs), rhs(pt.rhs) {}
  ProcType(unique_ptr<Type> l, unique_ptr<Type> r) : loc(), lhs(move(l)), rhs(move(r)) {}
  ProcType(unique_ptr<Type> l, unique_ptr<Type> r, const Location& loc) : loc(loc), lhs(move(l)), rhs(move(r)) {}
};

class Procedure {
public:
  Location         loc;
  string           arg_id;
  unique_ptr<Type> arg_type;
  unique_ptr<Ast>  body;

  Procedure() = delete;
  Procedure(const string& str, unique_ptr<Type> at, unique_ptr<Ast> b)
    : Ast(), arg_id(str), arg_type(move(at)), body(move(b)) {}

  Procedure(const string& str, unique_ptr<Type> at, unique_ptr<Ast> b, const Location& loc)
    : Ast(loc), arg_id(str), arg_type(move(at)), body(move(b)) {}

  Procedure(const Procedure& p)
    : Ast(p.loc), arg_id(p.arg_id), arg_type(p.arg_type), body(p.body) {}
};

class ProcSet {
public:
  Procedure def;
  list <Procedure> set;
  bool polymorphic;

  ProcSet() = delete;
  ProcSet(const Procedure& p, bool poly) : Ast(), def(p), set(), polymorphic(poly) {}
  ProcSet(const Procedure& p, bool poly, const Location& loc) : Ast(loc), def(p), set(), polymorphic(poly) {}
  ProcSet(const ProcSet& ps) : Ast(ps.loc), def(ps.def), set(ps.set), polymorphic(ps.polymorphic) {}
};


enum class EntityTag {
  Nil,
  Int,
  Bool,
  Proc,
};

/*
  an entity is simply put some form considered
  with it's type. these also happen to
  correspond exactly with beta-normal
  forms when looking from a lambda calculus
  perspective, and values held within cells,
  when looking from an assembly perspective.
  again, this mental/internal alignment is important.
*/
class Entity : public Ast {
public:
  Type    type;
  EntityTag tag;
  union {
    char    Nil;
    int     Int;
    bool    Bool;
    ProcSet Proc;
  };

  Entity() = delete;
  Entity(const Type& t, const char& c, const Location& loc)
    : Ast(loc), type(t), Nil(c) {
      tag = EntityTag::Nil;
    }

  Entity(const Type& t, const int& i, const Location& loc)
    : Ast(loc), type(t), Int(i) {
      tag = EntityTag::Int;
    }

  Entity(const Type& t, const bool& b, const Location& loc)
    : Ast(loc), type(t), Bool(b) {
      tag = EntityTag::Bool;
    }

  Entity(const Type& t, const Procedure& p, bool poly, const Location& loc)
    : Ast(loc), type(t), Proc(p, poly) {
      tag = EntityTag::Proc;
    }

  Entity(const Type& t, const ProcSet& p, const Location& loc)
    : Ast(loc), type(t), Proc(p) {
      tag = EntityTag::Proc;
    }
};

string AstToString(const Ast& a);
