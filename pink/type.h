#pragma once
#include <string>
using std::string;
#include "ast.h"

typedef struct _type {
	// the name of the type is how we disambiguate between types
	// in the compiler.
	string name;
	// the expr is a refrence the to definition of the type.
	// this may or may not be the most effective way. but
	// anytime you have a variable of the type it is probable
	// that you would need the definition as well. but maybe
	// the variable only needs to store the name of its type.
	// the kernel will define the primitive types. in
	// the eventual translation of pink into pink',
	// i think pink' will lack any 'int' base type, but,
	// should include some 'word' base type, which can be
	// the primitive which 'int' and 'float' are based on,
	// and some primitive 'byte' keyword which can be the
	// primitive which 'char', and therefore 'text' is based on, and more.
	// the programmers, in this case, are those writing the kernel.
	// the user of pink would similarly be able to define types using
	// word, or byte, or bit, and their type would then be able to
	// take advantage of any bit/byte packing we can do in the target
	// code, implicitly.
	_ast*  expr;

	// what if you request the compiler generate these functions
	// for you, instead of having to define them yourself.
	// think about the combination of macros and reflection,
	// this could make most boilerplate code something that
	// the compiler can generate, which means:
	// a) all functions are always up to date with the latest definition of
	//    the data-type.
	// b) they can be defined once semantically by the language
	//    maintainer and the macro expransion + reflection will do the work of 
	//    structural analysis of the type. meaning that both maintinence and
	//    use of the type is taken care of by the language itself.
	// in my mind, this has the effect of eliminating entire classes of bug
	// from occurring in the resulting language. in much the same way that
	// static type analysis catches an entire class of bug. this reduces the
	// labor required to write programs because the boilerplate interactions
	// between data and the program are already well understood by the language.
	// (the 'cannonical class' functions, i.e. construction,
	//  destruction, assignment.)
	// (in haskell this is done with deriving(),
	//  in c++ you specify them explicitly)
	// because they all follow a common semantic pattern, i'm
	// sure a macro could be constructed here.
	// which could define a type constructor function,
	// that had each type in the argument-list in the
	// order in which they appear in the declaration
	// and with the default constructor defined as the
	// default argument binding. so one gets a
	// constructor with some degree of flexibility
	// over the way you can call the function for 'free'
	// (r: 'free' -> 'deduced/inferred/(defined by the compiler automagically)')
	// also as in, when i need it it needs to be in the executable,
	// if i never use it then it doesn't need to be in the executable.
	// and that fact is something that can be deduced via static analysis.
	// but what, you may ask, about all the fun pointer tricks (move assignemnt)
	// that can make assignment faster? well, i feel like the
	// compiler could deduce the use of those via static analysis.
	// and they could then be reintroduced as a possible optimization.
	_type() {}
	_type(string n, _ast* e) : name(n), expr(e) {}

	_type operator=(_type& rhs) {
		name = rhs.name;
		expr = rhs.expr;
		return *this;
	}
} _type;

/*
any primitive type is
	"nil" _ast* -> nullptr
	"int" _ast* -> _int()
	"real" _ast* -> _real()
	"text" _ast* -> _text()
	"bool" _ast* -> _bool()

a user type is
	"user-typename" _ast* -> _user_type_constructor()

pros:
	- user-types and primitive types having the same shape simplifies
		the type system somewhat.

	- functions can be encoded directly with
	"fn-name" _ast* -> _tuple(_list(_arg), _list(_arg))

cons:
	- it is now one/more string compares instead of
	  int comparisons for type inference.
	  (user types were always going to be implemented somewhat like this,
	   because the user string has to be the typename.)
*/



