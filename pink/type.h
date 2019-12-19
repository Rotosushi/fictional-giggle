#pragma once
#include <string>
using std::string;
#include "ast.h"

typedef struct _type {
	string name;
	_ast*  expr;

	// what if you request the compiler generate these functions
	// for you, instead of having to define them yourself.
	// (the 'cannonical class')
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
	// if i never use it then it doesn't need to be output.
	_type() {}
	_type(string n, _ast* e) : name(n), expr(e) {}

	_type operator=(_type& rhs) {
		name = rhs.name;
		expr = rhs.expr;
		return *this;
	}
} _type;

/*
any regular type is
	"nil" _ast* -> nullptr
	"int" _ast* -> _int()
	"real" _ast* -> _real()
	"text" _ast* -> _text()
	"bool" _ast* -> _bool()

a user type is
	"user-typename" _ast* -> _user_type_definition_or_constructor()

pros:
	- user-types and primitive types having the same shape simplifies
		the type system somewhat.

	- functions can be 
	"fn-name" _ast* -> _tuple(_list(_arg), _list(_arg))

cons:
	- it is now one/more string compares instead of
	  int comparisons for type inference.
	  (user types were always going to be implemented somewhat like this)
*/



