#pragma once

#include "InternalRepresentation.h"
#include "symset.h"

#include "Function.h"

enum _type {
	/* internal use */
	_ERR,
	_DEDUCE,
	_FCALL,
	/* usable types */
	_NIL,
	_INT,
	_REAL,
	_TEXT,
	_BOOL,

	/* TODO:
		_LAMBDA,
		_ARRAY,
		_PTR,
		_RECORD,
		_ENUM,
		_TUPLE,
		? _SET,
		_THREAD,
		_MACRO,
		_USERTYPE,

	*/
};

string to_string(_type t);


class Type : public Entity {
	// "the type of an atom is defined by the valid
	//  set of operations upon it"
	_ast* type_expression;
};

