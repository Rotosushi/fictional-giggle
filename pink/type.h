#pragma once

enum _type {
	_ERR,
	_NONE,
	_MAYBE,
	_INT,
	_FLOAT,
	_TEXT,
	_BOOL,
	_LAMBDA,
	_VAR,
	_DEDUCE,
	_STRUCT,
	_ARRAY,
	_TUPLE,
	
	/* TODO:
		_ARRAY,
		_PTR,
		_STRUCT,
		_UNION,
		_ENUM,
		_TUPLE,
		? _SET,
		_THREAD,
		_MACRO,

	*/
};