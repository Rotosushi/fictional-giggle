#pragma once

enum _type {
	/* internal use */
	_ERR,
	_INFER,
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



