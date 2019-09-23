#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;

enum _type {
	S_INT,
	S_FLOAT,
	S_STRING,
	S_BOOL,
	S_VAR,
	// i think, also: S_POLYMORPH_VAR
	S_FUNCTION,
	S_LAMBDA,
};

class _symbol {
public:
	string id;
	_type type;
};



