#pragma once
#include <string>
using std::string;

#include "InternalRepresentation.h"
#include "symset.h"
#include "type.h"
#include "ast.h"

class Atom : public Entity {
	string type;
	_ast* value;
public: 
	string gettype() { return type; }
	void settype(string& t) { type = t; }
};