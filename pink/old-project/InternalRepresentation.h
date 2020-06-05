#pragma once
#include <string>
using std::string;

#include "ast.h"

enum kind {
	atom,
	fn
};

class Entity {
private:
	string name;
	string type;
	_ast* value;

public:

	string getname() { return name; }
	void setname(string n) { name = n; }
};