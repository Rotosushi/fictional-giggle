#pragma once
#include <utility>
using std::pair;
#include <vector>
using std::vector;
#include <unordered_map>
using std::unordered_map;


#include "InternalRepresentation.h"
#include "symset.h"

class Argument : public Entity {
	string type;
public:
	string gettype() { return type; }
	void settype(string& t) { type = t; }
};

typedef pair<vector<Argument>, vector<Argument>> FunctionType;

class Function : public Entity {
	FunctionType type;
	unordered_map<string, Entity> symbol_table;
	_ast* body;

public:
	FunctionType gettype() { return type; }
	void settype(FunctionType& t) { type = t; }
};

