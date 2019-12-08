#pragma once
#include <string>
using std::string;
#include <unordered_map>
using std::unordered_map;
#include <unordered_set>
using std::unordered_set;

// internal representation
#include "Entity.h"
#include "Atom.h"
#include "Function.h"

// utilities
#include "symset.h"

class Module : public Entity {
	// symbols can be any entity in the language.
	unordered_map<string, Entity> symbol_table;

public:

};