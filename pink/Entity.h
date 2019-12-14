#pragma once
#include <string>
using std::string;


#include "type.h"
#include "ast.h"

enum _entity_type {
	E_ERR,
	E_MODULE,
	E_ATOM,
	E_FUNCTION,
};

typedef struct Entity {
	_entity_type entity_type;
	string name;
	_type type;
	_ast* value;

	Entity() : entity_type(E_ERR), name(), type(_ERR), value(nullptr) {}
	Entity(_entity_type e_t) { entity_type = e_t; }
} Entity;

typedef struct Module : public Entity {

	Module() { entity_type = E_MODULE; }
} Module;

typedef struct Atom : public Entity {

	Atom() { entity_type = E_ATOM; }
} Atom;

typedef struct Function : public Entity {

	Function() { entity_type = E_FUNCTION; }
} Function;

//
//class Type : public Entity {
//	 "the type of an atom is defined by the valid
//	  set of operations upon it"
//	_ast* type_expression;
//};
//
//class Entity {
//private:
//	string name;
//	Type type;
//	Entity* value;
//	unsigned int flags;
//
//public:
//	string getname() { return name; }
//	void setname(string n) { name = n; }
//
//	Type gettype() { return type; }
//	void settype(Type t) { type = t; }
//
//	Entity* getvalue(){ return value; }
//	void setvalue() { Entity* value; }
//
//	unsigned int getflags() { return flags; }
//	void setflags(unsigned int f) { flags = f; }
//};
//
//
//
//class Module : public Entity {
//	 symbols can be any entity in the language.
//	unordered_map<string, Entity> symbol_table;
//
//public:
//
//};
//
//
//class Atom : public Entity {
//
//public:
//};
//
//
//class Argument : public Entity {
//	string type;
//public:
//	string gettype() { return type; }
//	void settype(string& t) { type = t; }
//};
//
//typedef pair<vector<Argument>, vector<Argument>> FunctionType;
//
//class Function : public Entity {
//	
//public:
//	FunctionType gettype() { return type; }
//	void settype(FunctionType& t) { type = t; }
//};
//
