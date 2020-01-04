#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <map>
using std::map;

enum class Ast_tag {
	ERR,
	ENTITY,
	TYPE,
};


class Ast {
public:
	Ast_tag ast_tag;

	Ast() : ast_tag(Ast_tag::ERR) {}
	Ast(Ast_tag tag) : ast_tag(tag) {}
	virtual ~Ast() = 0;

private:
};

class Type : public Ast {
public:
	string name;
	Ast*   expr;

	Type() : Ast(Ast_tag::TYPE), name(), expr(nullptr) {}
	Type(string n, Ast* e) : Ast(Ast_tag::TYPE), name(n), expr(e) {}
	Type(const Type& t) : Ast(Ast_tag::TYPE), name(t.name), expr(t.expr) {}

	Type operator=(const Type& rhs) {
		name = rhs.name;
		expr = rhs.expr;
		return *this;
	}
private:
};

enum class Entity_tag {
	ERR,
	VAR,
	DECL,
	ARG,
	FN,
	ACT,
	SCOPE,
	ADT,
	TUPLE,
	IF,
	WHILE,
	RETURN,
	EXPR,
	BINOP,
	UNOP,
	POSTOP,
	LITERAL,
};

class Entity {
public:
	Entity_tag entity_tag;
	string name;
	Type   type;
	Ast*  value;

	Entity() :  name(), type(), value(nullptr) {}
	Entity(Entity_tag e_t, string n = "", Type t = Type( "", nullptr ), Ast* v = nullptr) : Ast(Ast_tag::_ENTITY), name(n), type(t), value(v) {}
	Entity(const Entity& v) : name(v.name), type(v.type), value(v.value) {}

	Entity operator=(const Entity& rhs) {
		name = rhs.name;
		type = rhs.type;
		value = rhs.value;
		return *this;
	}

	virtual string typeof() { return type.name; }
private:
};

class Literal : public Entity {
public:
	Literal() : Entity(Entity_tag::LITERAL) {}
	Literal(string n, Type t, Ast* v) : Entity(Entity_tag::LITERAL, n, t, v) {}
	Literal(const Literal& l) : Entity(Entity_tag::LITERAL, l.name, l.type, l.value) {}
private:
};

class Var : public Entity {
public:
	Var() : Entity(Entity_tag::VAR) {}
	Var(string n, Type t, Ast* v) : Entity(Entity_tag::VAR, n, t, v) {}
	Var(const Var& v) : Entity(Entity_tag::VAR, v.name, v.type, v.value) {}
private:
};

class Decl : public Entity {
public:
	string op;
	Ast* init;

	Decl() : Entity(Entity_tag::DECL), op(), init(nullptr) {}
	Decl(string n, Type t, Ast* v, string o, Ast* i) :
		Entity(Entity_tag::VAR, n, t, v), op(o), init(i) {}
	Decl(const Decl& d) :
		Entity(Entity_tag::VAR, d.name, d.type, d.value), op(d.op), init(d.init) {}

	Decl operator=(const Decl& d) {
		name  = d.name;
		type  = d.type;
		value = d.value;
		op    = d.op;
		init  = d.init;
		return *this;
	}
private:
};

class Scope : public Entity {
public:
	map<string, Decl> symbol_table;
	vector<Entity*> statements;

	Scope() : Entity(Entity_tag::SCOPE) {}
	Scope(map<string, Decl> s_t, vector<Entity*> stmts) :
		Entity(Entity_tag::SCOPE), symbol_table(s_t), statements(stmts) {}
	Scope(const Scope& s) : 
		Entity(Entity_tag::SCOPE), symbol_table(s.symbol_table), statements(s.statements) {}


};

class Arg : public Entity {
public:
	Arg() : Entity(Entity_tag::ARG) {}
	Arg(string n, Type t, Ast* v) : Entity(Entity_tag::ARG, n, t, v) {}
	Arg(const Arg& a) : Entity(Entity_tag::ARG, a.name, a.type, a.value) {}
};

class Fn : public Entity {
public:
	Fn() : Entity(Entity_tag::FN) {}
	Fn(string n, Type t, Ast* v) : Entity(Entity_tag::FN, n, t, v) {}
	Fn(const Fn& f) : Entity(Entity_tag::FN, f.name, f.type, f.value) {}
};







