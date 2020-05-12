
#ifndef AST_H
#define AST_H

/*
	a type can either be
	nil, or type -> type,
	or (type)
	however we don't need to maintain
	any semantic notion of ()
	because they control the
	parse tree by their
	very use in the grammar.

	when we have user defined types,
	maybe instead of specifying each type
	by it's name as a enum flag, we instead
	have the enum represent the typekind *
	which instead holds the	string which names the type?
	then we can just track the names
	of types in the syntax tree
	and then when we need to look up types,
	we can do it via the environment.
*/

typedef enum NodeTag {
	N_TYPE,
	N_ID,
	N_LAMBDA,
	N_CALL,
	N_BIND,
} NodeTag;

typedef enum TypeTag {
	T_INFER,
	T_NIL,
	T_FUNC,
} TypeTag;

struct Ast;

/*
	c's type system's limitations
	force us to be less typesafe
	than I would like, in a language
	with subtype polymorphism,
	we can describe a Type clearly
	as a subtype of Ast. then
	the function type members could be
	two pointers to Type instead of
	pointers to Ast, which would allow
	the same composition of type nodes
	to describe recursive types as is
	provided by Ast pointers, but it
	would disallow assigning a lambda
	Ast node or a Call Ast Node to a type variable,
	something which is probably a semantic error
	on the part of the programmer.

	(
	though in a pure type system;
		where types are first class values,
	  I could imagine some meaning for a
	  type-described-by-a-lambda,
		it could be a parametric type constructor.
	)
*/
typedef struct Type {
	TypeTag tag;
	union {
		char null;
		struct {
			struct Ast* lhs;
			struct Ast* rhs;
		} rarrow;
	} u;
} Type;



/* a name simply owns the string which is the name */
typedef struct Id {
	char* s;
} Id;

/* an arg needs to hold a name and it's type */
typedef struct Arg {
	Id   id;
	struct Ast* type;
} Arg;

/* a lambda needs to hold it's argument, and it's body */
typedef struct Lambda {
	Arg  arg;
	struct Ast* body;
} Lambda;

/* a call needs to point to it's left and right terms */
typedef struct Call {
	struct Ast* lhs;
	struct Ast* rhs;
} Call;

/* a bind needs to hold the to-be-bound name and the term that is being bound */
typedef struct Bind {
	Id   id;
	struct Ast* term;
} Bind;

/*
	the ast needs to provide the uniform type so that we can talk about
	ast's as objects and values. we use a tagged union implementation
	to ensure correct usage of the union.
*/
typedef struct Ast {
	NodeTag tag;
	union {
		Type   type;
		Value  value;
		Id     id;
		Lambda lambda;
		Call   call;
		Bind   bind;
	} u;
} Ast;

Ast* CreateAstTypeInfer();
Ast* CreateAstTypeNil();
Ast* CreateAstTypeFn(Ast* lhs, Ast* rhs);
Ast* CreateAstId(char* name);
Ast* CreateAstLambda(char* name, Ast* type, Ast* body);
Ast* CreateAstCall(Ast* l, Ast* r);
Ast* CreateAstBind(char* name, Ast* term);

void AstDelete(Ast* ast);

Ast* CopyAst(Ast* ast);

char* AstToString(Ast*);

#endif /* AST_H */
