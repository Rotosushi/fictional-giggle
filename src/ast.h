
#ifndef AST_H
#define AST_H

#include "type.h"

typedef enum NodeType {
	N_NIL,
	N_NAME,
	N_LAMBDA,
	N_CALL,
	N_BIND
} NodeType;


typedef struct Nil {
	char null;
} Nil;

typedef struct Name {
	char* s;
} Name;

typedef struct Arg {
	Name name;
	Type type;
} Arg;

typedef struct Lambda {
	Arg arg;
	Ast* body;
} Lambda;

typedef struct Call {
	Ast* lhs;
	Ast* rhs;
} Call;

typedef struct Bind {
	Name name;
	Ast* term;
} Bind;

typedef struct Ast {
	NodeType tag;
	Type type;
	union {
		Nil  nil;
		Name name;
		Lambda lambda;
		Call call;
		Bind bind;
	};
} Ast;

Ast* CreateAstNil();
Ast* CreateAstName(char* name);
Ast* CreateAstLambda(char* name, Type type, Ast* body);
Ast* CreateAstCall(Ast* l, Ast* r);
Ast* CreateAstBind(char* name, Ast* term);

#endif /* AST_H */
