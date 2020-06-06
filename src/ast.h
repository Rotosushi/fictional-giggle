
#ifndef AST_H
#define AST_H

/*
	this is the definition of the location tracking
	structure used by the parser and Ast to store the
	textual location of tokens, the compiler uses this
	information for precise error reporting
*/
typedef struct StrLoc
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} StrLoc;


/*
	a type can either be
	nil, or type -> type,
	or (type)
	however we don't need to maintain
	any semantic notion of ()
	because their work can be
	entirely accomplished by their
	directing of the parser.

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
	N_ENTITY,
	N_CALL,
	N_BINOP,
  N_UNOP,
} NodeTag;

typedef enum EntityTag {
  E_ID,
	E_TYPE,
  E_LITERAL,
} EntityTag;

typedef enum TypeTag {
	T_NIL,
  T_PROC,
  T_POLY,
} TypeTag;

typedef enum LiteralTag {
  L_NIL,
  L_PROC,
} LiteralTag;

struct Ast;

typedef struct Type {
	TypeTag tag;
  union {
    char nil;
    struct {
      struct Ast* lhs;
      struct Ast* rhs;
    } proc;
  } u;
} Type;

/* an arg needs to hold a name and it's type */
typedef struct Arg {
	char*       id;
	struct Ast* type;
} Arg;

/* a lambda needs to hold it's argument, and it's body */
typedef struct Procedure {
	Arg  arg;
	struct Ast* body;
} Procedure;

typedef struct Literal {
  LiteralTag tag;
  union {
    char nil;
    Procedure proc;
  } u;
} Literal;

/* the entity struct conveys that this term is a thing,
	 something which has existance and can be acted upon.
	 it may not be representable at runtime, or it may.
*/
typedef struct Entity {
	EntityTag tag;
	union {
    char*     id;
    Type      type;
    Literal   literal;
		/*
		char*  string_literal
		int    int_literal
		double real_literal
		...
		*/
	} u;
} Entity;


/* a call needs to point to it's left and right terms */
typedef struct Call {
	struct Ast* lhs;
	struct Ast* rhs;
} Call;

/* a binop needs to hold it's op, lhs and rhs*/
typedef struct Binop {
  char* op;
	struct Ast* lhs;
  struct Ast* rhs;
} Binop;

typedef struct Unop {
  char* op;
  struct Ast* rhs;
} Unop;

/*
	the ast needs to provide the uniform type so that we can talk about
	ast's as objects and Entitys. we use a tagged union implementation
	to ensure correct usage of the union.
*/
typedef struct Ast {
	NodeTag tag;
	struct StrLoc lloc;
	union {
		Entity entity;
		Call   call;
		Binop  binop;
    Unop   unop;
	} u;
} Ast;

/*
Ast* CreateAstId(char* name, struct StrLoc* llocp);
Ast* CreateAstEntityNil(struct StrLoc* llocp);
Ast* CreateAstEntityTypeNil(struct StrLoc* llocp);
Ast* CreateAstEntityTypePoly();
Ast* CreateAstEntityTypeFn(Ast* l, Ast* r, struct StrLoc* llocp);
Ast* CreateAstEntityFn(char* name, Ast* type, Ast* body, struct StrLoc* llocp);
Ast* CreateAstCall(Ast* l, Ast* r, struct StrLoc* llocp);
Ast* CreateAstBind(char* name, Ast* term, struct StrLoc* llocp);
*/
Ast* CreateAstEntityId(char* id, struct StrLoc* llocp);
Ast* CreateAstEntityTypeNil(struct StrLoc* llocp);
Ast* CreateAstEntityTypePoly();
Ast* CreateAstEntityTypeProc(struct Ast* lhs, struct Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstEntityLiteralNil(struct StrLoc* llocp);
Ast* CreateAstEntityLiteralProc(char* arg_id, Ast* arg_type, Ast* body, struct StrLoc* llocp);
Ast* CreateAstCall(Ast* lhs, Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstBinop(char* op, Ast* lhs, Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstUnop(char* op, Ast* rhs, struct StrLoc* llocp);

void DeleteAst(Ast* ast);

Ast* CopyAst(Ast* ast);

char* AstToString(Ast* ast);

#endif /* AST_H */
