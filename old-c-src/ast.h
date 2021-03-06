
#ifndef AST_H
#define AST_H

#include <stdbool.h>

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
  N_ID,
	N_ENTITY,
	N_CALL,
  N_BIND,
	N_BINOP,
  N_UNOP,
  N_IF,
} NodeTag;

typedef enum EntityTag {
	E_TYPE,
  E_LITERAL,
} EntityTag;

typedef enum TypeTag {
  T_PROC,
  T_POLY,
  T_NIL,
  T_INT,
  T_BOOL,
} TypeTag;

typedef enum LiteralTag {
  L_NIL,
  L_PROC,
  L_INT,
  L_BOOL,
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

/* a procedure literal needs to hold it's argument, and it's body */
typedef struct Lambda {
	Arg  arg;
	struct Ast* body;
} Lambda;

/* a linked list node for the set of literals */
typedef struct ProcInst {
  Lambda def;
  struct ProcInst* next;
} ProcInst;

typedef struct ProcSet {
  Lambda def;
  ProcInst* set;
  bool polymorphic;
} ProcSet;


typedef struct Literal {
  LiteralTag tag;
  union {
    char nil;
    int  integer;
    bool boolean;
    ProcSet proc;
  } u;
} Literal;

/* the entity struct conveys that this term is a thing,
	 something which has existance and can be acted upon.
	 it may not be representable at runtime, or it may.
*/
typedef struct Entity {
	EntityTag tag;
	union {
    Type      type;
    Literal   literal;
	} u;
} Entity;


/* a call needs to point to it's left and right terms */
typedef struct Call {
	struct Ast* lhs;
	struct Ast* rhs;
} Call;

typedef struct Bind {
  char*       id;
  struct Ast* term;
} Bind;

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

typedef struct Cond {
  struct Ast* test;
  struct Ast* first;
  struct Ast* second;
} Cond;

/*
	the ast needs to provide the uniform type so that we can talk about
	ast's as objects and Entitys. we use a tagged union implementation
	to ensure correct usage of the union.
*/
typedef struct Ast {
	NodeTag tag;
	struct StrLoc lloc;
  bool hasloc;
	union {
    char*  id;
		Entity entity;
		Call   call;
    Bind   bind;
		Binop  binop;
    Unop   unop;
    Cond   cond;
	} u;
} Ast;


Ast* CreateAstEntityTypeBool(struct StrLoc* llocp);
Ast* CreateAstEntityTypeInt(struct StrLoc* llocp);
Ast* CreateAstEntityTypeNil(struct StrLoc* llocp);
Ast* CreateAstEntityTypePoly();
Ast* CreateAstEntityTypeProc(struct Ast* lhs, struct Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstEntityLiteralBool(bool value, struct StrLoc* llocp);
Ast* CreateAstEntityLiteralInt(int value, struct StrLoc* llocp);
Ast* CreateAstEntityLiteralNil(struct StrLoc* llocp);
Ast* CreateAstEntityLiteralProc(char* arg_id, Ast* arg_type, Ast* body, struct StrLoc* llocp);
Ast* CreateAstId(char* id, struct StrLoc* llocp);
Ast* CreateAstBind(char* id, Ast* term, struct StrLoc* llocp);
Ast* CreateAstCall(Ast* lhs, Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstBinop(char* op, Ast* lhs, Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstUnop(char* op, Ast* rhs, struct StrLoc* llocp);
Ast* CreateAstCond(Ast* cond, Ast* first, Ast* second, struct StrLoc* llocp);

void DeleteAst(Ast* ast);

Ast* CopyAst(Ast* ast);

char* AstToString(Ast* ast);

#endif /* AST_H */
