#include <string.h>

#include "type.h"
#include "ast.h"


Ast* CreateNodeNil()
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag  = N_NIL;
  node->type.kind = K_DATA;
  node->type.bt.tag  = T_NIL;
  node->b.nil.null = '\0';
  return node;
}

Ast* CreateNodeName(char* name)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag  = N_NAME;
  /*
    this is actually an important default descision.
    what is the type of any given name?
    here we choose the most obvious solution, the name
    has no type by itself. this is because the type
    of a name is defined in the environment. so we
    delay the descision to some later point of the program.
    why then include a type in the Name nodes? because the
    type has been defined as a property of the Node itself,
    instead of a property of the language entities themselves.
    so it makes sense to give this known name node
    state/kind/type a name, UNDEF.
  */
  node->type.kind = K_UNDEF;
  node->type.bt.tag = T_UNDEF;
  /*
    we want to avoid all of the shallow copy subtleties.
    so we make a deep copy.
    this function is built for use with the lexer/parser, so the passed
    in char* is more likely than not; yytext. which is a very volatile
    ptr.
  */
  node->b.name.s = strdup(name);
  return node;
}

Ast* CreateNodeLambda(Arg arg, Ast* body)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_LAMBDA;
  InitFuncType(&node->type, arg.type, body->type);
  node->b.lambda.arg.name.s = strdup(arg.name.s);
  node->b.lambda.arg.type   = arg.type;
  node->b.lambda.body       = body;
  return node;
}

Ast* CreateNodeCall(Ast* l, Ast* r)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_CALL;
  /*
    we don't want to typecheck at creation time,
    so unless it is starkly obvious what the type
    is, we mark it unknown, given knowing the type
    of a call involves inspecting both ptrs, it
    is not starkly obvious what the type of this
    call is.
  */
  node->type.kind = K_UNDEF;
  node->type.b.bt = T_UNDEF;
  node->lhs = l;
  node->rhs = r;
  return node;
}

Ast*	CreateNodeBind(char* name, Ast* term)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_BIND;
  node->type.kind = K_UNDEF;
  node->type.b.bt = T_UNDEF;
  node->b.bind.name.s = strdup(name);
  node->b.bind.term = term;
  return node;
}
