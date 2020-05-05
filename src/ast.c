#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "ast.h"

Ast* CreateAstTypeInfer()
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_TYPE;
  node->u.type.tag = T_INFER;
  node->u.type.u.null = '\0';
  return node;
}

Ast* CreateAstTypeNil()
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag           = N_TYPE;
  node->u.type.tag    = T_NIL;
  node->u.type.u.null = '\0';
  return node;
}

Ast* CreateAstTypeFn(Ast* lhs, Ast* rhs)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_TYPE;
  node->u.type.tag = T_FUNC;
  node->u.type.u.rarrow.lhs = lhs;
  node->u.type.u.rarrow.rhs = rhs;
  return node;
}

Ast* CreateAstId(char* name)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag  = N_ID;
  /*
    we want to avoid all of the shallow copy subtleties.
    so we make a deep copy.
    this function is built for use with the lexer/parser, so the passed
    in char* is more likely than not; yytext. which is a very volatile
    ptr.
  */
  node->u.id.s = strdup(name);
  return node;
}

Ast* CreateAstLambda(char* name, Ast* type, Ast* body)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_LAMBDA;
  node->u.lambda.arg.id.s = strdup(name);
  if (type == NULL) node->u.lambda.arg.type = CreateAstTypeInfer();
  else              node->u.lambda.arg.type = type;
  node->u.lambda.body     = body;
  return node;
}

Ast* CreateAstCall(Ast* l, Ast* r)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_CALL;
  node->u.call.lhs = l;
  node->u.call.rhs = r;
  return node;
}

Ast* CreateAstBind(char* name, Ast* term)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_BIND;
  node->u.bind.id.s = strdup(name);
  node->u.bind.term = term;
  return node;
}

void AstDeleteType(Ast* type);
void AstDeleteId(Ast* id);
void AstDeleteLambda(Ast* lambda);
void AstDeleteCall(Ast* call);
void AstDeleteBind(Ast* bind);

void AstDelete(Ast* ast)
{
  if (ast != NULL) {
    switch (ast->tag) {
      case N_TYPE:
        AstDeleteType(ast);
        break;
      case N_ID:
        AstDeleteId(ast);
        break;
      case N_LAMBDA:
        AstDeleteLambda(ast);
        break;
      case N_CALL:
        AstDeleteCall(ast);
        break;
      case N_BIND:
        AstDeleteBind(ast);
        break;
      default:
        fprintf(stderr, "Unexpected Ast Tag!");
        exit(1);
    }
  }
}

void AstDeleteType(Ast* type)
{
  if (type != NULL) {
    if (type->u.type.tag == T_FUNC) {
      AstDeleteType(type->u.type.u.rarrow.lhs);
      AstDeleteType(type->u.type.u.rarrow.rhs);
    }
    free (type);
  }
}

void AstDeleteId(Ast* id)
{
  if (id != NULL) {
    if (id->u.id.s != NULL)
      free(id->u.id.s);
    free(id);
  }
}

void AstDeleteLambda(Ast* lambda)
{
  if (lambda != NULL) {
    if (lambda->u.lambda.arg.id.s)
      free(lambda->u.lambda.arg.id.s);
    AstDeleteType(lambda->u.lambda.arg.type);
    AstDelete(lambda->u.lambda.body);
  }
}

void AstDeleteCall(Ast* call)
{
  if (call != NULL) {
    AstDelete(call->u.call.lhs);
    AstDelete(call->u.call.rhs);
    free(call);
  }
}

void AstDeleteBind(Ast* bind)
{
  if (bind != NULL) {
    if (bind->u.bind.id.s != NULL)
      free(bind->u.bind.id.s);
    AstDelete(bind->u.bind.term);
    free(bind);
  }
}


char* AstTypeToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    Type* type = &(ast->type);
    switch (type->tag) {
      case T_INFER: {
        result = strdup("infer");
        break;
      }
      case T_NIL: {
        result = strdup("nil");
        break;
      }
      case T_FUNC: {
        char* t1 = AstTypeToString(type->u.rarrow.lhs);
        char* t2 = AstTypeToString(type->u.rarrow.rhs);
        char* rarrow = " -> ";
        int len = strlen(t1) + 4 + strlen(t2) + 1;
        result = (char*)calloc(len, sizeof(char));
        strcat(result, t1);
        strcat(result, rarrow);
        strcat(result, t2);
        break;
      }
      default: {
        fprintf("Unknown Type tag; aborting");
        exit(1);
      }
    }
  }
}

char* AstIdToString(Ast* ast)
{
  if (ast != NULL) {

  }
}

char* AstLambdaToString(Ast* ast)
{
  if (ast != NULL) {

  }
}

char* AstCallToString(Ast* ast)
{
  if (ast != NULL) {

  }
}

char* AstBindToString(Ast* ast)
{
  if (ast != NULL) {

  }
}

char* AstToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    switch (ast->tag) {
      case N_TYPE: {
        result = AstTypeToString(ast);
        break;
      }
      case N_ID: {
        result = AstIdToString(ast);
        break;
      }
      case N_LAMBDA: {
        result = AstLambdaToString(ast);
        break;
      }
      case N_CALL: {
        result = AstCallToString(ast);
        break;
      }
      case N_BIND: {
        result = AstBindToString(ast);
        break;
      }
      default:
        fprintf (stderr, "Unknown Ast Tag, aborting");
        exit(1);
    }
  }
}
