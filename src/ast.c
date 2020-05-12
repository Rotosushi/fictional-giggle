#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

#include "error.h"
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
  node->u.id.s = name;
  return node;
}

Ast* CreateAstLambda(char* name, Ast* type, Ast* body)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_LAMBDA;
  node->u.lambda.arg.id.s = name;
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
  node->u.bind.id.s = name;
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
        error_abort("malformed ast tag! aborting");
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


Ast* CopyAstType(Ast* type);
Ast* CopyAstId(Ast* id);
Ast* CopyAstLambda(Ast* lambda);
Ast* CopyAstCall(Ast* call);
Ast* CopyAstBind(Ast* bind);

Ast* CopyAst(Ast* ast)
{
  switch(ast->tag) {
    case N_TYPE:   return CopyAstType(ast);
    case N_ID:     return CopyAstId(ast);
    case N_LAMBDA: return CopyAstLambda(ast);
    case N_CALL:   return CopyAstCall(ast);
    case N_BIND:   return CopyAstBind(ast);
    default: error_abort ("malformed ast! aborting");
  }
}

Ast* CopyAstType(Ast* type)
{
  switch (type->u.type.tag) {
    case T_NIL:   return CreateAstTypeNil();
    case T_INFER: return CreateAstTypeInfer();
    case T_FUNC:  return CreateAstTypeFn(CopyAstType(type->u.type.u.rarrow.lhs), \
                                         CopyAstType(type->u.type.u.rarrow.rhs));
    default: error_abort("malformed type! aborting");
  }
}

Ast* CopyAstId(Ast* id)
{
  return CreateAstId(strdup(id->u.id.s));
}

Ast* CopyAstLambda(Ast* lambda)
{
  return CreateAstLambda(strdup(lambda->u.lambda.arg.id.s), \
                         CopyAstType(lambda->u.lambda.arg.type), \
                         CopyAst(lambda->u.lambda.body));
}

Ast* CopyAstCall(Ast* call)
{
  return CreateAstCall(CopyAst(call->u.call.lhs), \
                       CopyAst(call->u.call.rhs));
}

Ast* CopyAstBind(Ast* bind)
{
  return CreateAstBind(strdup(bind->u.bind.id.s), \
                       CopyAst(bind->u.bind.term));
}


char* AstTypeToString(Ast*);
char* AstIdToString(Ast*);
char* AstLambdaToString(Ast*);
char* AstCallToString(Ast*);
char* AstBindToString(Ast*);

char* AstTypeToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    Type* type = &(ast->u.type);
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
        if (!t1) {
          error_abort("malformed type! aborting");
        }
        char* t2 = AstTypeToString(type->u.rarrow.rhs);
        if (!t2) {
          error_abort("malformed type! aborting");

        }
        char* lprn = "(", *rprn = ")";
        char* rarrow = " -> ";
        int len;
        bool grouped = type->u.rarrow.lhs->u.type.tag == T_FUNC;
        if (grouped) {
          len = 1 + strlen(t1) + 1 + 4 + strlen(t2) + 1;
          result = (char*)calloc(len, sizeof(char));
          strcat(result, lprn);
          strcat(result, t1);
          strcat(result, rprn);
          strcat(result, rarrow);
          strcat(result, t2);
          free (t1);
          free (t2);
        }
        else {
          len = strlen(t1) + 4 + strlen(t2) + 1;
          result = (char*)calloc(len, sizeof(char));
          strcat(result, t1);
          strcat(result, rarrow);
          strcat(result, t2);
          free (t1);
          free (t2);
        }


        break;
      }
      default: {
        error_abort("unknown type tag! aborting");
      }
    }
  }
  return result;
}

char* AstIdToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char* id = ast->u.id.s;
    if (id == NULL) {
      error_abort("malformed id! aborting");
    }
    int len  = strlen (ast->u.id.s) + 1;
    result = (char*)calloc(len, sizeof(char));
    //strcat(result, " ");
    strcat(result,  id);
    //strcat(result, " ");
  }
  return result;
}

char* AstLambdaToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char *bs = " \\ ", *cln = " : ", *rarw = " => ";

    char* arg_id = ast->u.lambda.arg.id.s;
    if   (arg_id == NULL) {
      error_abort("malformed arg id! aborting");
    }

    char* arg_type = AstTypeToString(ast->u.lambda.arg.type);
    if   (arg_type == NULL) {
      error_abort("malformed arg type! aborting");
    }

    char* body = AstToString(ast->u.lambda.body);
    if   (body == NULL) {
      error_abort("malformed body! aborting");
    }

    int len = strlen(bs)       \
            + strlen(arg_id)   \
            + strlen(cln)      \
            + strlen(arg_type) \
            + strlen(rarw)     \
            + strlen(body) + 1;
    result  = (char*)calloc(len, sizeof(char));
    strcat(result, bs);
    strcat(result, arg_id);
    strcat(result, cln);
    strcat(result, arg_type);
    strcat(result, rarw);
    strcat(result, body);
    free(arg_type);
    free(body);
  }
  return result;
}

char* AstCallToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char* spc = " ", *lprn = "(", *rprn = ")";
    char* lhs = AstToString(ast->u.call.lhs);
    if   (lhs == NULL) {
       error_abort("malformed call lhs! aborting");
    }
    char* rhs = AstToString(ast->u.call.rhs);
    if   (rhs == NULL) {
       error_abort("malformed call rhs! aborting");
    }

    int len   = strlen(lhs) + strlen(spc) + 1 + strlen(rhs) + 2;
    result    = (char*)calloc(len, sizeof(char));
    strcat(result, lhs);
    strcat(result, spc);
    strcat(result, lprn);
    strcat(result, rhs);
    strcat(result, rprn);
    free(lhs);
    free(rhs);
  }
  return result;
}

char* AstBindToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char* clneq = " := ";

    char* id    = ast->u.bind.id.s;
    if (id == NULL) {
      error_abort("malformed bind id! aborting");
    }

    char* term  = AstToString(ast->u.bind.term);
    if (term == NULL) {
      error_abort("malformed bind term! aborting");
    }

    int len = strlen(id) + strlen(clneq) + strlen(term) + 1;
    result  = (char*)calloc(len, sizeof(char));
    strcat(result, id);
    strcat(result, clneq);
    strcat(result, term);
    free(term);
  }
  return result;
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
        error_abort("malformed type tag! aborting");
    }
  }
  return result;
}
