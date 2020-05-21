#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

#include "error.h"
#include "ast.h"

Ast* CreateAstId(char* name)
{
  Ast* node    = (Ast*)malloc(sizeof(Ast));
  node->tag    = N_ID;
  node->u.id.s = name;
  return node;
}

Ast* CreateAstEntityTypeNil()
{
  Ast* node                    = (Ast*)malloc(sizeof(Ast));
  node->tag                    = N_ENTITY;
  node->u.entity.tag           = E_TYPE;
  node->u.entity.u.type.tag    = T_NIL;
  node->u.entity.u.type.u.null = '\0';
  return node;
}

Ast* CreateAstEntityTypeFn(Ast* lhs, Ast* rhs)
{
  Ast* node                          = (Ast*)malloc(sizeof(Ast));
  node->tag                          = N_ENTITY;
  node->u.entity.tag                 = E_TYPE;
  node->u.entity.u.type.tag          = T_LAMBDA;
  node->u.entity.u.type.u.rarrow.lhs = lhs;
  node->u.entity.u.type.u.rarrow.rhs = rhs;
  return node;
}



Ast* CreateAstEntityFn(char* name, Ast* type, Ast* body)
{
  Ast* node                        = (Ast*)malloc(sizeof(Ast));
  node->tag                        = N_ENTITY;
  node->u.entity.tag               = E_LAMBDA;
  node->u.entity.u.lambda.arg.id.s = name;
  node->u.entity.u.lambda.arg.type = type;
  node->u.entity.u.lambda.body     = body;
  return node;
}

Ast* CreateAstCall(Ast* l, Ast* r)
{
  Ast* node        = (Ast*)malloc(sizeof(Ast));
  node->tag        = N_CALL;
  node->u.call.lhs = l;
  node->u.call.rhs = r;
  return node;
}

Ast* CreateAstBind(char* name, Ast* term)
{
  Ast* node         = (Ast*)malloc(sizeof(Ast));
  node->tag         = N_BIND;
  node->u.bind.id.s = name;
  node->u.bind.term = term;
  return node;
}

void DeleteAstId(Ast* id);
void DeleteAstEntity(Ast* entity);
void DeleteAstCall(Ast* call);
void DeleteAstBind(Ast* bind);

void DeleteAst(Ast* ast)
{
  if (ast != NULL) {
    switch (ast->tag) {
      case N_ID:
        DeleteAstId(ast);
        break;
      case N_ENTITY:
        DeleteAstEntity(ast);
        break;
      case N_CALL:
        DeleteAstCall(ast);
        break;
      case N_BIND:
        DeleteAstBind(ast);
        break;
      default:
        error_abort("malformed ast tag! aborting");
    }
  }
}

void DeleteAstEntity(Ast* entity)
{
  if (entity != NULL) {
    switch (entity->u.entity.tag) {
      case E_TYPE:   {
        if (entity->u.entity.u.type.tag == T_LAMBDA) {
          DeleteAstEntity(entity->u.entity.u.type.u.rarrow.lhs);
          DeleteAstEntity(entity->u.entity.u.type.u.rarrow.rhs);
        }
        free(entity);
        break;
      }
      case E_LAMBDA: {
        if (entity->u.entity.u.lambda.arg.id.s != NULL)
          free (entity->u.entity.u.lambda.arg.id.s);
        DeleteAstEntity(entity->u.entity.u.lambda.arg.type);
        DeleteAst(entity->u.entity.u.lambda.body);
        break;
      }
      default: error_abort("malformed entity tag! aborted");
    }
  }
}

void DeleteAstId(Ast* id)
{
  if (id != NULL) {
    if (id->u.id.s != NULL)
      free(id->u.id.s);
    free(id);
  }
}

void DeleteAstCall(Ast* call)
{
  if (call != NULL) {
    DeleteAst(call->u.call.lhs);
    DeleteAst(call->u.call.rhs);
    free(call);
  }
}

void DeleteAstBind(Ast* bind)
{
  if (bind != NULL) {
    if (bind->u.bind.id.s != NULL)
      free(bind->u.bind.id.s);
    DeleteAst(bind->u.bind.term);
    free(bind);
  }
}


Ast* CopyAstId(Ast* id);
Ast* CopyAstEntity(Ast* entity);
Ast* CopyAstCall(Ast* call);
Ast* CopyAstBind(Ast* bind);

Ast* CopyAst(Ast* ast)
{
  switch(ast->tag) {
    case N_ID:     return CopyAstId(ast);
    case N_ENTITY: return CopyAstEntity(ast);
    case N_CALL:   return CopyAstCall(ast);
    case N_BIND:   return CopyAstBind(ast);
    default: error_abort ("malformed ast! aborting");
  }
}

Ast* CopyAstEntity(Ast* entity)
{
  switch (entity->u.entity.tag) {
    case E_TYPE: {
      switch(entity->u.entity.u.type.tag) {
        case T_NIL:    return CreateAstEntityTypeNil();

        case T_LAMBDA: return CreateAstEntityTypeFn(CopyAstEntity(entity->u.entity.u.type.u.rarrow.lhs), \
                                                   CopyAstEntity(entity->u.entity.u.type.u.rarrow.rhs));
        default: error_abort("malformed type! aborting");
      }
    }
    case E_LAMBDA: {
      return CreateAstEntityFn(strdup(entity->u.entity.u.lambda.arg.id.s), \
                              CopyAstEntity(entity->u.entity.u.lambda.arg.type), \
                              CopyAst(entity->u.entity.u.lambda.body));
    }
    default: error_abort("malformed entity! aborting");
  }
}

Ast* CopyAstId(Ast* id)
{
  return CreateAstId(strdup(id->u.id.s));
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

char* AstIdToString(Ast*);
char* AstEntityTypeToString(Ast*);
char* AstEntityLambdaToString(Ast*);
char* AstEntityToString(Ast*);
char* AstCallToString(Ast*);
char* AstBindToString(Ast*);


char* AstEntityTypeToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    Type* type = &(ast->u.entity.u.type);

    switch (type->tag) {
      case T_NIL: {
        result = strdup("nil");
        break;
      }

      case T_LAMBDA: {
        char* t1 = AstEntityToString(type->u.rarrow.lhs);
        if (!t1) {
          error_abort("malformed type! aborting");
        }
        char* t2 = AstEntityToString(type->u.rarrow.rhs);
        if (!t2) {
          error_abort("malformed type! aborting");
        }
        char* lprn = "(", *rprn = ")";
        char* rarrow = " -> ";
        int len;
        // this could seg-fault, but only if the tree is malformed.
        bool grouped = type->u.rarrow.lhs->u.entity.u.type.tag == T_LAMBDA;
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

char* AstEntityLambdaToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char *bs = " \\ ", *cln = " : ", *rarw = " => ";
    char* arg_id = ast->u.entity.u.lambda.arg.id.s;
    if   (arg_id == NULL) {
      error_abort("malformed arg id! aborting");
    }

    char* arg_type = AstEntityToString(ast->u.entity.u.lambda.arg.type);
    if   (arg_type == NULL) {
      error_abort("malformed arg type! aborting");
    }

    char* body = AstToString(ast->u.entity.u.lambda.body);
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

char* AstEntityToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    switch (ast->u.entity.tag) {
      case E_TYPE: {
        result = AstEntityTypeToString(ast);
      }
      case E_LAMBDA: {
        result = AstEntityLambdaToString(ast);
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

    int len = strlen(lhs) + strlen(spc) + 1 + strlen(rhs) + 2;
    result  = (char*)calloc(len, sizeof(char));
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

    char* id = ast->u.bind.id.s;
    if (id == NULL) {
      error_abort("malformed bind id! aborting");
    }

    char* term = AstToString(ast->u.bind.term);
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
      case N_ENTITY: {
        result = AstEntityToString(ast);
        break;
      }
      case N_ID: {
        result = AstIdToString(ast);
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
