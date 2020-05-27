#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

#include "parser.h"
#include "ast.h"
#include "error.h"


/*
struct YYLTYPE {
int first_line;
int first_column;
int last_line;
int last_column;
};
*/

Ast* CreateAstId(char* name, YYLTYPE* llocp)
{
  Ast* node    = (Ast*)malloc(sizeof(Ast));
  node->tag    = N_ID;
  node->u.id.s = name;
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
  return node;
}

Ast* CreateAstEntityTypeNil(YYLTYPE* llocp)
{
  Ast* node                    = (Ast*)malloc(sizeof(Ast));
  node->tag                    = N_ENTITY;
  node->u.entity.tag           = E_TYPE;
  node->u.entity.u.type.tag    = T_NIL;
  node->u.entity.u.type.u.null = '\0';
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
  return node;
}

Ast* CreateAstEntityTypeFn(Ast* lhs, Ast* rhs, YYLTYPE* llocp)
{
  Ast* node                          = (Ast*)malloc(sizeof(Ast));
  node->tag                          = N_ENTITY;
  node->u.entity.tag                 = E_TYPE;
  node->u.entity.u.type.tag          = T_LAMBDA;
  node->u.entity.u.type.u.rarrow.lhs = lhs;
  node->u.entity.u.type.u.rarrow.rhs = rhs;
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
  return node;
}



Ast* CreateAstEntityFn(char* name, Ast* type, Ast* body, YYLTYPE* llocp)
{
  Ast* node                        = (Ast*)malloc(sizeof(Ast));
  node->tag                        = N_ENTITY;
  node->u.entity.tag               = E_LAMBDA;
  node->u.entity.u.lambda.arg.id.s = name;
  node->u.entity.u.lambda.arg.type = type;
  node->u.entity.u.lambda.body     = body;
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
  return node;
}

Ast* CreateAstCall(Ast* l, Ast* r, YYLTYPE* llocp)
{
  Ast* node        = (Ast*)malloc(sizeof(Ast));
  node->tag        = N_CALL;
  node->u.call.lhs = l;
  node->u.call.rhs = r;
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
  return node;
}

Ast* CreateAstBind(char* name, Ast* term, YYLTYPE* llocp)
{
  Ast* node         = (Ast*)malloc(sizeof(Ast));
  node->tag         = N_BIND;
  node->u.bind.id.s = name;
  node->u.bind.term = term;
  if (llocp != NULL) {
    node->lloc.first_line   = llocp->first_line;
    node->lloc.first_column = llocp->first_column;
    node->lloc.last_line    = llocp->last_line;
    node->lloc.last_column  = llocp->last_column;
  } else {
    node->lloc.first_line   = 0;
    node->lloc.first_column = 0;
    node->lloc.last_line    = 0;
    node->lloc.last_column  = 0;
  }
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
        error_abort("malformed ast tag! aborting", __FILE__, __LINE__);
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
      default: error_abort("malformed entity tag! aborted", __FILE__, __LINE__);
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
    default: error_abort ("malformed ast! aborting", __FILE__, __LINE__);
  }
}

Ast* CopyAstEntity(Ast* entity)
{
  switch (entity->u.entity.tag) {
    case E_TYPE: {
      switch(entity->u.entity.u.type.tag) {
        case T_NIL:    return CreateAstEntityTypeNil(NULL);

        case T_LAMBDA: return CreateAstEntityTypeFn(CopyAstEntity(entity->u.entity.u.type.u.rarrow.lhs), \
                                                   CopyAstEntity(entity->u.entity.u.type.u.rarrow.rhs),  \
                                                   NULL);
        default: error_abort("malformed type! aborting", __FILE__, __LINE__);
      }
    }
    case E_LAMBDA: {
      return CreateAstEntityFn(strdup(entity->u.entity.u.lambda.arg.id.s),       \
                              CopyAstEntity(entity->u.entity.u.lambda.arg.type), \
                              CopyAst(entity->u.entity.u.lambda.body),           \
                              NULL);
    }
    default: error_abort("malformed entity! aborting", __FILE__, __LINE__);
  }
}

Ast* CopyAstId(Ast* id)
{
  return CreateAstId(strdup(id->u.id.s), NULL);
}

Ast* CopyAstCall(Ast* call)
{
  return CreateAstCall(CopyAst(call->u.call.lhs), \
                       CopyAst(call->u.call.rhs), \
                       NULL);
}

Ast* CopyAstBind(Ast* bind)
{
  return CreateAstBind(strdup(bind->u.bind.id.s),  \
                       CopyAst(bind->u.bind.term), \
                       NULL);
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
          error_abort("malformed type! aborting", __FILE__, __LINE__);
        }
        char* t2 = AstEntityToString(type->u.rarrow.rhs);
        if (!t2) {
          error_abort("malformed type! aborting", __FILE__, __LINE__);
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
        error_abort("unknown type tag! aborting", __FILE__, __LINE__);
      }
    }
  }
  return result;
}

char* AstEntityLambdaToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char *bs = " \\ ", *cln = " : ", *reqarw = " => ";
    char* arg_id = ast->u.entity.u.lambda.arg.id.s;
    if   (arg_id == NULL) {
      error_abort("malformed arg id! aborting", __FILE__, __LINE__);
    }

    char* arg_type = AstEntityToString(ast->u.entity.u.lambda.arg.type);
    if   (arg_type == NULL) {
      error_abort("malformed arg type! aborting", __FILE__, __LINE__);
    }

    char* body = AstToString(ast->u.entity.u.lambda.body);
    if   (body == NULL) {
      error_abort("malformed body! aborting", __FILE__, __LINE__);
    }

    int len = strlen(bs)       \
            + strlen(arg_id)   \
            + strlen(cln)      \
            + strlen(arg_type) \
            + strlen(reqarw)     \
            + strlen(body) + 1;
    result  = (char*)calloc(len, sizeof(char));
    strcat(result, bs);
    strcat(result, arg_id);
    strcat(result, cln);
    strcat(result, arg_type);
    strcat(result, reqarw);
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
        break;
      }
      case E_LAMBDA: {
        result = AstEntityLambdaToString(ast);
        break;
      }
      default:
        error_abort("malformed entity tag! aborting", __FILE__, __LINE__);
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
      error_abort("malformed id! aborting", __FILE__, __LINE__);
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
       error_abort("malformed call lhs! aborting", __FILE__, __LINE__);
    }
    char* rhs = AstToString(ast->u.call.rhs);
    if   (rhs == NULL) {
       error_abort("malformed call rhs! aborting", __FILE__, __LINE__);
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
      error_abort("malformed bind id! aborting", __FILE__, __LINE__);
    }

    char* term = AstToString(ast->u.bind.term);
    if (term == NULL) {
      error_abort("malformed bind term! aborting", __FILE__, __LINE__);
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
        error_abort("malformed type tag! aborting", __FILE__, __LINE__);
    }
  }
  return result;
}
