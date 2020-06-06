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

Ast* CreateAstEntityId(char* name, StrLoc* llocp)
{
  Ast* node          = (Ast*)malloc(sizeof(Ast));
  node->tag          = N_ENTITY;
  node->u.entity.tag = E_ID;
  node->u.entity.id  = name;
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

Ast* CreateAstEntityTypeNil(StrLoc* llocp)
{
  Ast* node                    = (Ast*)malloc(sizeof(Ast));
  node->tag                    = N_ENTITY;
  node->u.entity.tag           = E_TYPE;
  node->u.entity.u.type.tag    = T_NIL;
  node->u.entity.u.type.u.nil  = '\0';
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

Ast* CreateAstEntityTypePoly()
{
  Ast* node                    = (Ast*)malloc(sizeof(Ast));
  node->tag                    = N_ENTITY;
  node->u.entity.tag           = E_TYPE;
  node->u.entity.u.type.tag    = T_POLY;
  node->u.entity.u.type.u.nil  = '\0';
  node->lloc.first_line        = 0;
  node->lloc.first_column      = 0;
  node->lloc.last_line         = 0;
  node->lloc.last_column       = 0;
  return node;
}

Ast* CreateAstEntityTypeProc(struct Ast* lhs, struct Ast* rhs, struct StrLoc* llocp)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_ENTITY;
  node->u.entity.tag = E_TYPE;
  node->u.entity.u.type.tag = T_PROC;
  node->u.entity.u.type.u.proc.lhs = lhs;
  node->u.entity.u.type.u.proc.rhs = rhs;
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

Ast* CreateAstEntityLiteralNil(StrLoc* llocp)
{
  Ast* node                          = (Ast*)malloc(sizeof(Ast));
  node->tag                          = N_ENTITY;
  node->u.entity.tag                 = E_LITERAL;
  node->u.entity.u.literal.tag       = L_NIL;
  node->u.entity.u.literal.u.epsilon = '\0';
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

Ast* CreateAstEntityLiteralProc(char* name, Ast* type, Ast* body, StrLoc* llocp)
{
  Ast* node                                = (Ast*)malloc(sizeof(Ast));
  node->tag                                = N_ENTITY;
  node->u.entity.tag                       = E_LITERAL;
  node->u.entity.u.literal.tag             = L_PROC:
  node->u.entity.u.literal.u.proc.arg.id   = name;
  node->u.entity.u.literal.u.proc.arg.type = type;
  node->u.entity.u.literal.u.proc.body     = body;
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

Ast* CreateAstCall(Ast* l, Ast* r, StrLoc* llocp)
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

Ast* CreateAstBinop(char* op, Ast* lhs, Ast* rhs, StrLoc* llocp)
{
  Ast* node         = (Ast*)malloc(sizeof(Ast));
  node->tag         = N_BINOP;
  node->u.binop.op  = op;
  node->u.binop.lhs = lhs;
  node->u.binop.rhs = rhs;
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

Ast* CreateAstUnop(char* op, Ast* rhs, StrLoc* llocp)
{
  Ast* node = (Ast*)malloc(sizeof(Ast));
  node->tag = N_UNOP;
  node->u.unop.op = op;
  node->u.unop.rhs = rhs;
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

void DeleteAstEntity(Ast* entity);
void DeleteAstCall(Ast* call);
void DeleteAstBinop(Ast* binop);
void DeleteAstUnop(Ast* unop);

void DeleteAst(Ast* ast)
{
  if (ast != NULL) {
    switch (ast->tag) {
      case N_ENTITY:
        DeleteAstEntity(ast);
        break;
      case N_CALL:
        DeleteAstCall(ast);
        break;
      case N_BINOP:
        DeleteAstBinop(ast);
        break;
      case N_UNOP:
        DeleteAstUnop(ast);
        break;
      default:
        error_abort("malformed ast tag! aborting", __FILE__, __LINE__);
    }
  }
}

void DeleteAstEntityType(Ast* type)
{
  if (type != NULL) {
    Type* t = &(type->u.type);
    switch(t->tag) {
      case T_PROC:
        DeleteAst(t->u.proc.lhs);
        DeleteAst(t->u.proc.rhs);
        /* we explicitly want fall-through behavior here,
            because the deletion code is the same for all
            types, if we factor deleting the tree structure
            into the above case
        */
      case T_NIL: case T_POLY:
        free(type);
        break;
      default:
        error_abort("malformed type tag! aborting", __FILE__, __LINE__);
    }
  }
}

void DeleteAstEntityLiteral(Ast* literal)
{
  if (literal != NULL) {
    Literal* l = &(literal->u.literal);
    switch(l->tag) {
      case L_PROC:
        if (l->u.proc.arg.id)
          free(l->u.proc.arg.id);
        DeleteAst(l->u.proc.arg.type);
        DeleteAst(l->u.proc.body);
      case L_NIL:
        free(literal);
        break;
      default:
        error_abort("malformed literal tag! aborting", __FILE__, __LINE__);
    }
  }
}

void DeleteAstEntity(Ast* entity)
{
  if (entity != NULL) {
    Entity* e = &(entity->u.entity);
    switch(e->tag) {
      case E_ID:
        if (e->u.id)
          free(e->u.id);
        free(entity);
        break;
      case E_TYPE:
        DeleteAstEntityType(entity);
        break;
      case E_LITERAL:
        DeleteAstEntityLiteral(entity);
        break;
      default:
        error_abort("malformed entity tag! aborting", __FILE__, __LINE__);
    }
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

void DeleteAstBinop(Ast* binop)
{
  if (binop != NULL) {
    if (binop->u.binop.op)
      free (binop->u.binop.op);
    DeleteAst(binop->u.binop.lhs);
    DeleteAst(binop->u.binop.rhs);
    free(binop);
  }
}

void DeleteAstUnop(Ast* unop)
{
  if (unop != NULL) {
    if (unop->u.unop.op)
      free (unop->u.unop.op);
    DeleteAst(unop->u.unop.rhs);
    free(unop);
  }
}


Ast* CopyAstEntity(Ast* entity);
Ast* CopyAstCall(Ast* call);
Ast* CopyAstBinop(Ast* binop);
Ast* CopyAstUnop(Ast* unop);

Ast* CopyAst(Ast* ast)
{
  if (ast == NULL)
    return NULL;

  switch(ast->tag) {
    case N_ENTITY: return CopyAstEntity(ast);
    case N_CALL:   return CopyAstCall(ast);
    case N_BINOP:  return CopyAstBind(ast);
    case N_UNOP:   return CopyAstUnop(ast);
    default: error_abort ("malformed ast! aborting", __FILE__, __LINE__);
  }
}

Ast* CopyAstEntityType(Ast* type)
{
    if (type != NULL) {
      Type* t = &(type->u.entity.u.type);
      Ast* res = NULL;
      switch(t->tag) {
        case T_NIL:
          return CreateAstEntityTypeNil(NULL);
          break;
        case T_POLY:
          return CreateAstEntityTypePoly();
          break;
        case T_PROC:
          return CreateAstEntityTypeProc(CopyAst(t->u.proc.lhs), \
                                         CopyAst(t->u.proc.rhs));
          break;
        default:
          error_abort("malformed type tag! aborting", __FILE__, __LINE__);
      }
    }
}

Ast* CopyAstEntityLiteral(Ast* literal)
{
  if (literal != NULL) {
    Literal* l = &(literal->u.entity.u.literal);
    switch(l->tag) {
      case L_NIL:
        return CreateAstEntityLiteralNil(NULL);
        break;
      case L_PROC:
        return CreateAstEntityLiteralProc(strdup(l->u.proc.arg.id), \
                                         CopyAst(l->u.proc.arg.type), \
                                         CopyAst(l->u.proc.body));
      default:
        error_abort("malformed literal tag! aborting", __FILE__, __LINE__);
    }
  }
}

Ast* CopyAstEntity(Ast* entity)
{
  if (entity != NULL) {
    Entity* e = &(entity->u.entity);
    switch(e->tag) {
      case E_ID:
        return CreateAstEntityId(strdup(e->u.id), NULL);
        break;
      case E_TYPE:
        return CopyAstEntityType(entity);
        break;
      case E_LITERAL:
        return CopyAstEntityLiteral(entity);
        break;
      default:
        error_abort("malformed entity tag! aborting", __FILE__, __LINE__);
    }
  }
}

Ast* CopyAstCall(Ast* call)
{
  if (call != NULL) {
    return CreateAstCall(CopyAst(call->u.call.lhs), CopyAst(call->u.call.rhs));
  }
}

Ast* CopyAstBinop(Ast* binop)
{
  if (binop != NULL) {
    return CreateAstBinop(strdup(binop->u.binop.op),   \
                          CopyAst(binop->u.binop.lhs), \
                          CopyAst(binop->u.binop.rhs));
  }
}

Ast* CopyAstUnop(Ast* unop)
{
  if (unop != NULL) {
    return CreateAstUnop(strdup(unop->u.unop.op), \
                         CopyAst(unop->u.unop.rhs));
  }
}


char* AstEntityTypeToString(Ast*);
char* AstEntityProcToString(Ast*);
char* AstEntityToString(Ast*);
char* AstCallToString(Ast*);
char* AstBinopToString(Ast*);
char* AstUnopToString(Ast*);


char* AstEntityTypeToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    Type* type = &(ast->u.entity.u.type);

    switch (type->tag) {
      case T_NIL: {
        result = strdup("Nil");
        break;
      }

      case T_POLY: {
        result = strdup("Poly");
        break;
      }

      case T_PROC: {
        char* t1 = AstToString(type->u.proc.lhs);
        if (!t1) {
          error_abort("malformed type! aborting", __FILE__, __LINE__);
        }
        char* t2 = AstToString(type->u.proc.rhs);
        if (!t2) {
          error_abort("malformed type! aborting", __FILE__, __LINE__);
        }
        char* lprn = "(", *rprn = ")";
        char* rarrow = " -> ";
        int len;
        // this could seg-fault, but only if the tree is malformed.
        // this is also the perfect place for a local function.
        bool grouped = type->u.proc.lhs->u.entity.u.type.tag == T_PROC;
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

char* AstEntityLiteralToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    Literal* l = &(ast->u.entity.u.literal);
    switch (l->tag) {
      case L_NIL:
        result = strdup("nil");
        break;
      case L_PROC: {
        char *bs = "\\ ", *cln = " : ", *reqarw = " => ";
        char* arg_id = strdup(ast->u.entity.u.lambda.arg.id.s);
        if   (arg_id == NULL) {
          error_abort("malformed arg id! aborting", __FILE__, __LINE__);
        }

        char* arg_type = AstToString(ast->u.entity.u.lambda.arg.type);
        if   (arg_type == NULL) {
          error_abort("malformed arg type! aborting", __FILE__, __LINE__);
        }

        char* body = AstToString(ast->u.entity.u.lambda.body);
        if   (body == NULL) {
          error_abort("malformed body! aborting", __FILE__, __LINE__);
        }

        if (ast->u.entity.u.lambda.arg.type == T_POLY) {
          int len = strlen(bs)     \
                  + strlen(arg_id) \
                  + strlen(reqarw) \
                  + strlen(body) + 1;
          result = (char*)calloc(len, sizeof(char));
          strcat(result, bs);
          strcat(result, arg_id);
          strcat(result, reqarw);
          strcat(result, body);
        } else {
          int len = strlen(bs)       \
                  + strlen(arg_id)   \
                  + strlen(cln)      \
                  + strlen(arg_type) \
                  + strlen(reqarw)   \
                  + strlen(body) + 1;
          result  = (char*)calloc(len, sizeof(char));
          strcat(result, bs);
          strcat(result, arg_id);
          strcat(result, cln);
          strcat(result, arg_type);
          strcat(result, reqarw);
          strcat(result, body);

        }
        free(arg_id);
        free(arg_type);
        free(body);
      }
    }
  }
  return result;
}

char* AstEntityToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    switch (ast->u.entity.tag) {
      case E_ID: {
        result = strdup(ast->u.entity.u.id);
        break;
      }
      case E_TYPE: {
        result = AstEntityTypeToString(ast);
        break;
      }
      case E_LITERAL: {
        result = AstEntityLiteralToString(ast);
        break;
      }
      default:
        error_abort("malformed entity tag! aborting", __FILE__, __LINE__);
    }
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
    strcat(result, lprn);
    strcat(result, lhs);
    strcat(result, rprn);
    strcat(result, spc);
    strcat(result, rhs);
    free(lhs);
    free(rhs);
  }
  return result;
}

char* AstBinopToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {

    char * spc = " ";

    char* op = ast->u.binop.op;

    char* lhs = AstToString(ast->u.binop.lhs);
    if (lhs == NULL) {
      error_abort("malformed binop lhs! aborting", __FILE__, __LINE__);
    }

    char* rhs = AstToString(ast->u.bind.term);
    if (rhs == NULL) {
      error_abort("malformed binop rhs! aborting", __FILE__, __LINE__);
    }

    int len = strlen(lhs) + 1 + strlen(op) + 1 + strlen(rhs) + 1;
    result  = (char*)calloc(len, sizeof(char));
    strcat(result, lhs);
    strcat(result, spc);
    strcat(result, op);
    strcat(result, spc);
    strcat(result, rhs);
    free(lhs);
    free(rhs);
  }
  return result;
}

char* AstUnopToString(Ast* ast)
{
  char* result = NULL;
  if (ast != NULL) {
    char* spc = " ";
    char* op  = ast->u.unop.op;
    char* rhs = AstToString(ast->u.unop.rhs);

    int len = strlen(op) + 1 + strlen(rhs) + 1;
    result = (char*)calloc(len, sizeof(char));
    strcat(result, op);
    strcat(result, spc);
    strcat(result, rhs);
    free(rhs);
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
      case N_CALL: {
        result = AstCallToString(ast);
        break;
      }
      case N_BINOP: {
        result = AstBinopToString(ast);
        break;
      }
      case N_UNOP: {
        result = AstUnopToString(ast);
        break;
      }
      default:
        error_abort("malformed type tag! aborting", __FILE__, __LINE__);
    }
  }
  return result;
}
