
#include <stdio.h>
#include <stdlib.h>

#include "printer.h"
#include "ast.h"


void PrintAstType(Ast*);
void PrintAstId(Id*);
void PrintAstLambda(Lambda*);
void PrintAstCall(Call*);
void PrintAstBind(Bind*);

/*
  this is a rather ineffecient solution,
  here we are using function calls to
  traverse the tree structure recursively
  and as an action while traversing, we
  print the data contained within the node
  a better solution would construct a string
  representation of the ast recursively,
  and then print once.
*/
void PrintAst(Ast* ast)
{
  if (ast != NULL) {
    switch(ast->tag) {
      case N_TYPE:   {
        PrintAstType(ast);
        break;
      }
      case N_ID:     {
        PrintAstId(&(ast->u.id));
        break;
      }
      case N_LAMBDA: {
        PrintAstLambda(&(ast->u.lambda));
        break;
       }
      case N_CALL:   {
        PrintAstCall(&(ast->u.call));
        break;
       }
      case N_BIND:   {
        PrintAstBind(&(ast->u.bind));
        break;
      }
      default:
        fprintf(stderr, "Internal Error: Unexpected Ast NodeTag");
        exit(1);
    }
    printf("\n");
  }
}

void PrintAstType(Ast* type)
{
  if (type != NULL) {
    switch(type->u.type.tag) {
      case T_INFER: {
          printf("infer");
        break;
      }
      case T_NIL: {
          printf("nil");
          break;
      }
      case T_FUNC: {
          PrintAstType(type->u.type.u.rarrow.lhs);
          printf(" -> ");
          PrintAstType(type->u.type.u.rarrow.rhs);
      }
    }
  }
}

void PrintAstId(Id* id)
{
  if (id != NULL) {
    printf("%s", id->s);
  }
}

void PrintAstLambda(Lambda* lambda)
{
  if (lambda != NULL) {
    printf("\\ ");
    PrintAstId(&(lambda->arg.id));
    printf(" : ");
    PrintAstType(lambda->arg.type);
    printf(" => ");
    PrintAst(lambda->body);
  }
}

void PrintAstCall(Call* call)
{
  if (call != NULL) {
    PrintAst(call->lhs);
    printf(" ");
    PrintAst(call->rhs);
  }
}

void PrintAstBind(Bind* bind)
{
  if (bind != NULL) {
    PrintAstId(&(bind->id));
    printf(" := ");
    PrintAst(bind->term);
  }
}
