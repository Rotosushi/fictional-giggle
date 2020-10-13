#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"

shared_ptr<Ast> Binop::clone_internal()
{
  return make_shared(Binop(op, lhs->clone(), rhs->clone(), location));
}

string Binop::to_string_internal()
{
  string result;
  result += lhs->to_string();
  result += " ";
  result += op;
  result += " ";
  result += rhs->to_string();
  return result;
}

TypeJudgement Binop::getype_internal(SymbolTable* env, OperatorTable* ops)
{
  /*
    we find the Binop Eliminator via the BinopSet
    then ask the Eliminator for something with the
    types of the left and right hand side.
    we are relying on this Eliminator Lookup procedure
    to then uniquely select an elimination
    procedure. if this happens then this term is
    well formed. otherwise this term is untypable.
  */

}

EvalJudgement Binop::evaluate_internal(SymbolTable* env, OperatorTable* ops)
{

}
