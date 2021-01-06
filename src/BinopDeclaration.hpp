#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

// TODO: write this file
class BinopDeclaration : public Ast
{
  /*
  binop declarations are a kind of formal
  procdure declaration, except that the name
  comes from the Token::Operator sequence
  of characters, instead of the Token::Id sequence
  that proceudres are stored under, and there
  must be two arguments. (the arguments could be
  polymorphic, i suppose.)
  */
};
