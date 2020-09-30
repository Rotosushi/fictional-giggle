#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "SymbolTable.hpp"


class Entity
{
public:
  Location location;
  Entity() = delete;
  Entity(const Location& loc) : loc(loc) {}
  virtual ~Entity() = default;

  virtual string to_string() { return to_string_internal(); }
  virtual TypeJudgement getype(SymbolTable* env) { return getype_internal(env); }
  virtual shared_ptr<Entity> evaluate(SymbolTable* env) { return evaluate_internal(env); }
protected:
  virtual string to_string_internal() = delete;
  virtual TypeJudgement getype_internal(SymbolTable* env) = delete;
  virtual shared_ptr<Entity> evaluate_internal(SymbolTable* env) = delete;
};
