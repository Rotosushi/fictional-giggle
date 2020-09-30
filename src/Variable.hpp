#pragma once
#include <string>
using std::string;
#include

#include "Entity.hpp"
#include "SymbolTable.hpp"

class Variable : public Entity
{
public:
  string id;

  Variable(const string& str, const Location& loc)
    : Entity(loc), id(str) {}
  Variable(const Variable& other)
    : Entity(other.loc), id(other.id) {}

protected:
  virtual string to_string_internal() override
  {
    return id;
  }

  virtual TypeJudgement getype_internal(SymbolTable* env)
  {
    /*
        id is-a-member-of env
    ---------------------------
    env |- (id : type = value) : type
    */
    optional<shared_ptr<Entity>> bound_term = (*env)[id];

    if (bound_term)
    {
      TypeJudgement bound_type = (*bound_term)->getype(env);
      return bound_type;
    }
    else
    {
      return Judgement(location, "variable [" + id + "] not defined in environment.");
    }
  }
};
