#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

#include "Entity.hpp"
#include "SymbolTable.hpp"
#include "TypeJudgement.hpp"

class Bind : public Entity
{
public:
  string id;
  shared_ptr<Entity> rhs;

  Bind(const string& str, shared_ptr<Entity> r, const Location& l)
    : Entity(l), id(str), rhs(r) {}

  Bind(const Bind& other)
    : Entity(other.location), id(other.id), rhs(other.rhs) {}

protected:
    virtual string to_string_internal() override
    {
      string result;
      result  = id;
      result += " := ";
      result += rhs->to_string();
      return result;
    }

    virtual TypeJudgement getype_internal(SymbolTable* env) override
    {
      /*
    ENV |- id is-not-currently-bound-in-env, term2 : type2
          ------------------------------------------
                ENV |- id := term2 : type2
      */
      optional<shared_ptr<Entity>> sym = (*env)[id];

      if (sym)
      {
        string errdsc = "id ["
                      + id
                      + "] is already bound to ["
                      + rhs->to_string()
                      + "]\n";
        return TypeJudgement(location, errdsc);
      }
      else
      {
        // recal that judgements track their own failure
        // state, so it need not be explicitly mentioned
        // that this call can fail here.
        TypeJudgement rhstype = rhs->getype(env);
        return rhstype;
      }
    }
};
