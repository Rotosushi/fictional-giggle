
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Type.hpp"

shared_ptr<Ast> Type::clone()
{
  return clone_internal();
}

string Type::to_string()
{
  return to_string_internal();
}

bool Type::is_polymorphic()
{
  return is_poly_internal();
}

shared_ptr<Ast> MonoType::clone_internal()
{
  return make_shared(MonoType(*this));
}

string MonoType::to_string_internal()
{
  switch (tag)
  {
    case AtomicType::Poly:
      return "Poly";
    case AtomicType::Nil:
      return "Nil";
    case AtomicType::Int:
      return "Int";
    case AtomicType::Bool:
      return "Bool";
    default:
      throw "bad AtomicType tag.";
  }
}

bool MonoType::is_poly_internal()
{
  switch (tag)
  {
    case AtomicType::Poly:
      return true;
    case AtomicType::Nil:
      return false;
    case AtomicType::Int:
      return false;
    case AtomicType::Bool:
      return false;
    default:
      throw "bad AtomicType tag.";
  }
}

shared_ptr<Ast> ProcType::clone_internal()
{
  return make_shared(ProcType(lhs->clone(), rhs->clone()));
}

string ProcType::to_string_internal()
{
  string result;
  result  = lhs->to_string();
  result += " -> ";
  result += rhs->to_string();
  return result;
}

bool ProcType::is_poly_internal()
{
  return lhs->is_polymorphic() || rhs->is_polymorphic();
}

TypeJudgement TypesEquivalent(Type* t1, Type* t2)
{
  MonoType* mt1 = dynamic_cast<MonoType*>(t1);
  MonoType* mt2 = dynamic_cast<MonoType*>(t2);

  if (mt1 != nullptr)
  {
    if (mt2 != nullptr)
    {
      if (mt1->tag == mt2->tag)
      {
        return TypeJudgement(t1);
      }
      else
      {
        string errdsc = "Atomic Types t1:["
                      + t1->to_string()
                      + "] t2:["
                      + t2->to_string()
                      + "] not equal\n";
        TypeError typeerror(Location(), errdsc);
        return Judgement(typeerror);
      }
    }
    else
    {
      string errdsc = "cannot compare atomic type t1:["
                    + t1->to_string()
                    + "] to non-atomic type t2:["
                    + t2->to_string()
                    + "] not equal\n";
      TypeError typeerror(Location(), errdsc);
      return TypeJudgement(typeerror);
    }
  }
  else
  {
    ProcType* pt1 = dynamic_cast<ProcType*>(t1);
    ProcType* pt2 = dynamic_cast<ProcType*>(t2);

    if (pt1 != nullptr)
    {
      if (pt2 != nullptr)
      {
        TypeJudgement j1 = TypesEquivalent(pt1->lhs, pt2->lhs);

        if (j1.succeeded())
        {
          TypeJudgement j2 = TypesEquivalent(pt2->rhs, pt2->rhs);

          if (j2.succeeded())
          {
            return TypeJudgement(t1);
          }
          else
          {
            return j2;
          }
        }
        else
        {
          return j1;
        }
      }
      else
      {
        string errdsc = "cannot compare proc type t1:["
                      + t1->to_string();
                      + "] to non-proc type t2:["
                      + t2->to_string();
                      + "]";
        TypeError typeerror(Location(), errdsc);
        return Judgement(typeerror);
      }
    }
    else
    {
      throw "bad type node";
    }
  }
}
