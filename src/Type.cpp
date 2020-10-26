
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Type.hpp"

shared_ptr<Type> Type::clone()
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

shared_ptr<Type> MonoType::clone_internal()
{
  return shared_ptr<Type>(new MonoType(*this));
}

string MonoType::to_string_internal()
{
  switch (tag)
  {
    case AtomicType::None:
      return "";
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
    case AtomicType::None:
      return false;
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

shared_ptr<Type> ProcType::clone_internal()
{
  return shared_ptr<Type>(new ProcType(lhs->clone(), rhs->clone(), location));
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

TypeJudgement TypesEquivalent(shared_ptr<Type> t1, shared_ptr<Type> t2)
{
  MonoType* mt1 = dynamic_cast<MonoType*>(t1.get());
  MonoType* mt2 = dynamic_cast<MonoType*>(t2.get());

  if (mt1 != nullptr)
  {
    if (mt2 != nullptr)
    {
      if (mt1->tag == mt2->tag)
      {
        return TypeJudgement(t1->clone());
      }
      else
      {
        string errdsc = "Atomic Types t1:["
                      + t1->to_string()
                      + "] t2:["
                      + t2->to_string()
                      + "] not equal\n";
        TypeError typeerror(Location(), errdsc);
        return TypeJudgement(typeerror);
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
    ProcType* pt1 = dynamic_cast<ProcType*>(t1.get());
    ProcType* pt2 = dynamic_cast<ProcType*>(t2.get());

    if (pt1 != nullptr)
    {
      if (pt2 != nullptr)
      {
        TypeJudgement j1 = TypesEquivalent(pt1->lhs, pt2->lhs);

        if (j1.succeeded())
        {
          TypeJudgement j2 = TypesEquivalent(pt1->rhs, pt2->rhs);

          if (j2.succeeded())
          {
            return TypeJudgement(t2->clone());
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
                      + t1->to_string()
                      + "] to non-proc type t2:["
                      + t2->to_string()
                      + "]";
        TypeError typeerror(Location(), errdsc);
        return TypeJudgement(typeerror);
      }
    }
    else
    {
      throw "bad type node";
    }
  }
}
