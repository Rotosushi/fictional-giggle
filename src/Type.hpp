#pragma once
#include <string>
using std::string;

#include "TypeJudgement.hpp"
/*
  type := Nil
        | Int
        | Bool
        | type -> type
*/

enum class AtomicType
{
  Poly,
  Nil,
  Int,
  Bool,
};

class Type
{
public:
  Type() = delete;
  virtual ~Type() {};

  string to_string() { return to_string_internal(); }
  bool   is_polymorphic() { return is_poly_internal(); }
protected:
  virtual string to_string_internal() = delete;
  virtual bool   is_poly_internal() = delete;
};

class MonoType
{
public:
  AtomicType tag;

  MonoType(AtomicType t) : tag(t) {}
  MonoType(const MonoType& other)
    : tag(other.tag) {}
  ~MonoType() {};

protected:
  virtual string to_string_internal() override
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

  virtual bool is_poly_internal() override
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
};

class ProcType
{
public:
  shared_ptr<Type> lhs;
  shared_ptr<Type> rhs;

  ProcType(shared_ptr<Type> l, shared_ptr<Type> r)
    : lhs(l), rhs(r) {}
  ProcType(const ProcType& other)
    : lhs(other.lhs), rhs(other.rhs) {}
  ~ProcType() {}

protected:
  virtual to_string_internal() override
  {
    string result;
    result  = lhs->to_string();
    result += " -> ";
    result += rhs->to_string();
    return result;
  }

  virtual is_poly_internal() override
  {
    return lhs->is_polymorphic() || rhs->is_polymorphic();
  }
};


TypeJudgement TypesEquivalent(const Type* t1, const Type* t2)
{
  MonoType* mt1 = dynamic_cast<MonoType*>(t1);
  MonoType* mt2 = dynamic_cast<MonoType*>(t2);

  if (mt1 != nullptr)
  {
    if (mt2 != nullptr)
    {
      if (mt1->tag == mt2->tag)
      {
        return Judgement(t1);
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
      return Judgement(typeerror);
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
        Judgement j1 = equivalent(pt1->lhs, pt2->lhs);

        if (j1.succeeded())
        {
          Judgement j2 = equivalent(pt2->rhs, pt2->rhs);

          if (j2.succeeded())
          {
            return Judgement(t1);
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
