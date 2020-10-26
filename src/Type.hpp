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
  None,
  Poly,
  Nil,
  Int,
  Bool,
};

class Type
{
public:
  Location location;
  Type() = delete;
  Type(const Location& loc) : location(loc) {}
  virtual ~Type() {};

  virtual shared_ptr<Type> clone();
  virtual string to_string();
  virtual bool   is_polymorphic();
protected:
  virtual shared_ptr<Type> clone_internal() = 0;
  virtual string to_string_internal() = 0;
  virtual bool   is_poly_internal() = 0;
};

class MonoType : public Type
{
public:
  AtomicType tag;

  MonoType(AtomicType t, const Location& loc)
    : Type(loc), tag(t) {}
  MonoType(const MonoType& other)
    : Type(other.location), tag(other.tag) {}
  ~MonoType() {};

protected:
  virtual shared_ptr<Type> clone_internal() override;
  virtual string to_string_internal() override;
  virtual bool is_poly_internal() override;
};

class ProcType : public Type
{
public:
  shared_ptr<Type> lhs;
  shared_ptr<Type> rhs;

  ProcType(shared_ptr<Type> l, shared_ptr<Type> r, const Location& loc)
    : Type(loc), lhs(l), rhs(r) {}
  ProcType(const ProcType& other)
    : Type(other.location), lhs(other.lhs), rhs(other.rhs) {}
  ~ProcType() {}

protected:
  virtual shared_ptr<Type> clone_internal() override;
  virtual string to_string_internal() override;
  virtual bool is_poly_internal() override;
};


TypeJudgement TypesEquivalent(shared_ptr<Type> t1, shared_ptr<Type> t2);
