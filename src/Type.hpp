#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;

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

class RefType : public Type
{
public:
  shared_ptr<Type> ref_type;

  RefType(shared_ptr<Type> t, const Location& loc)
    : Type(loc), ref_type(t) {}

  RefType(const RefType& other)
    : Type(other.location), ref_type(other.ref_type) {}

protected:
  virtual shared_ptr<Type> clone_internal() override;
  virtual string to_string_internal() override;
  virtual bool is_poly_internal() override;
};

class ProcType : public Type
{
public:
  vector<shared_ptr<Type>> arg_types;
  shared_ptr<Type> return_type;

  ProcType(vector<shared_ptr<Type>>& arg_types, shared_ptr<Type>& return_type, const Location& loc)
    : Type(loc), arg_types(arg_types), return_type(return_type) {}

  ProcType(vector<shared_ptr<Type>>& proc_types, const Location& loc);

  ProcType(vector<pair<string,shared_ptr<Type>>>& args, shared_ptr<Type> ret_type);

  ProcType(vector<pair<string, shared_ptr<Type>>>& args, shared_ptr<Type> ret_type, const Location& loc);

  ProcType(const ProcType& other)
    : Type(other.location), arg_types(other.arg_types), return_type(other.return_type) {}

  ~ProcType() {}

protected:
  virtual shared_ptr<Type> clone_internal() override;
  virtual string to_string_internal() override;
  virtual bool is_poly_internal() override;
};


TypeJudgement TypesEquivalent(shared_ptr<Type> t1, shared_ptr<Type> t2);
