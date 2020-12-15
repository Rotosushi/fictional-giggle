#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
using std::get;
#include <memory>
using std::shared_ptr;

#include "Location.hpp"
#include "Type.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"

/*
  given the two following facts it may be profitable to
  cache the type of each Ast node in the node itself.
  (especially if it's simply a shared_ptr to the form.)
  1) typechecking runs before evaluation
  2) evaluation sometimes requires a type object to
      do it's work, meaning it -must- currently issue
      a call to getype. this can instead sometimes be replaced
      by an immediate return of a ptr.
  given the caching, we would expect to optimize evaluation
      somewhat. it may also optimize some typehecking, especially in
      cases where the algorithm must traverse the same tree over
      and over again. (say, everytime an expression involving a
      previously bound term must be typed.)
*/
class Ast
{
public:
  Location location;
  shared_ptr<Type> cached_type;
  Ast() = delete;
  Ast(const Location& loc) : location(loc), cached_type(nullptr) {}
  Ast(const Ast& other) : location(other.location), cached_type(other.cached_type) {}
  virtual ~Ast() {};

  shared_ptr<Ast> clone();
  string to_string();
  TypeJudgement getype(Environment env);
  EvalJudgement evaluate(Environment env);
  void substitute(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env);
  bool appears_free(vector<string>& names, vector<string>& appeared_free);
  void rename_binding_in_body(vector<pair<string, string>>& renaming_pairs);
protected:
  virtual shared_ptr<Ast> clone_internal() = 0;
  virtual string to_string_internal() = 0;
  virtual TypeJudgement getype_internal(Environment env) = 0;
  virtual EvalJudgement evaluate_internal(Environment env) = 0;
  virtual void substitute_internal(vector<pair<string, shared_ptr<Ast>>>& subs, shared_ptr<Ast>* term, Environment env) = 0;
  virtual bool appears_free_internal(vector<string>& names, vector<string>& appeared_free) = 0;
  virtual void rename_binding_in_body_internal(vector<pair<string, string>>& renaming_pairs) = 0;

  void insert_if_unique(string& name, vector<string>& names);

};
