
#include <string>
#include <memory>
using std::unique_ptr;
#include <utility>
using std::move;
#include <list>
using std::list;


string _AstToString(const Variable& var);
string _AstToString(const Call& call);
string _AstToString(const Bind& bind);
string _AstToString(const Binop& binop);
string _AstToString(const Unop& unop);
string _AstToString(const Cond& cond);
string _AstToString(const MonoType& mt);
string _AstToString(const PolyType& pt);
string _AstToString(const ProcType& pt);

string AstToString(const Ast& a)
{
  return _AstToString(a);
}

string AstToString(const Type& t)
{
  return _AstToString(t);
}
