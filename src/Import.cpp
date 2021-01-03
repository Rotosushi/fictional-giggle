#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;

#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"
#include "REPL.hpp"
#include "Import.hpp"

Import::Import(string src_fl, const Location& loc)
  : Ast(loc),
    source_stream(),
    source_file(src_fl)
{

}

Import::Import(const Import& other)
  : Ast(other.location),
    source_stream(other.source_stream),
    source_file(other.source_file)
{

}

shared_ptr<Ast> Import::clone_interal()
{
  return shared_ptr<Ast>(new Import(*this));
}

string Import::to_string_internal()
{
  return "import \"" + source_file + "\"";
}

TypeJudgement Import::getype_internal(Environment& env)
{
  return TypeJudgement(shared_ptr<Type>(new MonoType(AtomicType::None, location)));
}

EvalJudgement Import::evaluate_internal(Environment& env)
{
  source_stream.open(source_file);

  if (source_stream.good())
  {
    Frepl(fins, cout, env);
    source_stream.close();
    return EvalJudgement(shared_ptr<Ast>(new Entity((void*)nullptr, location));
  }
  else
  {
    string errdsc = "Couldn't open File: \""
                  + source_file
                  + "\"";
    EvalError error{location, errdsc};
    return EvalJudgement(error);
  }
}

void substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env)
{
  return;
}

bool appears_free_internal(string& var)
{
  return false;
}

void rename_binding_internal(string& old_name, string& new_name)
{
  return;
}
