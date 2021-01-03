/*
  so an import directive should
  read in a file which contains
  some valid Pink code. This code
  is then brought into the current
  interpretive/compilation context.

  this is kindof a scary term for me,
  it brings to bear two very important
  concepts that need to be carefully
  considered. namely, language directives.
  that is special terms in the language
  that do specialized things that are not
  possible from user code, but need to exist
  for a proper language to form.
  (others are exceptions, I/O facilities, etc.)
  the other concept that starts to be teased
  is modules/namespaces. which is critical to
  get right if you want to be able to disconnect
  the compilation of distinct units of code.
  for c, they went with the preprocessor to
  encapsulate the idea of a language directive.
  this is disconnected from the idea of type
  modifiers (making types const, or pointers, etc.)
  each directive is carried out within the preprocessor,
  this is coherent because C exists only in the
  compiled sense. when would the preprocessor run
  within a ficticious C interpretive environment?
  well, it would by nessecity be running alongside
  the interpreter, so that macros and the ability
  to import code could occur at any point in interpretation/
  the program text, just like when you compile C.
  oh, did you not know you can #include a file after
  the top of a C file, and not only will the compiler
  not complain, it will hapily insert the file into
  the other file directly at that location. which
  if your statements above the #include directive
  rely on data that is defined within the #included
  file, you will create definition errors when the
  program text does in fact define everything. (unless
  the error really is you forgot to define the name lol.))

  so, obviously I have to contend with an interpretive
  environment, alongside the compiled environment, which
  is already an additional layer of complexity. but
  additionally I am sick and tired of having to define
  things in the exact perfect order, I would love for
  the compiler to be able to handle out of order declarations.
  I would love for the module mechanism to be cogent of that
  fact so that not only do we get to define our procedures
  naturally within the module (and not have to predefine
  any functions of anything like that.)
  (additionally, this allows the programmer to define the
  set of procedures in any order, and reduces the error
  rate of refactorings (moving a procedure definition
  is not cause for an error like it is in c/c++))
  and the names
  which get exported between modules are handled such that
  the programmer need not consider "pragma once" as it were.
  the export/import mechanisms need to be duals of eachother,
  and then, all of this data needs to be handled in such
  a way that we can consider compiling each separate file
  and combining their corresponding object files into a
  single executable to prodduce the exact same executable
  as bringing each file into the same compilation environment
  and transforming the main procedure (and recursively all
  procedures that main calls) into the executable program.
  (all this modulus optimizations, as obviously more
  optimizations are available to the environment which
  holds all the information representing the program at once.)

  now, how to accomplish all of this is something i am less
  sure about, so I am writing this Import class with the
  intention of writing a more generic interface for
  language directives in general at a later date,
  using what i have learned from this interface.

*/
#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <utility>
using std::pair;
#include <memory>
using std::shared_ptr;

#include <fstream>
using std::ifstream;


#include "Ast.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "Environment.hpp"

class Import : public Ast
{
  ifstream source_stream;
public:
  string source_file;

  Import(string src_fl, const Location& loc);
  Import(const Import& other);

protected:
  virtual shared_ptr<Ast> clone_internal() override;
  virtual string to_string_internal() override;
  virtual TypeJudgement getype_internal(Environment& env) override;
  virtual EvalJudgement evaluate_internal(Environment& env) override;
  virtual void substitute_internal(string& var, shared_ptr<Ast>* term, shared_ptr<Ast>& value, Environment& env) override;
  virtual bool appears_free_internal(string& var) override;
  virtual void rename_binding_internal(string& old_name, string& new_name) override;
};
