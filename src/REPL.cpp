
#include <string>
using std::string;
using std::getline;
#include <vector>
using std::vector;
#include <list>
using std::list;
#include <memory>
using std::shared_ptr;
#include <iostream>
using std::istream;
using std::ostream;
using std::endl;
#include <fstream>
using std::ifstream;
#include <exception>
using std::exception;

#include "Ast.hpp"
#include "Entity.hpp"
#include "Parser.hpp"
#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"
#include "Environment.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "PinkError.hpp"
#include "PinkKernel.hpp"
#include "PinkException.hpp"
#include "REPL.hpp"


void Repl(istream& ins, ostream& outs, Environment& env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };

  string input_text;

  try {
  do {
    outs << ":> ";
    getline (ins, input_text);


    ParserJudgement termjdgmt = env.parser->parse(input_text);

    if (termjdgmt)
    {
      shared_ptr<Ast> term = termjdgmt.u.jdgmt;

      TypeJudgement typejdgmt = term->getype(env);

      if (env.cleanup_list->size() > 0)
      {
        // destructors get called here
        for (string& id : (*env.cleanup_list))
        {
          env.scope->unbind(id);
        }

        env.cleanup_list->clear();
      }

      if (typejdgmt)
      {
        outs << "type:[" + typejdgmt.u.jdgmt->to_string() + "]" << endl;

        EvalJudgement evaljdgmt;

        evaljdgmt = term->evaluate(env);

        if (evaljdgmt && is_entity(evaljdgmt.u.jdgmt))
        {
          outs << "~> " << evaljdgmt.u.jdgmt->to_string() << endl;
        }
        else if (!evaljdgmt)
        {
          // an evaluation error
          outs << buildErrStr(evaljdgmt.u.error, input_text) << endl;
        }
        // this is another location it is appropriate to
        // perform cleanup.
      }
      else
      {
        // a type error
        outs << buildErrStr(typejdgmt.u.error, input_text) << endl;
      }
    }
    else
    {
      // a parser error
      outs << buildErrStr(termjdgmt.u.error, input_text) << endl;
    }
    // here is where it is safe to do cleanup
    // before the next iteration of the R.E.P.L.
    input_text.clear();
  } while (true);
  }
  catch (PinkException& e)
  {
    outs << e.what() << endl;
  }

  return;
}

void Frepl(ifstream& fins, ostream& outs, Environment& env)
{
  auto is_entity = [](shared_ptr<Ast> term)
  {
    Entity* entity = dynamic_cast<Entity*>(term.get());
    return entity != nullptr;
  };

  string input_text;
  bool failed = false;

  try {
  do {
    outs << ":> ";
    getline (fins, input_text);


    ParserJudgement termjdgmt = env.parser->parse(input_text);

    if (termjdgmt)
    {
      shared_ptr<Ast> term = termjdgmt.u.jdgmt;

      TypeJudgement typejdgmt = term->getype(env);

      if (env.cleanup_list->size() > 0)
      {
        // destructors get called here
        for (string& id : (*env.cleanup_list))
        {
          env.scope->unbind(id);
        }

        env.cleanup_list->clear();
      }

      if (typejdgmt)
      {
        outs << "type:[" + typejdgmt.u.jdgmt->to_string() + "]" << endl;

        EvalJudgement evaljdgmt;

        evaljdgmt = term->evaluate(env);

        if (evaljdgmt)
        {
          outs << "~> " << evaljdgmt.u.jdgmt->to_string() << endl;
        }
        else
        {
          // an evaluation error
          outs << buildErrStr(evaljdgmt.u.error, input_text) << endl;
          failed = true;
        }
        // this is another location it is appropriate to
        // perform cleanup.
      }
      else
      {
        // a type error
        outs << buildErrStr(typejdgmt.u.error, input_text) << endl;
        failed = true;
      }
    }
    else
    {
      // a parser error
      outs << buildErrStr(termjdgmt.u.error, input_text) << endl;
      failed = true;
    }
    // here is where it is safe to do cleanup
    // before the next iteration of the R.E.P.L.
    input_text.clear();
  } while (!failed && !fins.eof());
  }
  catch (PinkException& e)
  {
    outs << e.what() << endl;
  }


  return;
}
