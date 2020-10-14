
#include <string>
using std::string;
using std::getline;
#include <memory>
using std::shared_ptr;
#include <iostream>
using std::cin;
using std::cout;
using std::endl;

#include "Ast.hpp"
#include "Entity.hpp"
#include "Parser.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "TypeJudgement.hpp"
#include "EvalJudgement.hpp"
#include "PinkError.hpp"
#include "PinkKernel.hpp"


/*
  this is a very early version of this
  language, and as such each of the elements
  are in a very literal form, which is wholly
  unoptimized. this will change, but only after
  the exact semantics are nailed down, and
  therefore, we have a stable api to optimize
  against.
*/

/*
  a binop will have two distinct places which need
  to be maintained in lockstep to support
  the full binop semantics. first, the precedence
  and associativity of each op will be stored
  in a lookup table to be used by the parser.
  we want to let the parser run as fast a possible,
  so we are not keeping the eliminators stored
  along with the precedence and associativity.
  instead, in addition to the table of
  operator precedence and associativity, the
  record containing the elimination procedures will be
  registed within the environment, which then allows
  the usual lookup mechanism to be used in
  getting the elimination procedures, and
  if we further use the "HasInstance"
  pattern, we can hide the fact that we
  are keeping two kinds of eliminators
  and treating them as a single set.
  for primitive elimators, we are going to
  use function pointers, and then the procedure
  being pointed to will be able to enforce
  the specific semantics of applying specific
  binops, returning the resulting value after the
  operation is applied. if, instead of being able
  to find a primitive eliminator we would look
  at the programmers overloads, and this will be
  stored as a two argument procedure. however we
  decide to allow that.
  to me, separating these three things,
  precedence and associativity,
  primitive eliminators, and
  composite eliminators.
  allows the language to allow
  primitive operators before we have any way
  of declaring two argument procedures.
  this is basically what is happening with
  procedure overloading as well, because
  we don't currently allow a programmer to
  overload a procedure in the syntax. even
  though most of the underlying mechanisms
  are built. this will allow me to get much
  of the language stood up and working before
  I have to figure out the hard problem that
  is multiple dispatch. for now, the language
  has essentially single-dispatch.
  this is a direct consequence of procedures
  only ever having one argument.
*/


int main(int argc, char** argv)
{
  string input_text;
  SymbolTable top;
  OperatorTable ops;

  RegisterPrimitiveBinops(&ops);
  RegisterPrimitiveUnops(&ops);

  Parser parser(&top, &ops);

  cout << "Welcome to Pink! v0.0.2\n"
       << "press ctrl+c to exit.\n";

  do {
    cout << ":> ";
    getline (input_text, cin);

    ParserJudgement term = parser.parse(input_text);

    /*
    so theoretically we can flatten this
    if tree by testing for the terms being
    unsuccessful, which then a continue
    allows us to skip the unneeded portions
    of the loop. I dunno which is better
    considering this isn't a very big tree.
    */
    if (term.succeeded())
    {
      shared_ptr<Ast> ast = term.u.term;
      TypeJudgement ast_type = ast->getype(&top, &ops);

      if (ast_type.succeeded())
      {
        cout << "\ntype:[" + ast_type.u.judgement->to_string() + "]" << endl;
        /*
        evaluation is the farthest unit from working,
        but once it does, this is the expected usecase
        of the evaluate() procedure.

        EvalJudgement eval_term = ast->evaluate(&top, &ops);

        if (eval_term.succeeded())
        {
          cout << ":=> " << eval_term.u.judgement->to_string() << endl;
        }
        else
        {
          cout << buildErrStr(eval_term.u.err) << endl;
        }
        */
        // here is where it is safe to do cleanup
        // before the next iteration of the R.E.P.L.
        input_text.clear();
      }
      else
      {
        cout << buildErrStr(ast_type.u.err) << endl;
      }
    }
    else
    {
        cout << buildErrStr(term.u.err) << endl;
    }
  } while ();
}


















/* ------------------------------------------------------- */
