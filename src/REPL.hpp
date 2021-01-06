
#pragma once
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


void Repl(istream& ins, ostream& outs, Environment& env);
void Frepl(ifstream& fins, ostream& outs, Environment& env);
