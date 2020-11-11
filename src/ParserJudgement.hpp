#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;

class Ast;
#include "ParserError.hpp"

class ParserJudgement
{
public:
  bool success;
  union U {
    ParserError error;
    shared_ptr<Ast> jdgmt;

    ~U() {}
    U() : error("Default ParserJudgement", Location()) {}
    U(ParserError err) : error(err) {}
    U(shared_ptr<Ast> term) : jdgmt(term) {}
  } u;

  ParserJudgement() {}
  ParserJudgement(ParserError err) : success(false), u(err) {}
  ParserJudgement(shared_ptr<Ast> term) : success(true), u(term) {}

  bool succeeded();
  operator bool();
};
