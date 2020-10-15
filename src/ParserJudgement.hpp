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
    ParserError err;
    shared_ptr<Ast> jdgmt;

    ~U() {}
    U() : err("Default ParserJudgement", Location()) {}
    U(const ParserError& err) : err(err) {}
    U(const shared_ptr<Ast>& term) : jdgmt(term) {}
  } u;

  ParserJudgement() {}
  ParserJudgement(const ParserError& err) : success(false), u(err) {}
  ParserJudgement(const shared_ptr<Ast>& term) : success(true), u(term) {}

  bool succeeded();
  operator bool();
};
