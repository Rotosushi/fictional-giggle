#pragma once
#include <string>
using std::string;
using std::to_string;
#include <exception>
using std::exception;

class PinkException : public exception
{
  string errdsc;

public:
  PinkException(const char* errdsc, const char* file, int line);

    virtual const char* what();

};
