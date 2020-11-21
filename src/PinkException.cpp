#include <string>
using std::string;
using std::to_string;
#include <exception>
using std::exception;

#include "PinkException.hpp"

PinkException::PinkException(const char* errdsc, const char* file, int line)
{
  this->errdsc = string(errdsc) + " \nfile:" + file + " \nline:" + to_string(line);
}

const char* PinkException::what()
{
  return errdsc.c_str();
}
