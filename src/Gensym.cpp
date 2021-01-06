#include <string>
using std::string;
using std::to_string;

#include "Gensym.hpp"

string gensym(string prefix)
{
  // this is partially the LISP semantics because
  // that's what i'm used too.
  static int gensym_counter{0};
  string result;
  if (prefix != "")
  {
    result = prefix;
    result += to_string(gensym_counter);
  }
  else
  {
    result = "P";
    result += to_string(gensym_counter);
  }
  gensym_counter += 1;
  return result;
}
