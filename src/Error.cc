
#include <string>
using std::string;

#include "Error.hh"

string buildErrStr(const Location& loc, const string& errdesc, const string& errtext)
{
  /*
    this function only really makes sense if the
    errtext does not contain a NewLn character.

    should we be reading an entire input file, it
    will be that we extract the erroneous line
    from the entire source text before calling
    this function.

    result :=
    some-line-of-input-text-which-failed
    -------------^^^^^------------------
    a-line-of-text-describing-the-error-found
  */
  string result("   ");
  result += errtext;
  result += "\n";
  for (int i = 0; i < errtext.length() + 10; i++) {
    if (i < loc.first_column + 3 || i > loc.last_column + 3)
      result += "-";
    else
      result += "^";
  }
  result += "\n";
  result += errdesc;
  result += "\n";
  return result;
}

string buildErrStr(const ParserError& prserr, const string& errtext)
{
  return buildErrStr(prserr.location(), prserr.what(), errtext);
}
