
#include "PinkError.hpp"

Location& PinkError::location()
{
  return loc;
}

string PinkError::what()
{
  return dsc;
}

PinkError& PinkError::operator=(const PinkError& other)
{
  loc = other.loc;
  dsc = other.dsc;
  return *this;
}

string buildErrStr(Location loc, string errdsc, string errtxt)
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
  result += errtxt;
  result += "\n";
  for (int i = 0; i < errtxt.length() + 10; i++) {
    if (i < loc.first_column + 3 || i > loc.last_column + 3)
      result += "-";
    else
      result += "^";
  }
  result += "\n";
  result += errdsc;
  result += "\n";
  return result;
}

string buildErrStr(PinkError e, string errtxt)
{
  return buildErrStr(e.location(), e.what(), errtxt);
}
