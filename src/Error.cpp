
#include "Error.hpp"

Location Error::location()
{
  return loc;
}

string Error::what()
{
  return dsc;
}
