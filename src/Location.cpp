
#include "Location.hpp"

Location::Location()
  : first_line(0), first_column(0),
    last_line(0), last_column(0)
{

}

Location::Location(int fl, int fc, int ll, int lc)
  : first_line(fl), first_column(fc),
    last_line(ll),  last_column(lc)
{

}

Location::Location(const Location& other)
  : first_line(other.first_line), first_column(other.first_column),
    last_line(other.last_line), last_column(other.last_column)
{

}
