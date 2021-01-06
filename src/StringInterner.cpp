// TODO: write this file
#include <string>
using std::string;
#include <utility>
using std::pair;
using std::make_pair;
using std::get;
#include <map>
using std::map;

#include "StringInterner.hpp"

const char* StringInterner::Intern(string str)
{
  /*
    because I like to think I am clever,
    I am storing interned strings in alphabetical
    order. this combines the cases for early return
    in this algorithm. namely, if we find a match
    we can return the proper information immediately,
    and once we find a name that is larger than
    the name we are searching for we know there is
    no way for the name to appear so we can insert immediately,
    and we get an extra early return. only in the worst case
    of actually needing to insert at the end of the list
    will this algorithm ever actually string compare against
    each interned string.
  */
  for (auto istr = interned_strings.begin(); istr != interned_strings.end(); istr++)
  {
    int cmpval = str.compare((*istr));

    if (cmpval < 0)
      continue; // there is a hole here
    else if (cmpval == 0)
    {
      return (*istr).data();
    }
    else
    {
      // so, we just emplaced into the
      // position just before istr.
      // which means (istr - 1) is
      // an iterator to the string we just
      // inserted into the list.
      // this usage of emplace is also
      // why we need this form of the
      // for loop over the for each style.
      interned_strings.emplace(istr, str);
      return (*(--istr)).data();
    }
  }

  /*
  if we get here, we looked through the entire list
     and only found names that were smaller than what
     we were searching for. meaning we can insert at the
      end of the list. additionally, returning the data
      at the end of the list, otherwise known as the
      string we just interned.
  */
  interned_strings.emplace_back(str);
  return interned_strings.back().data();
}


const char* StringInterner::Find(string str)
{
  for (auto& str : interned_strings)
  {
    if (str.compare(str) == 0)
    {
      return str.data();
    }
  }

  return nullptr;
}
