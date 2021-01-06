// TODO: write this file

/*
okay, so string interning, essentially,
every name that is constant within the
program can be interned. this means that
we carry out comparison in constant time
as integer comparison between the two
pointers to the interned strings.
since we are only ever looking to see if the
identifier is the one we are looking for
or not.
structures within the langauge that utilize
constant names, i.e. bound names, bound operators,
Procedure names and argument names.,
can store references to interned strings, and
when we need to do actual string comparison,
well, the pointer is to the series of characters itself.
the parser cannot intern strings right?
do all interned strings need to be unique?
*/

#pragma once
#include <string>
using std::string;
#include <list>
using std::list;


class StringInterner
{
  /*
    given our usage of iterators, we can almost
    replace the list with a vector. however given
    how often we insert into the middle of this
    set, this might be a case for a list.
    however all of these words mean so much less
    than a benchmark.
  */
  list<string> interned_strings;
  /*
    we are maintaining a set of strings such that
    each string is unique. so that we can use the
    pointer itself as a kind of identifier for the
    string itself. because we know that every
    "x" is the same "x", throughout every constant
    name in the program, which means when we need to
    compare two constant names, if they truly are
    the same string of characters they will be the
    same pointer.

    this optimization allows us to save on memory when
    dealing with potentially thousands or hundreds of
    thousands of terms are being created/typed/evaluated
    during interpretation or compilation.
    each object which tracks constant names has it's
    memory footprint made constant with respect to
    the constant strings, and the footprint is probably
    made smaller by the pointer being smaller than any
    string longer than one word of storage. (four chars if ascii
    encoded, etc.) additionally, the memory access pattern is
    also made simpler by the fact that the pointer itself is
    the comparison object, making comparison constant time,
    and requires no additional memory lookup other than having
    the object containing the pointer itself.
    and as such, is a planned upgrade, but it wholly immaterial
    to what the language actually is/does. this is essentially
    the flyweight pattern, but for constant strings accross
    interpretation/compilation.

    this optimization also presents additional issues
    w.r.t. multiple compilation units. which is why
    this is being held off for now.
  */

public:
  const char* Intern(string str);
  const char* Find(string str);
};
