#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;

/*
  how the heck do we support
  multiple arguments?

  what the syntax should say

  f a b c d ...

  is called like

  f (a, b, c, d, ...)

  and f can have the signature

  f := \a, b, c, d, ... => ...

  or

  f := \a => \b => \c => ... => ...
*/
