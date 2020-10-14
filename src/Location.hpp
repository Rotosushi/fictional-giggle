#pragma once

class Location
{
public:
  int first_line;
  int first_column;
  int last_line;
  int last_column;

  Location();
  Location(int fl, int fc, int ll, int lc);
  Location(const Location& other);
};
