#pragma once
// for holding string locations.
class Location {
public:
  int first_line;
  int first_column;
  int last_line;
  int last_column;


  Location() {
    first_line   = 0;
    first_column = 0;
    last_line    = 0;
    last_column  = 0;
  }

  Location(int fl, int fc, int ll, int lc) {
    first_line   = fl;
    first_column = fc;
    last_line    = ll;
    last_column  = lc;
  }

  Location(const Location& loc) {
    first_line   = loc.first_line;
    first_column = loc.first_column;
    last_line    = loc.last_line;
    last_column  = loc.last_column;
  }

  Location& operator=(const Location& rhs) {
    first_line   = rhs.first_line;
    first_column = rhs.first_column;
    last_line    = rhs.last_line;
    last_column  = rhs.last_column;
    return *this;
  }

  ~Location() {
    first_line   = 0;
    first_column = 0;
    last_line    = 0;
    last_column  = 0;
  }
};
