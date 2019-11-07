#include <string>
using std::string;
#include "type.h"


string to_string(_type t)
{
	switch (t) {
	case _ERR: return "ERR";
	case _DEDUCE: return "DEDUCE";
	case _FCALL: return "FCALL";
	case _NIL: return "NIL";
	case _INT: return "INT";
	case _REAL: return "REAL";
	case _TEXT: return "TEXT";
	case _BOOL: return "BOOL";
	}
}
