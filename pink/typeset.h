#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <unordered_map>
using std::unordered_map;

#include "type.h"

// 
// map of symname -> bool
// we can insert new symbols, assigned a new value.
// then when we want to 'hash' the symbol, so we can then lookup it's assigned value.
// and when we want to test for set membership we get that value
// and look it up in the symset, which is a bit-compact,
// random-access lookup, based on the string value, that can dynamically grow.
// the speed is payed for by

// naive implementation.
class Symset {
	vector<bool> set;
	Symenum  symenum;

public:
	int get_sym_num(string& str) {
		return symenum.lookup(type);
	}

	void insert(string& str) {
		auto symnum = get_sym_num(type);
		if (set.size() < symnum)
			set.insert((set.begin() + symnum), true);
		else set[symnum] = true;
	}

	bool lookup(string& str) {
		auto symnum = get_sym_num(type);
		if (set.size() < symnum) {
			set.insert((set.begin() + symnum), false);
			return false;
		}
		else return set[symnum];
			
	}
};

// the map here is being used as a
// dynamic enum. w/fast lookup
class Symenum {
	int numsyms = 1;
	unordered_map<string, int> map;

public:
	void insert(string& str) {
		map[str.c_str()] = numsyms;
		numsyms++;
	}
	// 0 is the implicit error case;
	int lookup(string& str) {
		return map[str.c_str()];
	}
};