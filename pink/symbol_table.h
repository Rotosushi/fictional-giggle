#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <array>
using std::array;
#include <functional>
using std::hash;
#include <utility>
using std::pair;
#include <optional>
using std::optional;

#include "ast.h"
#include "type.h"

// so, a big question here is how big to
// make the symbol tables key array,
// I have landed on a medium size, which
// will probably hold any reasonable function
// without collisions. 
// even though we are using the std::hash to
// generate a very different number given even
// similar inputs. (which is nice given that strings,
// hash similarly under straightforward hash algorithms)
// because we are wrapping the hash around
// only 126 different locations our probability
// of collisions has been raised significantly.
// this is unavoidable, as allocating an
// array which could hold size_t different
// names would take up a massive amount of space.
// we have to choose somve value significantly less than
// size_t such that we still minimize collisions,
// as this reduces the amount of iteration done
// to find the declaration in the table.
// so we contain collisions in a list of declarations
// with different names but which hash to the same
// bucket.

constexpr auto SYM_TABLE_SIZE = 126;

class _symbol_table {
public:
	_symbol_table() {}

	void bind(const _vardecl& decl);
	void bind(const vector<_vardecl>& decls);
	void unbind(const _vardecl& decl);
	void unbind(const vector<_vardecl>& decls);
	void rebind(const _vardecl& decl);
	void rebind(const vector<_vardecl>& decls);

	auto begin() { return table.begin(); }
	auto end() { return table.end(); }

	// searches for a decl at position [hash(name) % SYM_TABLE_SIZE]
	// returns a refrence if it exists, otherwise nullopt_t
	optional<_vardecl&> operator[](const string& name);

private:
	array<vector<pair<string, _vardecl>>, SYM_TABLE_SIZE> table;
	
	
};



