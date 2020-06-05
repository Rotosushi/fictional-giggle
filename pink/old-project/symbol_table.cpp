//#include <string>
//using std::string;
//#include <vector>
//using std::vector;
//#include <functional>
//using std::hash;
//#include <utility>
//using std::pair;
//#include <optional>
//using std::optional;
//using std::nullopt;
//#include <exception>
//using std::exception;
//#include "symbol_table.h"
//
//#include "error.h"
//
//void _symbol_table::bind(const _vardecl& decl)
//{
//	auto idx = hash<string>()(decl.lhs.id) % SYM_TABLE_SIZE;
//	for (auto&& d : table[idx]) // ensure we do not create duplicate bindings
//		if (d.first == decl.lhs.id)
//			throw _semantic_error(__FILE__, __LINE__, "variable already defined: " + decl.lhs.id);
//	table[idx].push_back(pair<string, _vardecl>(decl.lhs.id, decl));
//}
//
//void _symbol_table::bind(const vector<_vardecl>& decls)
//{
//	for (auto&& decl : decls)
//		bind(decl);
//}
//
//void _symbol_table::unbind(const _vardecl& decl)
//{
//	auto idx = hash<string>()(decl.lhs.id) % SYM_TABLE_SIZE;
//	for (auto&& p : table[idx]) // find the bucket where our decl is, and iterate through it
//		if (p.first == decl.lhs.id) // if the names match, we overwrite the declaration
//			p = pair<string, _vardecl>();
//}
//
//void _symbol_table::unbind(const vector<_vardecl>& decls)
//{
//	for (auto&& decl : decls)
//		unbind(decl);
//}
//
//void _symbol_table::rebind(const _vardecl& decl)
//{
//	bool written = false;
//	auto idx = hash<string>()(decl.lhs.id) % SYM_TABLE_SIZE;
//	for (auto&& p : table[idx]) // find the bucket where our decl is, and iterate through it
//		if (p.first == decl.lhs.id) { // if the names match, we overwrite the declaration
//			p = pair<string, _vardecl>(decl.lhs.id, decl);
//			written = true;
//			break; // since we wrote the decl, there is no point to continuing to iterate.
//		}
//	// if we didn't overwrite a name, then we didn't write the decl
//	// so we push it to the end of the vector.
//	if (!written) table[idx].push_back(pair<string, _vardecl>(decl.lhs.id, decl));
//}
//
//void _symbol_table::rebind(const vector<_vardecl>& decls)
//{
//	for (auto&& decl : decls)
//		rebind(decl);
//}
//
//optional<_vardecl&> _symbol_table::operator[](const string& str)
//{
//	auto idx = hash<string>()(str);
//	auto&& pairs = table[idx];
//	for (auto&& p : pairs) {
//		auto&& decl = p.second;
//		if (decl.lhs.id == str) // if the id matches then this is the decl we are looking for.
//			return decl;
//		else return nullopt;
//	}
//	return nullopt;
//}
