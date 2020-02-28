//#include "control_flow_graph.h"
//#include "error.h"
//
//bool is_root(_control_block& cblk)
//{
//	return is_nth_bit_set(cblk.flags, (short)CBLK_FLAGS::ROOT_BLOCK);
//}
//
//bool is_end(_control_block& cblk)
//{
//	return is_nth_bit_set(cblk.flags, (short)CBLK_FLAGS::END_BLOCK);
//}
//
//void insert_edge(_control_flow_graph* from, _control_flow_graph* to)
//{
//}
//
//_control_flow_graph* create_control_flow_graph(_module& mdl)
//{
//	auto root_node = new _control_flow_graph;
//	auto end_node = new _control_flow_graph;
//	_control_block* root_block = new _control_block;
//	_control_block* end_block = new _control_block;
//	set_nth_bit(root_block->flags, (short)CBLK_FLAGS::ROOT_BLOCK);
//	set_nth_bit(end_block->flags, (short)CBLK_FLAGS::END_BLOCK);
//	_fn* root_fn;
//	stack<_scope> scope_stack;
//	/* 
//		we need to construct the graph starting from
//		the root function of the program. 
//	*/
//	if (mdl.root_name == "")
//		root_fn = &(lookup_fn("main", {}, mdl.functions));
//	else
//		root_fn = &(lookup_fn(mdl.root_name, {}, mdl.functions));
//
//	int i = 0;
//	_control_flow_graph* current_node = nullptr, * previous_node = root_node;
//	for (int i = 0; i < root_fn->body.statements.size(); i++) {
//		auto&& stmt = root_fn->body.statements[i];
//		if (current_node == nullptr) 
//			current_node = new _control_flow_graph;
//		
//		construct_control_block(stmt, current_node, root_fn, scope_stack, mdl.functions);
//
//		previous_node = current_node;
//		current_node = nullptr;
//		if (i == (root_fn->body.statements.size() - 1)) {
//			insert_edge(current_node, end_node);
//		}
//	}
//
//}
//
//void construct_control_block(Ast* stmt, _control_flow_graph* node, _fn* fn, stack<_scope> scope_stack, function_table& functions)
//{
//	switch (stmt->ast_type) {
//	case AST_IF: {
//
//	}
//	case AST_WHILE: {
//
//	}
//	case AST_FCALL: {
//
//	}
//	case AST_SCOPE: {
//
//	}
//	default: {
//
//	}
//	}
//}
//
//void construct_if_cblk(_control_flow_graph* node, _if* cond, _fn& local_scope, stack<_scope> scope_stack, function_table& functions)
//{
//
//}
//
//_fn& lookup_fn(string id, vector<_arg> args, function_table& functions)
//{
//	try {
//		auto&& overload_set = functions.at(id);
//		for (auto&& fn : overload_set) {
//			if (fn.id == id) {
//				bool success = true;
//				for (int i = 0; i < fn.argument_list.size(); ++i) {
//					if (args[i] != fn.argument_list[i])
//						success = false;
//				}
//				if (success) return fn;
//			}
//		}
//		// if we reach this point in the execution we did not find a function matching the argument list
//		throw _semantic_error(__FILE__, __LINE__, "function definition not found, no functions found with matching arguments: ", id + fn_to_string(id, args));
//	}
//	catch (std::out_of_range) {
//		// if we reach this point in the execution we did not find a function matching the name
//		throw _semantic_error(__FILE__, __LINE__, "function definition not found, no functions found with matching name: ", id + fn_to_string(id, args));
//	}
//}
