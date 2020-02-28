//#pragma once
//#include <vector>
//using std::vector;
//#include <stack>
//using std::stack;
//
//#include "ast.h"
//#include "flags.h"
//
///*
//	control flow of the program can be represented as a graph.
//
//	to this end, how does control flow through the program?
//
//	well, in it's most basic form, a program has a root,
//	and control will flow from one statement to the next
//	until the root function returns and the program ends.
//
//	we are adding a few more basic forms of statement which
//	can affect the flow of control.
//	the big four we need are:
//	conditionals,
//	loops,
//	sequences,
//	functions.
//
//	conditionals: if
//	loops: while
//	sequences: implicit with the sequence of expressions in a block of code
//	functions!
//
//	if and while entities perform local jumps
//	sequences are handled implicitly by the program counter
//	function entities perform non-local jumps
//
//	the local static state of a function can be accessed relative to the functions
//	stack frame pointer.
//	the local static state is simply the state described by it's local variables + parameters
//	allocated into the functions stack frame (including local sub-scoped variables)
//	conditionals, loops, and sequences are all represented in the body of their local
//	function. (we can consider global and or module local variables, they complicate things,
//	global is actually more straightforward, as the program already has some conception of global storage
//	but module local names seem more difficult, i can see where the OO ideas come from, instancing
//	each module would make them behave semantically like classes. and would point to a place to put module
//	locals, but classes are not what we want in the base language. as we defer to the f(a) syntax
//	over a.f(), so we want to instance Plain old data types, and make modules more of an encapsulation
//	of behavior.
//	maybe we make module locals act like 'static' variables in c/c++?
//	meaning, they are physically global, but can only be accessed by
//	functions which are in the same scope. (you can hide these variables
//	by not including their names in the modules export list, or make them
//	visible to other modules by exporting them.)
//
//	in order to analyze control flow we first need to divide the program into 
//	basic blocks. which are the smallest unit of computation (semantics) denoted by the
//	syntax. essentially a basic block is as long a sequence as one can make without
//	altering the flow of control.
//
//	our control flow graph, then becomes the graph of connections between basic blocks in the
//	program.
//
//	we have, again, four main basic forms of statement to consider, if, while, function calls, and statements.
//
//	the lowest hanging fruit is the statement, which is encapsulated nicely in the basic block
//	abstraction by definition.
//
//	the next we consider is the if statement, this on the surface can seem easy,
//	it is a branch. but we must consider the possibility of composition of branches.
//	how do if's handle possibly having an arbitrary length chain of if's?
//	well, implicitly, we do not presume what either body of the if will be
//	and allow them to be any statement. the control flow perspective of an if
//	conditional is simply a pair. we either take the then route, or the else route.
//	if either route happens to be an if, or another statement, we deal with it then.
//	this maintains ortoginality between the if statement and other forms of control.
//	if the else clause is elided by the programmer, then the else chain simply points to
//	the next statement in the sequence. if an if statement is used as the return value
//	of a function, the type should be deduced like:
//	if (cond) { then-return-type } else { else-return-type }
//
//	the then-return-type and the else-return-type must be name equivalent
//
//	if the else body is elided we have a choice, we could signal an error
//	or we could infer the type as none. and if going by pure inference
//	we could consider the whole type to be optional<then-return-type>
//
//
//	so we have four types of node in our graph,
//	basic, branch, loop, scope
//
//	scope (functions) are where we start, because programs start with root.
//	the root function will have it's own local stack allocated.
//	the body of a function will always start as a basic block.
//
//	an if has two locations it can jump to.
//
//	a while loops over it's internal basic block, and then continues sequential execution
//
//	a function takes control, and returns it to the caller.
//
//	a basic block always exists inside of a scope.
//	so a scope always starts a new basic block
//
//	we obviously need to signal a functions begin/end
//	as well as it's call/return semantics, so having
//	a function begin and end with some predefined nodes
//	would simplify reasoning about those locations.
//
//	each program will have a graph which starts from root. 
//	each vertex will point to the possible points where control
//	could flow. local jumps could be represented as a local dag.
//	where we track the flow of control from a statement to statement sense.
//	we can also go a level up and consider the program as
//	a whole to be a dag where the nodes represent modules and
//	the edges represent the data dependencies.
//	
//	so it's not one dag, it's three dags.
//	one in a statement sense, one in a function sense,
//	and one in a module sense.
//	we can unify the statement and function dags by encoding
//	the function call graph into the basic flow control.
//
//	a module exposes one or more names, either functions
//	or constants (for now.)
//
//	maybe we could represent macros as a fourth kind
//	of dag, a user definable dag. where we are interested in the structure of the dag,
//	and allow the nodes to be polymorphic and/or traited/attributed 
//	as well as the edges maybe being polymorphic? would that even make sense?
//
//
//	lisp in a sense, is it's own execution flow graph,
//	so these three dags are all able to be talked about
//	in the same language. because the three dags are all
//	represented by the same construct. the list.
//	they are all encoded into the s-expression.
//	in the imperitive sense, we need to define those actions
//	from which composition of these three dags may emerge.
//
//	the execution of each of the statements in sequence makes up the 
//	algorithm. from this perspective we can say that the emergent
//	property of the execution of each of the statements in sequence
//	is the algorithm. a declarative language states this emergent property
//	directly in the syntax.
//	the macro is in a sense, encapsulating some syntactically definable
//	form of this declarative sense and allowing the imperative language to
//	substitute actions in certain locations. in the way a functional language
//	operating on it's own code as data can.
//	it is not textual substitution, it is allowing the user
//	to elide sectons of the AST, and then later bind those
//	holes to a type, which is specified by the macro
//	expansion call.

//	
/* macros could act like some sort of name */
//*/
//
//enum _control_block_type {
//	CBLK_BASIC,
//	CBLK_IF,
//	CBLK_WHILE,
//	CBLK_SCOPE,
//	CBLK_FN,
//};
//
//typedef struct _control_block {
//	_control_block_type type;
//	_flags flags;
//	Ast* ast;
//} _control_block;
//
//enum class CBLK_FLAGS : short {
//	ROOT_BLOCK = 1,
//	END_BLOCK  = 2,
//
//};
//
//bool is_root(_control_block& cblk);
//bool is_end(_control_block& cblk);
//
///*
//	a graph is a set of nodes
//		N = { 1, 2, 3, 4, 5 }
//
//	and a set of edges
//		E = {{1, 2}, {1, 3}, {2, 4}, {2, 5}, {3, 1}, {3, 5}, {4, 5}}
//
//	this can be represented as an adjacency list
//	where each node is represented by a location
//	in a list, (or really any container)
//	and each node contains a list (or really any container)
//	of nodes, each member of the container representing
//	a node that this node has an edge to.
//*/
//
//typedef struct _control_flow_graph {
//	vector<_control_flow_graph*> edges;
//	_control_block block;
//} _control_flow_graph;
//
//void insert_edge(_control_flow_graph* from, _control_flow_graph* to);
//
//_control_flow_graph* create_control_flow_graph(_module& mdl);
//
//void construct_control_block(Ast* stmt, _control_flow_graph* node, _fn* fn, stack<_scope> scope_stack, function_table& functions);
//
//void construct_if_cblk(_control_flow_graph* node, _if* cond, _fn& local_scope, stack<_scope> scope_stack, function_table& functions);
//
///* helper functions */
//_fn& lookup_fn(string id, vector<_arg> args, function_table& functions);
//
