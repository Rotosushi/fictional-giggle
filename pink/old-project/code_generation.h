#pragma once

#include <string>
using std::string;

#include "ast.h"
#include "AsmFile.h"

class CodeGen {
public:
	AsmFile generate_asm_file(Module& m);

	CodeGen() {}
private:
	string final_sequence;
	
	string gen_data_segment(Module& m);
	string gen_text_segment(Module& m);
	string gen_bss_segment(Module& m);

	string gen_data_stmt(Ast* stmt);
	
	string gen_alloc_string_literal(StringLiteral& sl);
	string gen_string_nametag(StringLiteral& sl);
	string gen_string_lengthtag(StringLiteral& sl);

	string gen_text_stmt(Ast* stmt);

	string gen_main(Module& m);
	string gen_print(Print* print);
	string gen_export_label(string label);
	
	string gen_label(string label);

	string gen_ret();
	string gen_push(string reg);
	string gen_pop(string reg);
	string gen_mov(string dest, string source);
	string gen_lea(string dest, string source);
	string gen_call(string fname);
	string gen_syscall();
	
	string gen_alloc_stack(string size);
	string gen_dealloc_stack(string size);

	// function entry and exit
	string gen_min_fn_prolouge();
	string gen_min_fn_epilouge();
	string gen_max_fn_prolouge();
	string gen_max_fn_epilouge();

	// function call


	// string generate_prepare_stack_args(...);
	// string generate_prepare_first_xmm_arg(...);
	// string generate_prepare_first_ymm_arg(...);
};








































//
///*
//	the main tasks of code generation are:
//
//	1) resource allocation
//		determine the resources that will be required and used
//		during execution of instruction sequences, this includes
//		register allocation, static memory allocation, and eventually
//		dynamic memory allocation as well.
//
//	2) execution order determination
//		specify the sequence in which descendants of a node will be evaluated
//
//	3) code selection
//		select the final instruction sequence.
//
//	in order to optimize the code that is generated (under some cost criteria)
//	these tasks will be intertwined and iterated. 
//
//	this problem is NP-complete lol
//
//
//*/
//#include <string>
//using std::string;
//#include <stack>
//using std::stack;
//#include "ast.h"
//#include "type.h"
//
//
//
//class _code_generator {
//
//	/*
//		task zero: how big is everything?
//
//		we need to know how big our constituent parts are when we generate
//		code. the size of a variable is dependant on the size of it's type.
//		once we know the sizes of our constituent parts, the sizes of the 
//		aggregate wholes can be calculated by summing their parts.
//
//		the size of some function is the size of it's parameters + the size of
//		it's declarations (the decls that cannot fit in registers at least)
//		this is the size that needs to be allocated onto the stack when we
//		want to call a function.
//
//		the static size of a program is the size of the constant data
//		plus the size of the executable instructions.
//
//		this is itself composed of the static size of each of the modules
//		in a program. programmers are likely to include the same module in
//		many of their other modules, a very simple optimization would be
//		to give modules some form of refrence to other modules through the 
//		import mechanism, so each module becomes it's own static location
//		in the program, instead of repeating the definition everywhere.
//		
//		notice this has nothing to do with namespace management, once we have
//		gotten to this point in the compilation process, we assume that every
//		name refers to a type which it is valid to be. and every function call
//		refers to a function which exists and can be called. what the main concern
//		is now, is how the user program will be described in the underlying assembly.
//
//		these are the assumptions placed around the static definition of the program.
//
//		(maybe the dynamic definition has more flexibility in this regard, but
//		 again, this is v1)
//
//		(the dynamic size of a program is the static size, plus the sizes of the
//			stack and the heap during every point of execution. consequently, this
//			number is hard to know, even operating systems siplify this to the total available
//			space that individual programs are allocated, and not what they are currently using)
//
//		the task is one of building the size of composite
//		objects up from the sizes of it's base components; all the way down
//		to the already known primitive types, then back up the composite
//		structure and aggregating the types size.
//
//		in our case we will primarily be dealing with algebraic data types,
//		functions, modules, and the overall program.
//		so, sums and alternatives. the size of any data type is either, the
//		sum of the sizes of it's component types, or the size of the
//		largest of it's alternative types.
//		(the size of a function is unique to each function, including
//		 each individual polymorphic instance, despite the functions
//		 having the 'same' type)
//		the other consideration is the
//		size of a function (stack frame), which is built up from the size of its
//		arguments, return values, local variables, temporaries, and bookkeeping
//		information. temporaries refers to the unnamed temporaries used in
//		complex calculations. Bookkeeping usually includes the functions
//		return address, a refrence to the stack frame of the caller (sometimes
//		called a functions dynamic link) the saved values of registers needed
//		by both the caller and the callee, and more.
//		(more is generally implementation
//		dependant, and varies with the hardware or the compiler or the language)
//
//		fields within these composite objects, called it's components,
//		are characterized by their size, alignment, and relative position
//		within the composite object. these objects are described using algebraic
//		data types in pink. 
//
//		for simplicity of implementation of v1, we will not be considering
//		the process of packing small components into the same word to conserve
//		on the size of the final object. this will be considered an optimization
//		and the process with be tackled later. for now, we will say that the
//		smallest unit of storage that the compiler knows about is a single machine
//		word. smaller types will be simulated by a full word. so I hope the alignment
//		considerations will be lessend on implementation.
//
//		Objects are characterized during this aggregation process by
//		their size and relative address within the containing object.
//		The base address is determined at run time and the sequence of
//		relative addresses of aggregates in	which an object is contained
//		yields the effective address of that object.
//
//		that is to say, a functions data exists in some stack frame
//		relative to the bottom of the stack, pointed to by the frame pointer.
//		and relative to the declarations placement in the stack frame of the
//		function.
//		a functions local names can be accessed relative to the frame pointer
//		which simplifies the implementation dramatically. each function call
//		can modify the frame pointer to maintain it accross function calls.
//
//		the size of a module is the sum of the sizes of the instructions that
//		make up the modules component functions, plus the sum of the sizes
//		of the module local declarations.
//
//		the size of a function in the stack frame is the size of the data structures
//		that make up it's static members.
//
//		the size of a declaration is the size of it's type
//
//		the size of a type is the known size of the primitive type
//		or it is the sum of it's component types, or the size of the
//		largest of it's component types.
//
//
//
//
//
//		a program is logically separated into modules, but physically the program
//		is partitioned into it's static data, it's instructions, and it's dynamic
//		memory. the dynamic memory is further separated into two structures
//		the stack and the heap. the stack is used to support functions, and gives a location
//		for function local variables. the heap is used to provide support for dynamic semantics.
//		to deal with data that cannot be known statically at compile time.
//
//		module locals should be restricted to data that can be stored statically.
//		then module locals and globals have a clear storage location, and it makes
//		all of them static, so only serves to convey static information, and we
//		do not have to worry about globally modifiable state.
//
//		all state is modified through behaviors which act on local variables.
//		and only local variables are able to be modified.
//
//		if we want to effect change on the outside world that is always done
//		through modifying a local name, or calling a function.
//	*/
//	int _sizeof(_module& mdl, stack<_scope> scope_stack);
//	int _sizeof(_fn& fn, stack<_scope> scope_stack);
//	int _sizeof(_vardecl& decl, stack<_scope> scope_stack);
//	int _sizeof(_type& type, stack<_scope> scope_stack);
//	
//		
//
//		
//	/*
//		main task one: memory mapping
//
//		overarching goal: determine the static memory requirements of the program.
//
//		so essentially, this first step is to generate an allocation strategy
//		for each instance of a given construct in the language. we need to know how
//		to allocate and access each primitive and composite type in the language.
//		with the composite types being built up from primitive objects.
//
//		(things that usually complicate this process but are being ignored because
//		it's v1. primitive packing in adts, polymorphic type construction,
//		multiple source files. dynamic resolution of types and sizes at load time.
//		multiple target hardwares.)
//
//		aside:
//		functional languages have the ability to be lazily evaluated, which means
//		arguments can be passed unexecuted, but wrapped in an object that can be
//		executed later, and the full expression is built up over the course of function calls,
//		the full expression is never evaluated until the program
//		requests to use the expression as a source of data; and even then the expression
//		is only evaluated enough to fulfil the requested data, and then stops. this
//		allows infinite sets to be represented by finite memory. which makes it a very 
//		attractive feature. however this sort of thing can only be represented
//		using fully dynamic semantics, so is only really a candidate for a library of
//		dynamic execution structures or something.)
//
//		related thought:
//		so what if we consider lambdas to be a local function with a dynamic scope, we
//		still jump into a separate area of code to preform some execution, but the
//		jump is local, the storage is also contained within the callers stack frame.
//		so it's almost like an if conditonal, where we jump to the else case
//		if the cond is false, except the jump also simulates a function call.
//		(probably minus saving and restoring registers because it is essentially
//		an inline function.) a lambda that gets passed around as an argument or a return
//		value could have it's execution context exist in the argument or
//		return locations of the callee/caller depending on the direction.
//		(am i exectuing in the callee's stack or the caller's stack?,
//		 in the case of being a parameter, or a return value respectively)
//		 (maybe?? this might play weird with return chaining? i guess there are
//		 only two situations if we define it recursively, the case where we return
//		 a lambda, generally means the calling context wants to execute that lambda.
//		 but if the calling context itself returns the lambda, then
//		 the lambda needs to exist independantly. we could consider it to be
//		 name hiding of a module level function. placing the definition in the
//		 modules static definition. then "passing a lambda as an argument" is
//		 merely simulated via function pointers.
//
//		 we can support static arrays through static methods, we could support
//		 locally scoped dynamic arrays with an alloca like mechanism.
//		 if you allocate using [] syntax. you can only support resizable arrays through
//		 heap allocation strategies.
//		 if you allocate using the dynamic memory interface, you would get a pointer
//		 to memory on the heap, and use pass-by-value semantics to pass the refrence
//		 either as the return value, or written into some composite structure.
//		 we could consider it a compiler error to return a pointer to local symbols,
//		 as the refrence becomes invalid as soon as we return.
//		 it is perfectly fine to pass a pointer to local variables into another function,
//		 as the lifetime of the callee is always equal to or shorter than the lifetime
//		 of the caller.
//
//		 related thought: pointers, the programmer should be barred from returning
//		 a pointer to a locally scoped name. you can pass any pointer as a parameter
//		 or you can return a pointer to some memory allocated onto the heap.
//		 but returning a refrence to a name whose lifetime ends after the execution of the
//		 return statement is always a semantic error. maybe the counter-argument is, "hey
//		 you cannot determine that through static analysis of the source text"
//		 then I guess this implies there is some way for
//		 the compiler to determine that a name refers to memory allocated on the heap, 
//		 or on the stack. I feel like since the compiler has to arrange for the
//		 allocations to happen in the first place, this means there is enough information
//		 to deduce that.
//
//		 this sort of feature would also strengthen a form of alloca being in the
//		 native lanugage. the bugs surrounding alloca are either a) memory overuse,
//		 or b) returning a pointer to dead memory.
//		 a) is something that programmers need to be concerned with in the
//		 design of their algortihms anyways, in this regard alloca is no worse 
//		 than a massive static array or a improperly bounded recursive function.
//		 b) is solved if we make returning a pointer to a variable which
//		 is allocated on the current stack frame illegal.
//		 it might be that we support dynamic local storage via some alloca like
//		 mechanism. in c terms something like
//
//		 void myfunc(int size) {
//			int local_dynamic_array[size];
//		}
//
//		this sort of thing also gives more weight to some mechanism which could
//		internally detect stack smashing through the heap, or heap smashing through
//		the stack, which could be captured as a runtime error.
//		
//		if we dictate this as the language with which systems programs are written in the lowest level.
//		we can aggregate the semantics of programs written in the systems language and provide
//		interfaces to them through the means of apis, higher level languages, or something else i can't think of here.
//
//		this language is meant to solve the problem of, "we don't have the computing resources to
//		be able to provide an extremely rich semantic set of language primitives, but we still want
//		good tools to write quality code"
//
//		communication over wires to hardware components is a task that can be abstracted.
//		the set of hardware resources that a computer has is a data set that should be accessable.
//	*/
//};
