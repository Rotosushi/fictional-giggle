#pragma once

/*
	the main tasks of code generation are:

	1) resource allocation
		determine the resources that will be required and used
		during execution of instruction sequences, this includes
		register allocation, and static memory allocation.

	2) execution order determination
		specify the sequence in which descendants of a node will be evaluated

	3) code selection
		select the final instruction sequence.

	in order to optimize the code that is generated (under some cost criteria)
	these tasks will be intertwined and iterated. 

	this problem is NP-complete lol


*/



class _code_generator {

	/*
		main task one: memory mapping

		determine the size of every type in the program.
		then determine the static memory requirements of the program.

		the task of memory mapping is one of building the size of composite
		objects up from the sizes of it's base components; all the way down
		to the already known primitive types, then back up the composite
		structure and aggregating the types size.

		in our case we will primarily be dealing with algebraic data types.
		so, sums and alternatives. the size of any type is either, the
		sum of the sizes of it's component types, or the size of the
		largest of it's alternative types. the other consideration is the
		size of a functions stack frame, whcih is built up from the size of its
		arguments, return values, local variables, temporaries, and bookkeeping
		information. temporaries refers to the unnamed temporaries used in 
		complex calculations. Bookkeeping usually includes the functions
		return address, a refrence to the stack frame of the caller (sometimes
		called a functions dynamic link) the saved values of registers needed 
		by both the caller and the callee, and more. the rest is generally implementation
		dependant, and varies with the hardware or the compiler or the language.

		fields within these composite objects, called it's components,
		are characterized by their size, alignment, and relative position
		within the composite object.

		for simplicity of implementation of v1, we will not be considering
		the process of packing small components into the same word to conserve
		on the size of the final object. this will be considered an optimization
		and the process with be tackled later. for now, we will say that the
		smallest unit of storage that the compiler knows about is a single machine
		word. smaller types will be simulated by a full word. so I hope the alignment
		considerations will be lessend on implementation. 

		Objects are characterized during this aggregation process by
		their size and relative address within the containing object.
		The sum of the base address determined at run time and the sequence of
		relative addresses of aggregates in	which an object is contained
		yields the eective address of that object.



		so essentially, this first step is to generate an allocation strategy
		for each instance of a given construct in the language. we need to know how
		to allocate and access each primitive and composite type in the language.
		with the composite types being built up from primitive objects.

		(things that usually complicate this process but are being ignored because
		it's v1. primitive packing in adts, polymorphic type construction,
		multiple source files. dynamic resolution of types and sizes at load time.
		multiple target hardwares.)


		so this process in pink will start with generation of an allocation strategy
		for every algebraic data type described by the source code.

		and from there we will generate an allocation strategy for each function
		described by the source code.

		how does a program support calling functions to potentially arbitrary depth?

		well, the strategy used is to refrence variables which are considered local
		to a particular function relative to what is known as the frame pointer.
		where the frame pointer points to the start of said executing functions
		stack frame. when a function wants to call another function (even itself)
		all it needs to do is save it's own frame pointer (so it can be restored when
		the callee returns) and set the frame pointers value to the end of the callers
		stack frame, arrange for the callee to be allocated starting from that location, and then
		preform a jump into the code of the callee. (and to enforce something called
		strict execution; before calling a function any expression that occurs
		in a parameter must be fully evaluated before we can execute the callee)
		
		aside:
		functional languages have the ability to be lazily evaluated, which means
		arguments can be passed unexecuted, but wrapped in an object that can be
		executed later, and the full expression is built up over the course of function calls,
		the full expression is never evaluated until the program
		requests to use the expression as a source of data; and even then the expression
		is only evaluated enough to fulfil the requested data, and then stops. this
		allows infinite sets to be represented by finite memory.)

		related thought:
		so what if we consider lambdas to be a local function with a dynamic scope, we
		still jump into a separate area of code to preform some execution, but the
		jump is local, the storage is also contained within the callers stack frame.
		so it's almost like an if conditonal, where we jump to the else case
		if the cond is false, except the jump also simulates a function call.
		(probably minus saving and restoring registers because it is essentially
		an inline function.) a lambda that gets passed around as an argument or a return
		value could have it's execution context exist in the argument or
		return locations of the callee/caller depending on the direction.
		(am i exectuing in the callee's stack or the caller's stack?,
		 in the case of being a parameter, or a return value respectively)

		 we can support static arrays through static methods, we could support
		 locally scoped dynamic arrays with an alloca like mechanism.
		 if you allocate using [] syntax.
		 if you allocate using the dynamic memory interface, you would get a pointer
		 to memory on the heap, and use pass-by-value semantics to pass the refrence
		 either as the return value, or written into some composite structure.
		 we could consider it a compiler error to return a pointer to local symbols,
		 as the refrence becomes invalid as soon as we return.
		 it is perfectly fine to pass a pointer to local variables into another function,
		 as the lifetime of the callee is always equal to or shorter than the lifetime
		 of the caller.
	*/
};