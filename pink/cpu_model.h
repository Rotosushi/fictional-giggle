

// CPU model
// these are the general purpose registers which
// the compiler needs to know about in order to
// talk about ints, chars, strings, pointers,
// booleans, functions, conditionals, and loops.
enum class REG {
	rax,
	rbx,
	rcx,
	rdx,
	rsi,
	rdi,
	rbp,
	rsp,
	r8,
	r9,
	r10,
	r11,
	r12,
	r13,
	r14,
	r15,
};

// what are we concered about when we are writing some function?

/*
 * 3 kinds of things: {p} q {r}
 * 
 * that is, preconditions
 * computations
 * and postconditions
 * 
 * functions operate on variables and constants.
 * variables have a storage location in the local stack space
 * or in the local module.
 * constants can be ecoded directly into instructions.
 * 
 * a function has three major sections,
 * it's prolouge, it's body, and it's epilouge
 * the prolouge establishes certain constraints
 * the computation happens
 * the epilouge again establishes certain constraints.
 * 
 * each computation can be treated as a function,
 * where each of the functions arguments need to
 * be placed where the function expects it, then
 * when the function returns, the state of the 
 * local scope has been changed in a known way
 * and we need to establish some postconditions.
 * storing the return values.
 * 
 * 
 * then each line of the body of the source text
 * can be encoded into a series of instructions
 * which carry out the line, and each line can be
 * placed one after the other into some buffer,
 * and we can compose a module by appending each
 * function definition onto the same buffer,
 * and outputing that to be assembled and linked.
 */
 /*
  * our most common instructions:
  * 
  * mov <dest>, <src> // move src into dest
  * 
  * push <src> // pushes src onto the stack at rsp, increments rsp
  * 			// by a word
  * 
  * pop <dest> // pops the top word off the stack and into dest
  * 
  * call <func> // push the address of some function into the 
  * 			// instruction pointer, and onto the stack (i think)
  * 
  * ret // pops the previous instruction pointer off of the stack
  * 	// and into the instruction pointer, to start execution
  * 	// at the call location
  * 
  * add <dest>, <src> // dest += src
  * 
  * mul <src> // rax *= src, high bits go into rdx
  * 
  * div <src> // rax /= src, / -> rax, % -> rdx
  * 	!! rdx must be zero on execution of div or you get SIGFPE !!
  * 
  * shr <dest>, cl // cl is the lowest byte or rcx, optionally
  * 				// an 8bit immediate can be specified
  * 
  * jmp <label> // unconditionally sets the <label> as
  * 			// the next instruction to execute.
  * 
  * cmp a, b // compares two values, sets the flags such that a 
  * 		 // conditional jump can be called immediately after.
  * 
  * jl, jle, je, jg, jge, jne 
  * 	// jump in previous cmp flags set in a known pattern
  * 
























