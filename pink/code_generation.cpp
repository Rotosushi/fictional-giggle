#include <string>
using std::string;

#include "code_generation.h"
#include "ast.h"
#include "token.h"
#include "AsmFile.h"

AsmFile CodeGen::generate_asm_file(Module& m)
{
	/*
		a module in this very first version
		is simply a single stack space with
		one entry and exit point with the
		ability to be called by the operating
		system.
		 
		this requires that all code written
		be placed inside a function named
		main in the output assembly.
		
		however once we add function support
		those functions will be named by
		their defining text.
		
		the one statement that is supported is 
			print "some text"; 
		each print can be mapped onto a syscall
		to write, as long as the buffer holding
		the literal text is allocated properly.
		
		each print statement is represented in
		assembly as an allocation of
		the string literal in the .data section
		and a syscall in the .text section
		
		in nasm on x86-64 Linux that looks like:
		
		section .data
		...
		label db "the-string-literal", 0
		labelLen equ $ - label 
		...
		section .text 
		...
		mov rax, 1
		mov rdi, 1
		lea rsi, [label]
		mov rdx, labelLen
		syscall
		...
		
		where $ is a pseudo-variable which is
		effectively a pointer to the beginning
		of the current line, so subtracting the
		$ from label gives us the length of the
		string. it literally calculates the number
		of bytes between the position of the label
		and the start of the line on which $ appears.
		so this definition only works if the 
		length declaration is the immediate next
		line of the program. because of this dependancy
		it seems reasonable to package the definition
		of the string with it's length every time.
		
		
		
		conceptually we are working on a single file.
		in the context of the larger language, we are
		only considering the root file to begin.
		as defining the root of execution is part
		of making a minimum viable program which
		can be called by the operating system.
		 
		when we consider coalesing multiple input
		files into a single program, My first
		instinct is to call gen_asm_file() on
		each input file, and then use the assembler
		to assemble each output file, and then the linker
		to link them together, effectively pushing
		that problem into programs which have already
		solved it. the end program will eventually
		be in charge of orchestrating the assembling
		and linking steps, however that is not the
		main concern at the moment, we can focus on building
		full language semantics atop the assembler for now.
	*/
	
	/*
	 * our asmFile has three major sections which we care about,
	 * Data, Text, and Bss. Data is where our modual local variables
	 * are stored and defined. the Bss section is zero initialized
	 * on program load, so this is for zero initialized local
	 * variables. Text is where the executable instructions of
	 * the program are stored
	 */
	AsmFile asmFile;
	asmFile.name = "main.s";
	asmFile.data = gen_data_segment(m); 
	asmFile.text = gen_text_segment(m);
	asmFile.bss  = gen_bss_segment(m);
	return asmFile;
}

string CodeGen::gen_data_segment(Module& m)
{
	string result;
	
	result = "segment .data\n";

	
	for (auto stmt : m.stmts) {
		result += gen_data_stmt(stmt);
	}
	
	return result;
}

string CodeGen::gen_data_stmt(Ast* stmt) 
{
	string result;
	
	switch(stmt->type) {
		/* 
		 * we need to declare space in the data section
		 * for the string literal associated with this
		 * print statement.
		 */
		case AstType::PRINT: {
			Print* printStmt = (Print*)stmt;
			StringLiteral* strLit = (StringLiteral*)printStmt->arg;
			result += gen_alloc_string_literal(*strLit);
		}
		default: throw;
	}
	
	return result;
}

string CodeGen::gen_alloc_string_literal(StringLiteral& sl)
{
	string result;
	string nametag = "strLit" + string(sl.which);
	string lengthtag = "strLit" + string(sl.which) + "Len";
	
	result += gen_label(nametag);
	result += "db " + "\'" + sl.text + "\',0\n";
	result += gen_label(lengthtag);
	result += "equ $ - " + nametag + "\n";
	
	return result;
}

string CodeGen::gen_text_segment(Module& m)
{
	string result;
	
	result = "segment .text\n";
	
	result += gen_export_label("main");
	
	result += gen_main(m);
	
	return result;
}

string CodeGen::gen_bss_segment(Module& m)
{
	
}

string CodeGen::gen_main(Module& m)
{
	string result;
	
	result += gen_label("main");
	
	for (auto stmt : m.stmts) {
		result += gen_text_stmt(stmt);
	}
	
	return result;
	
}

string CodeGen::gen_text_stmt(Ast* stmt)
{
	switch (stmt->type) {
	case AstType::PRINT: {
		return gen_print((Print*)stmt);
	}
	default: {
		throw;
	}
	}
}

string CodeGen::gen_print(Print* print)
{
	string result;

	StringLiteral* strLit = (StringLiteral*) print->arg;
	string nametag = "strLit" + string(strLit.which);

	/* case of a statement like:
		print "hello, World!";
	*/
	result += gen_mov("rax", "1");
	result += gen_mov("rdi", "1");
	result += gen_lea("rsi", "[" + nametag + "]\n");
	result += gen_mov("rdx", nametag + "Len");
	result += gen_syscall();
	
	return result;
}

string CodeGen::gen_ret()
{
	return "\tret\n";
}

string CodeGen::gen_syscall()
{
	return "\tsyscall\n";
}

string CodeGen::gen_push(string reg)
{
	return "\tpush " + reg + "\n";
}

string CodeGen::gen_pop(string reg)
{
	return "\tpop " + reg + "\n";
}

string CodeGen::gen_mov(string dest, string source)
{
	return "\tmov " + dest + "," + source + "\n";
}

string CodeGen::gen_lea(string dest, string source)
{
	return "\tlea " + dest + "," + source + "\n";
}

string CodeGen::gen_call(string fname)
{
	return "\tcall " + fname + "\n";
}

string CodeGen::gen_alloc_stack(string size)
{
	return "\tsub rsp," + size + "\n";
}

string CodeGen::gen_dealloc_stack(string size)
{
	return "\tadd rsp," + size + "\n";
}

string CodeGen::gen_export_label(string label)
{
	return "\tglobal " + label + "\n";
}

string CodeGen::gen_label(string label) 
{
	return label + ":\n";
}

string CodeGen::gen_min_fn_prolouge()
{
	return gen_push("rbp") + gen_mov("rbp", "rsp");
}

string CodeGen::gen_min_fn_epilouge()
{
	return gen_pop("rbp") + gen_ret();
}


