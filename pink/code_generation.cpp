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
		string.
		
	*/
	AsmFile asmFile;
	asmFile.name = "main.s";
	asmFile.data = gen_data(m);
	asmFile.text = gen_test(m);
	asmFile.bss  = gen_bss(m);
	return asmFile;
}

string CodeGen::gen_data(Module& m)
{
	string result;
	
	
	return result;
}

string CodeGen::gen_text(Module& m)
{
	return gen_main(m);
}

string CodeGen::gen_bss(Module& m)
{
	
}

string CodeGen::gen_main(Module& m)
{
	string result;
	
	for (auto stmt : m.stmts) {
		result += gen_stmt(stmt);
	}
	
	return result;
	
}

string CodeGen::gen_stmt(Ast* stmt)
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
	switch (print->token) {
	case Token::LITERAL_STRING: {
		/* case of a statement like:
			print "hello, World!";
		*/
		result += gen_mov("rax", "1");
		result += gen_mov(
	}
	default: {
		throw;
	}
	}
	return result;
}

string CodeGen::gen_ret()
{
	return "ret";
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
	return "\tglobal " + label;
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


