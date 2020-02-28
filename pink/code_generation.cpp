#include <string>
using std::string;
using std::stoi;

#include "code_generation.h"
#include "ast.h"
#include "token.h"

const string& CodeGen::generate_instruction_sequence(Module& m)
{
	string result;

	/*
		a module in this very first version
		is simply a single stack space with
		one entry and exit point with the
		ability to be called by
	*/

	for (auto stmt : m.stmts) {
		result += gen_stmt(stmt);
	}
	
	
	final_sequence = result;
	return final_sequence;
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
	case Token::LITERAL_CHAR: {
		/* case of a statement like:
			print 'a';
			print 'S';
			print '$';

			print when passed a single character
			can call putchar:

			mov rcx, <literal>
			sub rsp, 20h
			call putchar
			add rsp, 20h

		*/
		result += gen_mov("rcx", print->text);
		result += gen_min_fn_call("putchar");
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

string CodeGen::gen_min_fn_decl(string name)
{
	return name + " proc public\n";
}

string CodeGen::gen_min_fn_endp(string name)
{
	return name + " endp\n";
}

string CodeGen::gen_min_fn_prolouge()
{
	return gen_push("rbp") + gen_mov("rbp", "rsp");
}

string CodeGen::gen_min_fn_epilouge()
{
	return gen_pop("rbp") + gen_ret();
}

string CodeGen::gen_min_fn_call(string fname)
{
	string result;
	result += gen_alloc_stack("20h");
	result += gen_call(fname);
	result += gen_dealloc_stack("20h");
	return result;
}

string CodeGen::gen_prepare_first_gpr_arg()
{
	return string();
}
