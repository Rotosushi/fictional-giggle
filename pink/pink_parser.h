#pragma once

enum Tok;

// debugging primitives:
void print_token_buffer();

// (token buffer access aka backtracking) primitives:
int  mark();
void release();
void seek(int);

// parser primitives:
bool match(Tok tok);
void consume();
void sync(int);
bool speculating();

// the main parse function
bool match_top_level();

// used by: 'match_top_level()'
bool speculate_context_declaration();
bool speculate_type_definition();
bool speculate_declaration();

bool match_context_declaration();
bool match_type_definition();
bool match_declaration();

bool match_alias();
bool match_struct();
bool match_union();
bool match_enum();
bool match_function();

bool match_type_specifier();
bool match_identifier();
bool match_lambda_definition();
bool match_assignment_operator();

bool match_type_primitive();
bool match_literal();

bool match_statement();
bool match_compiler_directive();

bool match_constant_expression();





