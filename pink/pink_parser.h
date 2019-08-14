#pragma once

// helper functions

void print_token_buffer();

// the main parse function
bool match_top_level();

// used by: everything
bool match_token(token, token);

// used by: 'match_top_level()'
bool speculate_context_declaration();
bool speculate_type_definition();
bool speculate_declaration();

void match_context_declaration();
void match_type_definition();
void match_declaration();

// used by: 'speculate_type_definition()'
bool speculate_alias();
bool speculate_struct();
bool speculate_union();
bool speculate_enum();
bool speculate_function();

void match_alias();
void match_struct();
void match_union();
void match_enum();
void match_function();

// used by: 'speculate_declaration()'
bool speculate_assignment_statement();
void match_assignment_statement();

void speculate_statement();
bool match_statement();

void speculate_compiler_directive();
bool match_compiler_directive();





