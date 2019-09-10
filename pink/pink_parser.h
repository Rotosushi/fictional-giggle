#pragma once

enum Tok;
// debugging primitives:
void print_token_buffer();

// (token buffer access aka backtracking) primitives:
int  mark();
void release();

// parser primitives:
bool speculate(Tok tok);
void consume();
void sync(int);
bool speculating();

// the main parse function
bool parse_module();


