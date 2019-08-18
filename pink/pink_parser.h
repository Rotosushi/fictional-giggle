#pragma once

enum Tok;


// debugging primitives:
void print_token_buffer();

// (token buffer access aka backtracking) primitives:
int  mark();
void release();
void seek(int);

// parser primitives:
bool speculate(Tok tok);
void consume();
void sync(int);
bool speculating();

// the main parse function
bool build_module();


