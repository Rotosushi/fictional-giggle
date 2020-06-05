#pragma once
#include <Windows.h>

/* basic program i/o on linux */
extern "C" void WriteStr(const char* source, int length);
extern "C" void ReadStr(char* dest, int length);
