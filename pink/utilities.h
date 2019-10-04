#pragma once

extern char* optarg;

char* getopt(int argc, char** argv, char* options);
char* substr(char* str, char* query);