#include <string>
using std::string;

// data structure which represents a single assembly file.

typedef struct Section {
	string contents;
} Section;

typedef struct Data : public Section {
} Data;

typedef struct Bss : public Section {
} Bss;

typedef struct Text : public Section {
} Text;

typedef struct AsmFile {
	string name;
	string data;
	string bss;
	string text;
} AsmFile;
