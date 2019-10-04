#include "utilities.h"
#include <cstdlib>

char* optarg = nullptr;
int optind = 1;

char* getopt(int argc, char** argv, char* options)
{
	int i = 0;
	char* opt;
	const char* p;

	if (optind >= argc || argv[optind][0] == '\0' || options == nullptr) return nullptr;

	while (argv[optind][i] == '-') i++; // -opt --opt
	
	opt = &(argv[optind][i]);
	p = substr(options, opt);

	if (p == nullptr) return nullptr; // passed option isn't one we are looking for

	// all options must have arguments
	// TODO: optional option arguments

	optind++;
	if (optind >= argc) return nullptr;

	optarg = argv[optind];
	optind++;
	return opt;
}

char* substr(char* str, char* query)
{
	int i = 0, j = 0;
	char* _substr = nullptr;

	if (str == nullptr || query == nullptr) return _substr;

	while (1) {
		if (str[i] == query[j]) { // search for a match one char at a time
			i++; j++;
		}
		else {
			i++; j = 0;
		}

		if (query[j] == '\0') { // if we have reached the end of our query, str contains query
			_substr = (char*)malloc(j);
			if (_substr != nullptr) {
				for (int n = 0; n < j; n++)
					_substr[n] = str[(i - j) + n];
				_substr[j] = '\0';
			}
			break;
		} 

		if (str[i] == '\0') break; // if we have reached the end of str but not query, str does not contain query
	}
	return _substr;
}