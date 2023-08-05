
#include <iostream>
#include <fstream>
#include <unordered_set>
#include <string.h>

int main(int argc, char **argv)
{
	const char* input;
	if (argc < 2)
		input = "input";
	else
		input = argv[1];

	std::ifstream in (input);
	if (not in.is_open())
	{
		std::cerr << "Cannot open '" << input << "' : " << strerror(errno) << std::endl;
		return 1;
	}

	int val;
	int freq = 0;
	bool sec = false, pre = false;
	std::unordered_set<int> s;

	while (!(pre && sec)) {
		// force to clear or seekg fail
		in.clear();
		in.seekg(0);
		in >> val;

		while (!in.eof()) {
			freq += val;
			if (!sec && s.find(freq) != s.end()) {
				sec = true;
				std::cout << "reaches twice : " << freq << std::endl;
			}
			s.insert(freq);
			in >> val;
		}

		if (!pre) {
			pre = true;
			std::cout << "frequence : " << freq << std::endl;
		}
	}

	return 0;
}
