
#include <iostream>
#include <fstream>
#include <cstdint>

int main(int argc, char **argv)
{
	const char * inname = "input";
	char c;
	int32_t floor = 0, down_floor = 0;

	if (argc > 1)
		inname = argv[1];

	std::ifstream infile{inname};
	while (infile.get(c)) {
		if (c == '(')
			floor++;
		else if (c == ')')
			floor--;
		if (floor < 0 && down_floor == 0) {
			down_floor = infile.tellg();
		}
	}

	std::cout << "part 1 : " << floor << ", part 2 : " << down_floor << std::endl;
	return 0;
}
