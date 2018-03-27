
#include <iostream>
#include <fstream>
#include <cstdint>

using namespace std;

int main(int argc, char **argv)
{
	const char * inname = "input01";
	ifstream infile = ifstream();
	char c;
	int32_t floor = 0, down_floor = 0;

	if (argc > 1)
		inname = argv[1];

	infile.open(inname);

	while (infile.get(c)) {
		if (c == '(')
			floor++;
		else if (c == ')')
			floor--;
		if (floor < 0 && down_floor == 0) {
			down_floor = infile.tellg();
		}
	}

	cout << "part 1 : " << floor << ", part 2 : " << down_floor << endl;
	return 0;
}
