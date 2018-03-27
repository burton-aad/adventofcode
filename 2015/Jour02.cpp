
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdint>
#include <cstdio>
#include <vector>
#include <algorithm>

int main(int argc, char **argv)
{
	const char * inname = "input02";
	std::ifstream infile = std::ifstream();
	std::string line;
	uint64_t paper = 0, ribbon = 0;

	if (argc > 1)
		inname = argv[1];

	infile.open(inname);

	while (std::getline(infile, line)) {
		int l, w, h;
		sscanf(line.c_str(), "%dx%dx%d", &l, &w, &h);

		std::vector<int> vec {l, w, h};
		std::sort(vec.begin(), vec.end());

		paper += 2*l*w + 2*w*h + 2*l*h + vec[0]*vec[1];
		ribbon += 2*vec[0] + 2*vec[1] + l*w*h;
	}

	std::cout << "paper : " << paper << ", ribbon " << ribbon << std::endl;
	return 0;
}
