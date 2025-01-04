

#include <iostream>
#include <fstream>
#include <cstdio>
#include <cassert>
#include <regex>

bool is_nice(const std::string str)
{
	bool dletter = false;
	int voyel = 0;
	char last = ' ';

	for (char c: str) {
		switch (c) {
		case 'a':
		case 'e':
		case 'i':
		case 'o':
		case 'u':
			voyel++;
			break;
		case 'b':
		case 'd':
		case 'q':
		case 'y':
			if (last == c-1)
				// naughty
				return false;
		}
		if (last == c)
			dletter = true;
		last = c;
	}

	return voyel >= 3 and dletter;
}

bool is_nice2(const std::string str)
{
	std::regex r1 ("(.)(.).*\\1\\2");
	std::regex r2 ("(.).\\1");

	return std::regex_search(str,r1) and std::regex_search(str, r2);
}

int main(int argc, char **argv)
{
	const char * inname = "input";
	std::string line;
	int count1 = 0, count2 = 0;

	assert(is_nice("ugknbfddgicrmopn"));
	assert(is_nice("aaa"));
	assert(!is_nice("jchzalrnumimnmhp"));
	assert(!is_nice("haegwjzuvuyypxyu"));
	assert(!is_nice("dvszwmarrgswjxmb"));

	assert(is_nice2("qjhvhtzxzqqjkmpb"));
	assert(is_nice2("xxyxx"));
	assert(!is_nice2("uurcxstgmygtbstg"));
	assert(!is_nice2("ieodomkazucvgmuy"));
	

	if (argc > 1)
		inname = argv[1];

	std::ifstream infile{inname};

	while (std::getline(infile, line)) {
		count1 += is_nice(line);
		count2 += is_nice2(line);
	}

	std::cout << "Part 1 : " << count1 << std::endl;
	std::cout << "Part 2 : " << count2 << std::endl;

	return 0;
}
