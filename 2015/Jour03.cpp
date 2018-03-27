
#include <iostream>
#include <fstream>
#include <utility>
#include <set>
#include <sstream>
#include <cassert>
#include <array>

int parse_path(std::istream &path)
{
	std::pair<int, int> pos(0, 0);
	std::set<std::pair<int, int>> d;
	char c;
	d.insert(pos);

	while (path.get(c)) {
		switch (c) {
		case '^': std::get<1>(pos) += 1; break;
		case 'v': std::get<1>(pos) -= 1; break;
		case '<': std::get<0>(pos) -= 1; break;
		case '>': std::get<0>(pos) += 1; break;
		}
		d.insert(pos);
	}

	// std::cout << "test : " << d.size() << std::endl;
	return d.size();
}

int parse_path_year2(std::istream &path)
{
	std::pair<int, int> santa(0, 0), robo(0, 0);
	std::pair<int, int> *pos = &santa;
	std::set<std::pair<int, int>> d;
	std::array<char, 2> s;
	d.insert(*pos);

	while (path.get(s.data(), 3)) {
		for (auto c : s) {
			switch (c) {
			case '^': std::get<1>(*pos) += 1; break;
			case 'v': std::get<1>(*pos) -= 1; break;
			case '<': std::get<0>(*pos) -= 1; break;
			case '>': std::get<0>(*pos) += 1; break;
			}
			d.insert(*pos);
			pos = &robo;
		}
		pos = &santa;
	}

	// std::cout << "test : " << d.size() << std::endl;
	return d.size();
}

int main(int argc, char **argv)
{
	const char * inname = "input03";
	std::ifstream infile = std::ifstream();
	std::stringstream ss;

	// ss << "^>";
	// assert(parse_path_year2(ss) == 3);
	// ss.str("");	ss.clear();
	// ss << "^>v<";
	// assert(parse_path_year2(ss) == 3);
	// ss.str("");	ss.clear();
	// ss << "^v^v^v^v^v";
	// assert(parse_path_year2(ss) == 11);

	if (argc > 1)
		inname = argv[1];

	infile.open(inname);
	std::cout << "houses with present : " << parse_path(infile) << std::endl;
	infile.clear();
	infile.seekg(0);
	std::cout << "houses with present on year 2 : " << parse_path_year2(infile) << std::endl;

	return 0;
}
