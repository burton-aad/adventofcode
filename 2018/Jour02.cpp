
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <vector>

template<class T, class U>
bool find_val(const std::unordered_map<T, U>& m, const U val)
{
	return std::find_if(m.begin(), m.end(),
	                    [&val] (const std::pair<T,U>& p) { return p.second == val; }) != m.end();
}

std::string diff_s(std::string& s, std::string& t)
{
	std::string r;
	auto t_it = t.begin();
	std::copy_if(s.begin(), s.end(), std::back_inserter(r),
	             [&t_it](char c){ return c == *t_it++; });
	return r;
}

void jour2(std::ifstream& in)
{
	std::string s;
	std::vector<std::string> v;
	int dbl = 0, tpl = 0;

	in >> s;
	while (!in.eof()) {
		std::unordered_map<char, int> m;
		for (auto c : s)
			m[c]++;
		dbl += find_val(m, 2);
		tpl += find_val(m, 3);

		for (auto vs : v) {
			auto d = diff_s(vs, s);
			if (d.size() == s.size()-1)
				std::cout << "correct boxes : " << vs << ", " << s << " : " << d << std::endl;
		}
		v.push_back(std::move(s));

		in >> s;
	}

	std::cout << "checksum : " << dbl*tpl << std::endl;
}


int main(int argc, char **argv)
{
	const char* input;
	if (argc < 2)
		input = "input02";
	else
		input = argv[1];

	std::ifstream in (input);
	jour2(in);

	return 0;
}
