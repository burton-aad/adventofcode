
#include <iostream>
#include <functional>
#include <optional>
#include <map>
#include <fstream>
#include <sstream>
#include <cctype>
#include <string>


struct Wire {
	std::string line;
	std::function<uint16_t(std::map<std::string, Wire>&)> func;
	std::optional<uint16_t> value{};

	uint16_t calc_val(std::map<std::string, Wire>& circuit) {
		if (!value) {
			// std::cout << "calc " << line << std::endl;
			value = func(circuit);
		}
		return value.value();
	}
};

using Circuit = std::map<std::string, Wire>;

std::vector<std::string> split(const std::string& s, char delim)
{
	std::vector<std::string> result;
    std::stringstream ss (s);
    std::string item;

    while (getline (ss, item, delim)) {
	    if (not item.empty())
		    result.push_back (item);
    }

    return result;
}

void reset(Circuit& c) {
	for (auto& p: c)
		p.second.value.reset();
}

int main(int argc, char** argv)
{
	const char * inname = "input";
	if (argc > 1)
		inname = argv[1];

	Circuit circuit{};

	std::ifstream infile{inname};
	std::string line;
	while (std::getline(infile, line)) {
		auto v = split(line, ' ');

		if (v[1] == "->") {
			if (isdigit(v[0][0]))
				circuit[v[2]] = Wire{line, [n=v[0]](Circuit&){ return stoi(n); }};
			else
				circuit[v[2]] = Wire{line, [n=v[0]](Circuit& c){ return c[n].calc_val(c); }};
		}
		else if (v[0] == "NOT")
			circuit[v[3]] = Wire{line, [n=v[1]](Circuit& c){ return ~(c[n].calc_val(c)); }, std::nullopt};
		else if (v[1] == "AND") {
			auto a=v[0], b=v[2];
			if (isdigit(a[0]))
				circuit[v[4]] = Wire{line, [a=stoi(a),b](Circuit& c){ return a & c[b].calc_val(c); }};
			else
				circuit[v[4]] = Wire{line, [a,b](Circuit& c){ return c[a].calc_val(c) & c[b].calc_val(c); }};
		}
		else if (v[1] == "OR")
			circuit[v[4]] = Wire{line, [a=v[0], b=v[2]](Circuit& c){ return c[a].calc_val(c) | c[b].calc_val(c); }};
		else if (v[1] == "RSHIFT")
			circuit[v[4]] = Wire{line, [a=v[0], b=std::stoi(v[2])](Circuit& c){ return c[a].calc_val(c) >> b; }};
		else if (v[1] == "LSHIFT")
			circuit[v[4]] = Wire{line, [a=v[0], b=std::stoi(v[2])](Circuit& c){ return c[a].calc_val(c) << b; }};
	}

	auto a = circuit["a"].calc_val(circuit);
	std::cout << "Part 1: " << a << std::endl;

	reset(circuit);
	circuit["b"].value = a;
	std::cout << "Part 2: " << circuit["a"].calc_val(circuit) << std::endl;
}
