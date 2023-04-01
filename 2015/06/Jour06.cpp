#include <iostream>
#include <string>
#include <array>
#include <regex>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <cassert>

constexpr int ASIZE = 1000;

template <typename F>
void run_action(std::array<std::array<char, ASIZE>, ASIZE>& lights,
                int xstart, int ystart, int xend, int yend, F action)
{
	assert(xstart <= xend);
	assert(ystart <= yend);
	for (auto j = ystart; j <= yend; j++)
		for (auto i = xstart; i <= xend; i++)
			lights[j][i] = action(lights[j][i]);
}

template <typename ON, typename OFF, typename TOGGLE>
std::array<std::array<char, ASIZE>, ASIZE>
calc_lights(const char * inname, ON on_action, OFF off_action, TOGGLE tog_action)
{
	const std::regex re("(.*) (\\d+),(\\d+) through (\\d+),(\\d+)");
	std::ifstream infile{inname};
	std::smatch m;
	std::array<std::array<char, ASIZE>, ASIZE> lights{};
	std::string line;

	while (std::getline(infile, line)) {
		if (not std::regex_search(line, m, re))
			throw std::runtime_error("Cannot parse line : " + line);
		auto xstart = std::stol(m[2].str());
		auto ystart = std::stol(m[3].str());
		auto xend = std::stol(m[4].str());
		auto yend = std::stol(m[5].str());
		if (m[1].str() == "turn on") {
			run_action(lights, xstart, ystart, xend, yend, on_action);
		}
		else if (m[1].str() == "turn off") {
			run_action(lights, xstart, ystart, xend, yend, off_action);
		}
		else if (m[1].str() == "toggle") {
			run_action(lights, xstart, ystart, xend, yend, tog_action);
		}
		else
			throw std::runtime_error("Invalid action : " + m[1].str());
	}

	return lights;
}

int main(int argc, char** argv) {
	const char * inname = "input";
	if (argc > 1)
		inname = argv[1];

	auto lights = calc_lights(inname, [](char){ return 1; }, [](char){ return 0; }, [](char v){ return !v; });

	auto lights_on = std::accumulate(lights.begin(), lights.end(), 0,
	                                 [](int ref, const auto& line) {
		                                 return ref + std::count(line.begin(), line.end(), 1);
	                                 });
	std::cout << "Part 1 : " << lights_on << std::endl;

	auto lights_2 = calc_lights(inname,
	                            [](char v){ return v+1; },
	                            [](char v){ return v ? v-1: 0; },
	                            [](char v){ return v+2; });
	auto brightness = std::accumulate(lights_2.begin(), lights_2.end(), 0,
	                                  [](int ref, const auto& line) {
		                                  return ref + std::accumulate(line.begin(), line.end(), 0);
	                                  });
	std::cout << "Part 2 : " << brightness << std::endl;
}
