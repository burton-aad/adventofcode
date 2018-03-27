
#include <iostream>
#include <fstream>
#include <cstdint>
#include <sstream>
#include <array>
#include <iomanip>

// md5 from openssl
#include <openssl/md5.h>

int main(int argc, char **argv)
{
	const char * input4 = "iwrupvqb";
	std::array<unsigned char, MD5_DIGEST_LENGTH> h, h5, h6;
	uint64_t i = 0, five = 0, six = 0;
	std::stringstream ss;

	if (argc > 1)
		input4 = argv[1];

	while (five == 0 or six == 0) {
		ss.str("");
		ss << input4 << i;
		// std::cout << ss.str().c_str() << " " << ss.tellp() << std::endl;

		MD5((unsigned char *)ss.str().c_str(), ss.tellp(), h.data());
		if (h[0] == 0 && h[1] == 0) {
			if (six == 0 and h[2] == 0) {
				six = i;
				h6 = h;
			}
			if (five == 0 and (h[2] & 0xF0) == 0) {
				five = i;
				h5 = h;
			}
		}

		i++;
	}

	std::cout << "number for 5 zeros : " << five << " - ";
	for (auto c : h5)
		std::cout << std::setw(2) << std::setfill('0') << std::hex << int(c);
	std::cout << std::endl;

	std::cout << "number for 6 zeros : " << std::dec << six << " - ";
	for (auto c : h)
		std::cout << std::setw(2) << std::setfill('0') << std::hex << int(c);
	std::cout << std::endl;

	return 0;
}
