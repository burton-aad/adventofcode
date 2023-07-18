
#include <iostream>
#include <fstream>
#include <cstdint>
#include <sstream>
#include <array>
#include <iomanip>
#include <stdexcept>

// md5 from openssl
#include <openssl/evp.h>

#if OPENSSL_API_COMPAT < 0x30000000L
#define EVP_MD_fetch(x, h, y) (EVP_MD*)EVP_get_digestbyname(h)
#define EVP_MD_free(_hash)
#endif

class MD5 {
private:
	 EVP_MD_CTX *_ctx = nullptr;
	 EVP_MD *_hash = nullptr;
public:

	MD5() {
		_ctx = EVP_MD_CTX_new();
		_hash = EVP_MD_fetch(NULL, "MD5", NULL);
	}

	~MD5() {
		EVP_MD_free(_hash);
		EVP_MD_CTX_free(_ctx);
	}

	void h(const std::string& s, unsigned char *md) {
		EVP_MD_CTX_reset(_ctx);
		if (!EVP_DigestInit_ex(_ctx, _hash, NULL))
			throw std::runtime_error("Cannot init digest");
		if (!EVP_DigestUpdate(_ctx, s.c_str(), s.size()))
			throw std::runtime_error("Cannot update digest");
		if (!EVP_DigestFinal_ex(_ctx, md, nullptr))
			throw std::runtime_error("Cannot final digest");
	}
};

int main(int argc, char **argv)
{
	const char * input4 = "iwrupvqb";
	std::array<unsigned char, 16> h, h5, h6;
	uint64_t i = 0, five = 0, six = 0;
	std::stringstream ss;
	MD5 md5;

	if (argc > 1)
		input4 = argv[1];

	while (five == 0 or six == 0) {
		ss.str("");
		ss << input4 << i;
		// std::cout << "'" << ss.str().c_str() << "' " << ss.tellp() << std::endl;

		md5.h(ss.str(), h.data());
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
