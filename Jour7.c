#include <stdio.h>
#include <string.h>
#include <errno.h>

#define INPUT_NAME "input7"

int supprot_tls(char *line, int r)
{
	int i = 0;
	char *p = line;
	char a = 0, b = 0;
	int in_bracket = 0;
	int ret = 0;

	for (i=0; i<r-1; i++, p++) {
		switch (*p) {
		case '[':
			in_bracket = 1;
			break;
		case ']':
			in_bracket = 0;
			break;
		default:
			if (*p == b && *(p+1) == a && a!=b) {
				if (in_bracket)
					return 0; // false
				else
					ret = 1;
			}
		}
		a = b;
		b = *p;
	}

	return ret;
}

int main()
{
	int i, r;
	FILE *f;
	char *line = NULL;
	size_t len = 0;
	int count = 0, linecnt = 0;

	f = fopen(INPUT_NAME, "rb");
	if (!f) {
		printf("Erreur fopen '%s': %s\n", INPUT_NAME, strerror(errno));
		return 1;
	}

	while((r = getline(&line, &len, f)) > 0) {
		while (line[r-1] == '\n' || line[r-1] == '\r') {
			line[r-1] = '\0';
			r--;
		}

		if (supprot_tls(line, r)) {
			printf("line '%s' support TLS\n", line);
			count++;
		}
		else
			printf("line '%s' does not support TLS\n", line);
		linecnt++;
	}

	printf("count : %d / %d\n", count, linecnt);
}
