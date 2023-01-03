#include <stdio.h>
#include <string.h>
#include <errno.h>

#define INPUT_NAME "input"

int support_tls(char *line, int r)
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

int support_ssl(char *line, int r)
{
	int i = 0, j;
	char *p = line;
	char a = 0;
	int in_bracket = 0;
	int supernet = 0, s[1000];
	int hypernet = 0, h[1000];

	for (i=0; i<r-1; i++, p++) {
		switch (*p) {
		case '[':
			in_bracket = 1;
			break;
		case ']':
			in_bracket = 0;
			break;
		default:
			if (*(p+1) == a && a!=*p) {
				if (in_bracket) {
					h[hypernet] = (*p << 8) + a;
					hypernet++;
				}
				else {
					s[supernet] = (a << 8) + *p;
					supernet++;
				}
			}
		}
		a = *p;
	}

	for (i=0; i<supernet; i++)
		for (j=0; j<hypernet; j++)
			if (s[i] == h[j])
				return 1;

	return 0;
}

int main()
{
	int i, r;
	FILE *f;
	char *line = NULL;
	size_t len = 0;
	int tls = 0, ssl = 0, linecnt = 0;

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

		if (support_tls(line, r)) {
			/* printf("line '%s' support TLS\n", line); */
			tls++;
		}
		/* else */
		/* 	printf("line '%s' does not support TLS\n", line); */

		if (support_ssl(line, r)) {
			/* printf("line '%s' support SSL\n", line); */
			ssl++;
		}
		/* else */
		/* 	printf("line '%s' does not support SSL\n", line); */

		linecnt++;
	}

	printf("tls support : %d / %d\n", tls, linecnt);
	printf("ssl support : %d / %d\n", ssl, linecnt);
}
