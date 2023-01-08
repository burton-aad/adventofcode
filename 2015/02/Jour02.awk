#!/usr/bin/awk -f

BEGIN { FS = "x" }

// {
	A = $1*$2
	B = $1*$3
	C = $2*$3
	a[NR] = 2*A + 2*B + 2*C
	if (A < B) {
		if (A < C)
			a[NR] += A
		else
			a[NR] += C
	}
	else if (B < C)
		a[NR] += B
	else
		a[NR] += C

	b[NR] = $1*$2*$3
	if ($1 < $2) {
		if ($2 < $3)
			b[NR] += 2*$1+2*$2
		else
			b[NR] += 2*$1+2*$3
	}
	else if ($1 < $3)
		b[NR] += 2*$1+2*$2
	else
		b[NR] += 2*$2+2*$3

	# print NR " -> " a[NR] ", " b[NR]
}

END {
	for (i in a) {
		ca += a[i]
		cb += b[i]
	}
	print ca ", " cb
}
