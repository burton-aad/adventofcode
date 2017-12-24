#!/usr/bin/awk -f

BEGIN { sum = 0 }

// {
	print $0
	min=$1; max=$1;
	for (i=2; i<=NF; i++) {
		if ($i < min) min = $i
		if ($i > max) max = $i
	}
	print min " - " max
	sum += max - min
}

END { print sum }
