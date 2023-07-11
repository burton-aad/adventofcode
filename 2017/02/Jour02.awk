#!/usr/bin/awk -f

BEGIN { sum = 0; sum2 = 0 }

// {
	# print $0
	min=$1; max=$1;
	for (i=2; i<=NF; i++) {
		if ($i < min) min = $i
		if ($i > max) max = $i
	}
	# print min " - " max
	sum += max - min

	for (i=1; i<=NF; i++) {
		for (j=i+1; j<=NF; j++) {
			if ($i > $j && $i % $j == 0) { sum2 += $i / $j; next }
			if ($i < $j && $j % $i == 0) { sum2 += $j / $i; next }
		}
	}
}

END { print "Part 1 : " sum; print "Part 2 : " sum2; }
