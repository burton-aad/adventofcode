#!/usr/bin/awk -f

BEGIN { sum = 0  }

// {
	for (i=1; i<=NF; i++) {
		for (j=i+1; j<=NF; j++) {
			if ($i > $j && $i % $j == 0) { sum += $i / $j; next }
			if ($i < $j && $j % $i == 0) { sum += $j / $i; next }
		}
	}
}

END { print sum }
