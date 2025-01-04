#!/usr/bin/awk -f

# première partie : grep -Evc '(\<[a-z]+\>).*\<\1\>' input

// {
    # search repeat
	delete t
	c = 0
	for (i=1; i<=NF; i++) {
		if ($i in t)
			break
		t[$i] = 1
		c++
	}
	if (c == NF)
		sum1 += 1

    # search anagram
	delete t
	for (i=1; i<=NF; i++) {
		n = split($i, a, "")
		asort(a)
		k = ""
		for (j=1; j<=n; j++)
			k = k a[j]
		if (k in t)
			next # anagram found go next line
		else
			t[k] = 1
    }
    # print valid
    # print $0
	sum += 1
}

END{
	print "Part 1:", sum1
	print "Part 2:", sum
}
