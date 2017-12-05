#!/usr/bin/awk -f

# première partie : grep -Evc '(\<[a-z]+\>).*\<\1\>' input4

// {
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
    print $0
}
