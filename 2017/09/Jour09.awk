#!/usr/bin/awk -f

BEGIN { RS="<([^>!]|!.)*>"; FPAT="[{}]"; }

{
	# print "NR " NR " : " $0
	# print "RT = \"" RT "\""
	for (i=1; i<=NF; i++)
		if ($i == "{")
			sum += ++score
		else
			score--
	# print score

	if (RT) {
		gsub("!.", "", RT)
		# print "n "n", s "RT
		nongarb += length(RT)-2
	}
}

END { 
	# print "end : " score " - " sum " - " nongarb
	print "Part 1: " sum
	print "Part 2: " nongarb
}
