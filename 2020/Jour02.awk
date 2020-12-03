#!/usr/bin/awk -f

BEGIN{ FS="[- :]+" }

{
	l = length(gensub("[^"$3"]", "", "g", $4));
	a += l >= $1 && l <= $2
	b += (match($4, "^.{" $1-1 "}" $3) + match($4, "^.{" $2-1 "}" $3)) == 1
}

END{print "Part 1 : " a ", Part 2 : " b}
