#!/usr/bin/awk -f

{
	f = int($0/3) - 2
	t1 += f
	while (f > 0) {
		t2 += f
		f = int(f/3)-2
	}
}

END{print "part 1 : " t1; print "part 2 : " t2}
