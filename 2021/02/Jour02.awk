#!/usr/bin/awk -f

/forward/{h += $2; d2 += a * $2}
/down/{d1 += $2; a += $2}
/up/{d1 -= $2; a -= $2}
END{print "Part 1 : " h * d1; print "Part 2 : " h * d2}
