#!/usr/bin/awk -f

BEGIN{ freq = 0 }

{ freq += $0 }

END { print "frequence : " freq }
