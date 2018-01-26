#! /usr/bin/awk -f

BEGIN { FS="[-xyT[:blank:]]*" }

# NR < 10 {
# 	print
# 	print $2, $3, $4, $5, $6
# }

NR > 2 {
	Size = $4
	Used = $5
	Avail = $6

	for (x in a) {
		for (y in a[x]) {
			if (a[x][y]["used"] > 0 && Avail > a[x][y]["used"]) {
				viables[x][y][$2][$3] = 1
				num_v++
			}
			if (Used > 0 && a[x][y]["avail"] > Used) {
				viables[$2][$3][x][y] = 1
				num_v++
			}
		}
	}

	a[$2][$3]["size"] = Size
	a[$2][$3]["used"] = Used
	a[$2][$3]["avail"] = Avail
}

function print_grid(a,    x,y,t)
{
	t = ORS
	ORS = ""
	for (x in a) {
		for (y in a[x]) {
			if (x == 0 && y == 0)
				print "("
			else
				print " "
			if (a[x][y]["used"] > 400)
				print "#"
			else if (a[x][y]["used"] == 0) {
				print "_"
				vide_x = x
				vide_y = y
			}
			else if (x == length(a)-1 && y == 0)
				print "G"
			else
				print "."
			if (x == 0 && y == 0)
				print ")"
			else
				print " "
		}
		print "\n"
	}
	ORS = t
}

END {
	print length(a) * length(a[0])
	print "part 1 : " num_v

	print_grid(a)

	# deplacement du vide devant G
	print "\n"
	print "Move vide"
	for (i=vide_x; i>0; i--) {
		if (a[i-1][vide_y]["used"] > a[i][vide_y]["size"]) {
			print "ERROR"
			break
		}
		a[i][vide_y]["used"] = a[i-1][vide_y]["used"]
		a[i][vide_y]["avail"] = a[i][vide_y]["size"] - a[i][vide_y]["used"]
		a[i-1][vide_y]["used"] = 0
		a[i-1][vide_y]["avail"] = a[i-1][vide_y]["size"]
		test++
	}

	for (i=vide_y; i>0; i--) {
		if (a[0][i-1]["used"] > a[0][i]["size"]) {
			print "ERROR\n"
			break
		}
		a[0][i]["used"] = a[0][i-1]["used"]
		a[0][i]["avail"] = a[0][i]["size"] - a[0][i]["used"]
		a[0][i-1]["used"] = 0
		a[0][i-1]["avail"] = a[0][i-1]["size"]
		test++
	}

	for (i=0; i<length(a)-2; i++) {
		if (a[i+1][0]["used"] > a[i][0]["size"]) {
			print "ERROR"
			break
		}
		a[i][0]["used"] = a[i+1][0]["used"]
		a[i][0]["avail"] = a[i][0]["size"] - a[i][0]["used"]
		a[i+1][0]["used"] = 0
		a[i+1][0]["avail"] = a[i+1][0]["size"]
		test++
		test2++
	}

	print "test " test ", " test2
	# solution, un deplacement de G == 5
	print "part 2 : " test + 5*test2 + 1
	# print_grid(a)
}

