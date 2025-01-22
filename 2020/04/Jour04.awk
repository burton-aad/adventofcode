#!/usr/bin/awk -f

BEGIN {RS="[ \n]"; FS=":"}

/^$/ {
	r += v == 0x7F; v = 0
	r2 += v2 == 0x7F; v2 = 0
}

/byr/ { v += lshift(1, 0) }
/byr:[0-9]{4}$/ { v2 += lshift(1920 <= $2 && $2 <= 2002, 0) }

/iyr/ { v += lshift(1, 1) }
/iyr:[0-9]{4}$/ { v2 += lshift(2010 <= $2 && $2 <= 2020, 1) }

/eyr/ { v += lshift(1, 2) }
/eyr:[0-9]{4}$/ { v2 += lshift(2020 <= $2 && $2 <= 2030, 2) }

/hgt/ { v += lshift(1, 3) }
/hgt:[0-9]+cm$/ { v2 += lshift("150cm" <= $2 && $2 <= "193cm", 3) }
/hgt:[0-9]+in$/ { v2 += lshift("59in" <= $2 && $2 <= "76in", 3) }

/hcl/ { v += lshift(1, 4) }
/hcl:#[0-9a-f]{6}$/ { v2 += lshift(1, 4) }

/ecl/ { v += lshift(1, 5) }
/ecl:(amb|blu|brn|gry|grn|hzl|oth)$/ { v2 += lshift(1, 5) }

/pid/ { v += lshift(1, 6) }
/pid:[0-9]{9}$/ { v2 += lshift(1, 6) }

# /cid/ { ignored }

END {
	r += v == 0x7F; r2 += v2 == 0x7F
	print "Part 1 : " r
	print "Part 2 : " r2
}
