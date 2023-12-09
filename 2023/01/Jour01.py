
import sys
import argparse
import re

ref = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}

def num1(l):
    m = re.findall(r"\d", l)
    return int(m[0] + m[-1])

def num2(l):
    # overlapping search with lookahead of nothing.
    m = re.findall(r"(?=(\d|one|two|three|four|five|six|seven|eight|nine))", l)
    r = int(ref.get(m[0], m[0]) + ref.get(m[-1], m[-1]))
    return r

def jour01(input):
    print("Part 1:", sum(num1(l) for l in input))
    print("Part 2:", sum(num2(l) for l in input))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='AoC 2023 - Jour 01')
    parser.add_argument('input_file', nargs='?', default="input", help='the input file')
    args = parser.parse_args(sys.argv[1:])
    with open(args.input_file) as f:
        jour01(f.readlines())
        # jour01(["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"])
        # jour01(["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen", "6czklmzsmxgmktzxmxsixmnlfxonetwonesgj"])
