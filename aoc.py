#!/usr/bin/env python3

import os, sys
import argparse
import asyncio
import subprocess
import time
from pathlib import Path
from typing import List

CWD = Path(__file__).parent

class Time:
    def __init__(self, time_ns):
        self.time_ns = time_ns

    @property
    def ms(self):
        return self.time_ns // 1_000_000

class Runner:
    def __init__(self, prog: Path):
        self.prog = prog
        self.day = prog.parent.name
        self.rc: int = None
        self.time: Time = Time(0)
        self._part1: str = None
        self._part2: str = None

    @property
    def part1(self):
        return self._part1 or "N/A"

    @property
    def part2(self):
        return self._part2 or "N/A"

    def compile(self):
        pass

    def run(self):
        raise NotImplementedError()

    def _run_exec(self, prog: Path, *args):
        start = time.time_ns()
        asyncio.run(self._async_run(prog, args))
        self.time = Time(time.time_ns() - start)

    async def _async_run(self, prog: Path, args = [], save_run=True):
        proc = await asyncio.create_subprocess_exec(
            prog, *args, stdout=asyncio.subprocess.PIPE,
            cwd=self.prog.parent,
        )

        extract_part = lambda l: l.decode().split(":", 1)[1].strip()

        async for l in proc.stdout:
            if l.startswith(b"Part 1:"):
                self._part1 = extract_part(l)
            elif l.startswith(b"Part 2:"):
                self._part2 = extract_part(l)
            elif self._part1 is None and b":" in l:
                self._part1 = extract_part(l)
            elif self._part2 is None and b":" in l:
                self._part2 = extract_part(l)

        await proc.wait()
        self.rc = proc.returncode

    def __repr__(self):
        return "Runner({}, time={}, rc={}, part1={!r}, part2={!r})".format(
            self.prog.name, self.time.ms, self.rc, self.part1, self.part2)


class Python(Runner):
    def run(self):
        self._run_exec(sys.executable, self.prog)

class Awk(Runner):
    def run(self):
        self._run_exec("awk", "-f", self.prog, "--", "input")

class C(Runner):
    def __init__(self, prog):
        super().__init__(prog)
        self.prog = self.prog.with_suffix('')

    def compile(self):
        ytd = self.prog.parents[1]
        subprocess.run(["make", self.prog.relative_to(ytd).with_suffix('')],
                       cwd=ytd)

    def run(self):
        self._run_exec(self.prog)


run_formats = {
    ".py": Python,
    ".c": C,
    ".awk": Awk,
}

def run(days):
    r = []
    for i, d in enumerate(days):
        print("{} / {}".format(i+1, len(days)))
        for f in sorted(d.iterdir()):
            if f.name == ".ignore":
                break
            elif f.name.startswith("Jour{:02}.".format(d.name)) and f.suffix in run_formats:
                runner = run_formats[f.suffix](f)
                runner.compile()
                runner.run()
                r.append(runner)
                break
        else:
            print("Warning: No source found to run {}".format(d))
    return r

def print_table(runs: List[Runner]):
    cols = ["day", "Part 1", "Part 2", "Time"]
    align = [len(c) for c in cols]
    for r in runs:
        align[1] = max(align[1], len(r.part1))
        align[2] = max(align[2], len(r.part2))
        align[3] = max(align[3], len("{}".format(r.time.ms)))
    length = sum(align) + len(align) * 3 - 1
    print("|".join("{:^{}}".format(c, a+2) for c, a in zip(cols, align)))
    print("-" * length)
    for r in runs:
        print("|".join("{:^{}}".format(c, a+2)
                       for c, a in zip([r.day, r.part1, r.part2, r.time.ms], align)))

def main():
    years = sorted(d.name for d in CWD.iterdir() if len(d.name) == 4 and d.name.startswith("20"))

    parser = argparse.ArgumentParser(description='Simple interface to run AoC pprograms')
    parser.add_argument("year", choices=years, help="Which year to run")
    parser.add_argument("-d", "--day", type=int, help="Run a single day of the year")
    args = parser.parse_args()

    year = CWD/"{}".format(args.year)
    if args.day:
        days = [year/"{:02}".format(args.day)]
        if not days[0].is_dir():
            raise ValueError("Invalid day {}".format(args.day))
    else:
        days = sorted(d for d in year.iterdir() if d.is_dir())

    print_table(run(days))

if __name__=="__main__":
    main()
