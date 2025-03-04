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
        self.dir = prog.parent
        self.day = prog.parent.name
        self.rc: int = None
        self.time: Time = Time(0)
        self._part1: str = None
        self._part2: str = None
        self.verbose = False

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

    async def _async_run(self, prog: Path, args = []):
        proc = await asyncio.create_subprocess_exec(
            prog, *args, stdout=asyncio.subprocess.PIPE,
            cwd=self.dir,
        )

        extract_part = lambda l: l.decode().split(":", 1)[1].strip()
        async for l in proc.stdout:
            if self.verbose:
                print(l.decode().strip())
            if l.startswith(b"Part 1"):
                self._part1 = extract_part(l)
            elif l.startswith(b"Part 2"):
                self._part2 = extract_part(l)
            elif self._part1 is None and b":" in l:
                self._part1 = extract_part(l)
            elif self._part2 is None and b":" in l:
                self._part2 = extract_part(l)

        self.rc = await proc.wait()

    def __repr__(self):
        return "Runner({}, time={}, rc={}, part1={!r}, part2={!r})".format(
            self.prog.name, self.time.ms, self.rc, self.part1, self.part2)


class Python(Runner):
    def run(self):
        self._run_exec(sys.executable, "-u", self.prog)

class Awk(Runner):
    def run(self):
        self._run_exec("awk", "-f", self.prog, "--", "input")

class ELisp(Runner):
    def run(self):
        if (self.prog.parent / "input").is_file():
            self._run_exec("emacs", "-Q", "--script", self.prog, "--", "input")
        else:
            self._run_exec("emacs", "-Q", "--script", self.prog)

class Bash(Runner):
    def run(self):
        self._run_exec("bash", self.prog, "input")

class C(Runner):
    def __init__(self, prog, *args, **kwargs):
        super().__init__(prog, *args, **kwargs)
        self.prog = self.prog.with_suffix('')

    def compile(self):
        subprocess.run(["make", self.prog.relative_to(self.dir).with_suffix('')],
                       cwd=self.dir)

    def run(self):
        self._run_exec(self.prog)

class Rs(C):
    def compile(self):
        subprocess.run(["rustc", self.prog.relative_to(self.dir).with_suffix('.rs')],
                       cwd=self.dir)

class Go(C):
    def compile(self):
        subprocess.run(["go", "build"], cwd=self.dir)

class Zig(Runner):
    def compile(self):
        subprocess.run(["zig", "build-exe", self.prog.name], cwd=self.dir)

    def run(self):
        self._run_exec(self.prog.with_suffix(''), "input")

class Rust(Runner):
    def __init__(self, prog, *args, **kwargs):
        super().__init__(prog, *args, **kwargs)
        self.prog = self.prog.parent / "target" / "debug" / "Jour{}".format(self.prog.parent.name)

    def compile(self):
        subprocess.run(["cargo", "build"], cwd=self.dir)

    def run(self):
        self._run_exec(self.prog, "input")


run_formats = {
    ".py": Python,
    ".c": C,
    ".cpp": C,
    ".awk": Awk,
    ".el": ELisp,
    ".rs": Rs,
    ".sh": Bash,
    ".go": Go,
    ".zig": Zig,
}

def parse_days(year, day):
    year = CWD/"{}".format(year)
    if day:
        days = [year/"{:02}".format(day)]
        if not days[0].is_dir():
            raise ValueError("Invalid day {}".format(day))
    else:
        days = sorted(d for d in year.iterdir() if d.is_dir())

    r = []
    for i, d in enumerate(days):
        for f in sorted(d.iterdir()):
            if f.name == ".ignore":
                break
            elif f.name.startswith("Jour{:02}.".format(d.name)) and f.suffix in run_formats:
                r.append(run_formats[f.suffix](f))
                break
            elif f.name == "Cargo.toml":
                r.append(Rust(f))
                break
        else:
            print("Warning: No source found to run {}".format(d))
    return r

def report_table(runs: List[Runner]):
    cols = [" day", "Part 1", "Part 2", "Time (ms)"]
    alen = [len(c) for c in cols]
    align = [">", "^", "^", ">"]
    fmt = ["", "", "", ","]
    for r in runs:
        alen[1] = max(alen[1], len(r.part1))
        alen[2] = max(alen[2], len(r.part2))
        alen[3] = max(alen[3], len("{:{}}".format(r.time.ms, fmt[3])))
    length = sum(alen) + len(alen) * 3 - 1
    print(" | ".join("{:^{}}".format(c, a) for c, a in zip(cols, alen)))
    print("-" * length)
    for r in runs:
        print(" | ".join("{:{}{}{}}".format(c, a, l, f)
                         for c, f, a, l in zip([r.day, r.part1, r.part2, r.time.ms], fmt, align, alen)))

def main():
    years = sorted(d.name for d in CWD.iterdir() if len(d.name) == 4 and d.name.startswith("20"))

    parser = argparse.ArgumentParser(description='Simple interface to run AoC pprograms')
    parser.add_argument("year", choices=years, help="Which year to run")
    parser.add_argument("-d", "--day", type=int, help="Run a single day of the year")
    parser.add_argument("-v", "--verbose", action="store_true", help="Let prog output be printed")
    args = parser.parse_args()

    runners = parse_days(args.year, args.day)
    for i, r in enumerate(runners):
        print("{} / {}".format(i+1, len(runners)))
        r.verbose = args.verbose
        r.compile()
        r.run()
    if args.verbose:
        print(runners)
    else:
        report_table(runners)

if __name__=="__main__":
    main()
