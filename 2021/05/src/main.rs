#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

use regex::Regex;
use std::io::{BufRead, BufReader};
use std::{env, fmt, fs};
use num::range_step;

#[derive(Default, Debug)]
struct Ocean {
    m_y: usize,
    m_x: usize,
    floor1: Vec<Vec<i32>>,
    floor2: Vec<Vec<i32>>,
}

impl fmt::Display for Ocean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for v in self.floor1.iter() {
            writeln!(f, "{:?}", v)?;
        }
        for v in self.floor2.iter() {
            writeln!(f, "{:?}", v)?;
        }
        Ok(())
    }
}

fn bidi_range(x: i32, y: i32) -> num::iter::RangeStep<i32>
{
	if x > y {
		range_step(x, y-1, -1)
	}
	else {
		range_step(x, y+1, 1)
	}
}

impl Ocean {
    fn inc_size(&mut self, x: usize, y: usize) {
        let mut resize_x = x >= self.m_x;
        self.m_x = self.m_x.max(x + 1);

        if y >= self.m_y {
            self.m_y = y + 1;
            self.floor1.resize_with(self.m_y, Default::default);
            self.floor2.resize_with(self.m_y, Default::default);
            resize_x = true;
        }

        if resize_x {
            for v in self.floor1.iter_mut() {
                v.resize(self.m_x, 0);
            }
            for v in self.floor2.iter_mut() {
                v.resize(self.m_x, 0);
            }
        }
    }

    fn add_point(&mut self, x: usize, y: usize, hv_line: bool) {
        self.inc_size(x, y);
		if hv_line { self.floor1[y][x] += 1 }
        self.floor2[y][x] += 1
    }

    fn add_hv_line(&mut self, x1: usize, y1: usize, x2: usize, y2: usize) {
        // println!("{},{} -> {},{}", x1, y1, x2, y2);
        if x1 == x2 {
            for y in bidi_range(y1 as i32, y2 as i32) {
                self.add_point(x1, y as usize, true);
            }
        } else if y1 == y2 {
            for x in bidi_range(x1 as i32, x2 as i32) {
                self.add_point(x as usize, y1, true);
            }
        }
		else {
			for (x, y) in bidi_range(x1 as i32, x2 as i32).zip(bidi_range(y1 as i32, y2 as i32)) {
				self.add_point(x as usize, y as usize, false);
			}
		}
    }

    fn add_line(&mut self, line: &str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
        }
        let cap = RE.captures(line).unwrap();
        self.add_hv_line(
            cap[1].parse::<usize>().unwrap(),
            cap[2].parse::<usize>().unwrap(),
            cap[3].parse::<usize>().unwrap(),
            cap[4].parse::<usize>().unwrap(),
        );
    }
}

fn main() {
    let input = env::args().nth(1).expect("Usage: run <input>");
    let f = BufReader::new(fs::File::open(input).unwrap());
    let mut o: Ocean = Default::default();

    for l in f.lines().filter_map(Result::ok) {
        o.add_line(&l);
    }

    // println!("{}", o);
    println!(
        "Part 1: {}",
        o.floor1.iter()
            .map(|v| v.iter().filter(|x| **x > 1).count())
            .sum::<usize>()
    );
    println!(
        "Part 2: {}",
        o.floor2.iter()
            .map(|v| v.iter().filter(|x| **x > 1).count())
            .sum::<usize>()
    );
}
