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
    floor: Vec<Vec<i32>>,
}

impl fmt::Display for Ocean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for v in self.floor.iter() {
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
            self.floor.resize_with(self.m_y, Default::default);
            resize_x = true;
        }

        if resize_x {
            for v in self.floor.iter_mut() {
                v.resize(self.m_x, 0);
            }
        }
    }

    fn get(&mut self, x: usize, y: usize) -> &mut i32 {
        self.inc_size(x, y);
        &mut self.floor[y][x]
    }

    fn add_hv_line(&mut self, x1: usize, y1: usize, x2: usize, y2: usize) {
        // println!("{},{} -> {},{}", x1, y1, x2, y2);
        if x1 == x2 {
            for y in bidi_range(y1 as i32, y2 as i32) {
                *self.get(x1, y as usize) += 1;
            }
        } else if y1 == y2 {
            for x in bidi_range(x1 as i32, x2 as i32) {
                *self.get(x as usize, y1) += 1;
            }
        }
		// Comment for part 1
		else {
			for (x, y) in bidi_range(x1 as i32, x2 as i32).zip(bidi_range(y1 as i32, y2 as i32)) {
				*self.get(x as usize, y as usize) += 1;
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
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let f = BufReader::new(fs::File::open(input).unwrap());
    let mut o: Ocean = Default::default();

    for l in f.lines().filter_map(Result::ok) {
        o.add_line(&l);
    }

    // println!("{}", o);
    println!(
        "Count : {}",
        o.floor.iter()
            .map(|v| v.iter().filter(|x| **x > 1).count())
            .sum::<usize>()
    );
}
