#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

use itertools::iproduct;
use regex::Regex;
use std::io::{BufRead, BufReader};
use std::{cmp, env, fs};

#[derive(Default, Debug)]
struct Square {
    // as start end
    x: (i32, i32),
    y: (i32, i32),
}

impl Square {
    fn from_line(line: &str) -> Square {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"target area: x=(\d+)..(\d+), y=(-\d+)..(-\d+)").unwrap();
        }
        let cap = RE.captures(&line).unwrap();
        let xa = cap[1].parse::<i32>().unwrap();
        let xb = cap[2].parse::<i32>().unwrap();
        let ya = cap[3].parse::<i32>().unwrap();
        let yb = cap[4].parse::<i32>().unwrap();
        Square {
            x: (cmp::min(xa, xb), cmp::max(xa, xb)),
            y: (cmp::min(ya, yb), cmp::max(ya, yb)),
        }
    }

    fn is_valid_step(&self, x: i32, y: i32) -> bool {
        // return false if step is too far away from square
        x <= self.x.1 && y >= self.y.0
    }

    fn is_inside(&self, x: i32, y: i32) -> bool {
        self.x.0 <= x && x <= self.x.1 && self.y.0 <= y && y <= self.y.1
    }
}

#[derive(Debug)]
struct ProbeIter {
    cur_pos: (i32, i32),
    cur_v: (i32, i32),
}

impl Iterator for ProbeIter {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        self.cur_pos.0 += self.cur_v.0;
        self.cur_pos.1 += self.cur_v.1;
        // Update speed
        if self.cur_v.0 > 0 {
            self.cur_v.0 -= 1;
        } else if self.cur_v.0 < 0 {
            self.cur_v.0 += 1;
        }
        self.cur_v.1 -= 1;
        Some(self.cur_pos)
    }
}

#[derive(Default, Debug)]
struct Probe {
    pos: (i32, i32),
    v: (i32, i32),
}

impl Probe {
    fn make(start_v: (i32, i32)) -> Probe {
        Probe {
            pos: (0, 0),
            v: start_v,
        }
    }

    fn iter_step(&self) -> ProbeIter {
        ProbeIter {
            cur_pos: self.pos,
            cur_v: self.v,
        }
    }
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input17"));
    let line = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| l.unwrap())
        .nth(0)
        .unwrap();

    let sq = Square::from_line(&line);
    println!("got square {:?}", sq);

    // By rules, max height is not influence by x.
    // y position descending woule be n(n+1)/2 at each step. Max possible
    // simply depend on square
    let far_y = cmp::max(sq.y.0.abs(), sq.y.1.abs());
    let max_h = (far_y - 1) * far_y / 2;
    println!("Part 1: {} (for y = {})", max_h, far_y - 1);

    // Only work for x > 0 and y < 0
    let max_x = sq.x.1;
    let min_x = (0..max_x)
        .filter(|i| i * (i + 1) / 2 > sq.x.0)
        .nth(0)
        .unwrap();
    let max_y = far_y - 1;
    let min_y = sq.y.0;
    println!(
        "Part 2: {}",
        iproduct!(min_x..=max_x, min_y..=max_y)
            .filter_map(|(x, y)| Probe::make((x, y))
                .iter_step()
                .take_while(|(x, y)| sq.is_valid_step(*x, *y))
                .filter(|(x, y)| sq.is_inside(*x, *y))
                .nth(0))
            .count()
    );
}
