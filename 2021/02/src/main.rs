#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

fn main() {
    let input = env::args().nth(1).expect("Usage: run <input>");
    let f = BufReader::new(fs::File::open(input).unwrap());

    let (mut h, mut d1, mut d2) = (0, 0, 0);
    for l in f.lines().filter_map(Result::ok) {
        // println!("line {:?}", l);
        let (action, val_s) = l.split(" ").collect_tuple().expect("2 values");
        let val = val_s.parse::<usize>().unwrap();
        match action {
            "forward" => {
                h += val;
                d2 += d1 * val
            }
            "down" => d1 += val,
            "up" => d1 -= val,
            _ => unreachable!(),
        }
    }

    println!("Part 1: {}", h * d1);
    println!("Part 2: {}", h * d2);
}
