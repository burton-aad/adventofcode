#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

fn main() {
    let input = env::args().nth(1).expect("Usage: run <input>");
    let f = BufReader::new(fs::File::open(input).unwrap());
    let mut v: Vec<usize> = Vec::new();

    for l in f.lines().filter_map(Result::ok) {
        // println!("line {:?}", l);
        v.push(l.parse::<usize>().unwrap());
    }

    let t: usize = v.iter().tuple_windows().map(|(a,b)| (a < b) as usize).sum();
    println!("Part 1: {:?}", t);

    let t2: usize = v
        .windows(3)
        .map(|w| w.iter().sum::<usize>())
        .tuple_windows()
        .map(|(a, b)| (a < b) as usize)
        .sum();
    println!("Part 2: {:?}", t2);
}
