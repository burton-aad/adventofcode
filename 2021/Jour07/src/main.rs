#![allow(non_snake_case)]

use std::io::{BufRead, BufReader};
use std::{env, fs};

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let mut f = BufReader::new(fs::File::open(input).unwrap());
    let mut buf = String::new();

    f.read_line(&mut buf).expect("Cannot read line");
    let start_pos: Vec<i32> = buf
        .split(",")
        .map(|s| s.trim().parse::<i32>().unwrap())
        .collect();
    // println!("start_pos {:?}", start_pos);

	let min = *start_pos.iter().min().unwrap();
	let max = *start_pos.iter().max().unwrap();

    // Brute force :)
	let vals = (min..=max).map(|rf| start_pos.iter().map(|p| (p - rf).abs()).sum::<i32>());
    println!("Part 1: {}", vals.min().unwrap());

	let vals = (min..=max).map(|rf| start_pos.iter().map(|p| (p - rf).abs() * ((p - rf).abs() + 1) / 2).sum::<i32>());
    println!("Part 2: {}", vals.min().unwrap());
}
