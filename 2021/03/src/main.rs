#![allow(non_snake_case)]

// use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

fn loop_filter(vals: &Vec<String>, cmp: fn(&Vec<&String>, &Vec<&String>) -> bool) -> usize {
    let mut l: Vec<&String> = vals.iter().collect();
    let mut rank: usize = 0;
    while l.len() > 1 {
        let (o, z): (Vec<_>, Vec<_>) = l.iter().partition(|s| s.chars().nth(rank).unwrap() == '1');
        if cmp(&o, &z) {
            l = o;
        } else {
            l = z;
        }
        rank += 1;
    }
    return usize::from_str_radix(l[0], 2).unwrap();
}

fn main() {
    let input = env::args().nth(1).expect("Usage: run <input>");
    let f = BufReader::new(fs::File::open(input).unwrap());
    let mut cols = [[0; 2]; 12];
    let mut vals: Vec<String> = vec![];

    for l in f.lines().filter_map(Result::ok) {
        // println!("line {:?}", l);
        for (i, v) in l.chars().enumerate() {
            cols[i][(v == '1') as usize] += 1;
        }
        vals.push(l);
    }

    let mut gamma: usize = 0;
    let mut epsilon: usize = 0;
    for [a, b] in cols {
        gamma <<= 1;
        epsilon <<= 1;
        if a < b {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }
    println!("Part 1: {}", gamma * epsilon);

    let ox = loop_filter(&vals, |o, z| o.len() >= z.len());
    let co2 = loop_filter(&vals, |o, z| o.len() < z.len());
    println!("Part 2: {}", ox * co2);
}
