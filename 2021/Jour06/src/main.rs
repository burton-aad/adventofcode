#![allow(non_snake_case)]

use std::io::{BufRead, BufReader};
use std::{collections, env, fs};

fn day_pass(deq : &mut collections::VecDeque<usize>)
{
	let n = deq.pop_front().unwrap();
	deq[6] += n;
	deq.push_back(n);
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let mut f = BufReader::new(fs::File::open(input).unwrap());
    let mut buf = String::new();

    f.read_line(&mut buf).expect("Cannot read line");
    let start_days: Vec<usize> = buf
        .split(",")
        .map(|s| s.trim().parse::<usize>().unwrap())
        .collect();

    println!("start {:?}", start_days);

    let mut deq = collections::VecDeque::<usize>::with_capacity(9);
    deq.resize(9, 0);
    for i in start_days {
        deq[i] += 1;
    }

    println!("init deq {:?}", deq);
    for _i in 0..80 {
		day_pass(&mut deq);
        // println!("After {} days : deq {:?} -> {}", i+1, deq, deq.iter().sum::<u32>());
    }
    println!("After 80 days : {}", deq.iter().sum::<usize>());

    for _i in 80..256 {
		day_pass(&mut deq);
    }
    println!("After 256 days : {}", deq.iter().sum::<usize>());
}
