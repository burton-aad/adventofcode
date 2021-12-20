#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

fn neighbor_8(i: usize, j: usize, i_max: usize, j_max: usize) -> Vec<(usize, usize)> {
    let mut r = Vec::new();
    if i > 0 {
        if j > 0 {
            r.push((i - 1, j - 1));
        }
        r.push((i - 1, j));
        if j + 1 < j_max {
            r.push((i - 1, j + 1));
        }
    }
    if j > 0 {
        r.push((i, j - 1));
    }
    if j + 1 < j_max {
        r.push((i, j + 1));
    }
    if i + 1 < i_max {
        if j > 0 {
            r.push((i + 1, j - 1));
        }
        r.push((i + 1, j));
        if j + 1 < j_max {
            r.push((i + 1, j + 1));
        }
    }
    r
}

fn step(energy: &mut Vec<Vec<u32>>) -> usize {
    let x = energy.len();
    let y = energy[0].len();
    let ne8 = |i, j| neighbor_8(i, j, x, y);
    let mut queue = Vec::new();
    let mut r = 0usize;

    // Zeros the flashed inline
    for (i, in_v) in energy.iter_mut().enumerate() {
        for (j, v) in in_v.iter_mut().enumerate() {
            *v += 1;
            if *v > 9 {
                queue.extend(ne8(i, j));
                r += 1;
                *v = 0;
            }
        }
    }

    while let Some((i, j)) = queue.pop() {
        if energy[i][j] != 0 {
            energy[i][j] += 1;
            if energy[i][j] > 9 {
                queue.extend(ne8(i, j));
                r += 1;
                energy[i][j] = 0;
            }
        }
    }

    r
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input11"));
    let start_energy = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect_vec()
        })
        .collect_vec();

    let mut energy = start_energy.clone();
	println!("Part 1 {}", (0..100).map(|_| step(&mut energy)).sum::<usize>());

	let total = energy.len() * energy[0].len();
	let mut s = 100;
	while step(&mut energy) != total { s += 1; }
	println!("Part 2 {}", s+1);
}
