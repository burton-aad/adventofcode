#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

fn neighbor_4(i: usize, j: usize, i_max: usize, j_max: usize) -> Vec<(usize, usize)> {
    let mut r = Vec::new();
    if i > 0 {
        r.push((i - 1, j))
    };
    if i + 1 < i_max {
        r.push((i + 1, j))
    };
    if j > 0 {
        r.push((i, j - 1))
    };
    if j + 1 < j_max {
        r.push((i, j + 1))
    };
    r
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let f = BufReader::new(fs::File::open(input).unwrap());

    let model: Vec<Vec<u8>> = f
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|e| e.to_digit(10).unwrap() as u8)
                .collect()
        })
        .collect();
    // println!("model {:?}", model);

    let ne4 = |i, j| neighbor_4(i, j, model.len(), model[0].len());

    let r = model
        .iter()
        .enumerate()
        .map(|(i, v)| {
            v.iter()
                .enumerate()
                .filter(|(j, &c)| ne4(i, *j).iter().all(|(x, y)| c < model[*x][*y]))
                .map(|(_, c)| c)
                .collect_vec()
        })
        .flatten()
        .collect_vec();
    // println!("r {:?}", r);

    println!(
        "Part 1: {}",
        r.iter().map(|&c| (c + 1) as usize).sum::<usize>()
    );

    let mut mm = model.clone();
    let mut u: u8 = 10;
    while let Some(x) = mm
        .iter()
        .enumerate()
        .filter_map(|(i, v)| v.iter().position(|&c| c < 9).map(|j| (i, j)))
        .next()
    {
        let mut next = vec![x];
        while let Some((i, j)) = next.pop() {
            if mm[i][j] < 9 {
                next.append(&mut ne4(i, j));
                mm[i][j] = u;
            }
        }
        u += 1
    }

    // println!("mm {:?}", mm);
    println!(
        "Part 2: {}",
        mm.iter()
            .flatten()
            .filter(|&&i| i != 9)
            .counts()
            .values()
            .sorted()
            .rev()
            .take(3)
            .product::<usize>()
    );
}
