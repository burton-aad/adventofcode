#![allow(non_snake_case)]

use itertools::{repeat_n, Itertools};
use std::collections::HashSet;
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

// Return shortest distance to all points + precedent position in path for each point
// starting at start param.
fn dijkstra(
    weight: &Vec<Vec<u8>>,
    start: (usize, usize),
) -> (Vec<Vec<usize>>, Vec<Vec<(usize, usize)>>) {
    let mut dist = vec![vec![usize::MAX; weight[0].len()]; weight.len()];
    let mut prec = vec![vec![(0, 0); weight[0].len()]; weight.len()];
    let mut queue = HashSet::<(usize, usize)>::new();
    let mut free = vec![vec![true; weight[0].len()]; weight.len()];

    // Start distance
    dist[start.0][start.1] = 0;
    queue.insert(start);

    let ne4 = |i, j| neighbor_4(i, j, weight.len(), weight[0].len());

    // while queue.iter().flatten().any(|b| *b) {
    while !queue.is_empty() {
        // next point
        let &(x, y) = queue.iter().min_by_key(|(i, j)| dist[*i][*j]).unwrap();
        queue.remove(&(x, y));
        free[x][y] = false;

        // Set dist for all points neighbor
        for &(xv, yv) in ne4(x, y).iter().filter(|(i, j)| free[*i][*j]) {
            let new_d = dist[x][y] + weight[xv][yv] as usize;
            if dist[xv][yv] > new_d {
                dist[xv][yv] = new_d;
                prec[xv][yv] = (x, y);
            }
            queue.insert((xv, yv));
        }
    }

    (dist, prec)
}

fn extend_risk(risk: &Vec<Vec<u8>>, extent: usize) -> Vec<Vec<u8>> {
    let r = risk
        .iter()
        .map(|v| {
            repeat_n(v, extent)
                .enumerate()
                .map(|(i, v)| {
                    v.iter()
                        .map(move |c| *c + i as u8)
                        .map(|c| if c > 9 { c - 9 } else { c })
                })
                .flatten()
                .collect_vec()
        })
        .collect_vec();

    repeat_n(r, extent)
        .enumerate()
        .map(|(i, vv)| {
            vv.into_iter().map(move |v| {
                v.iter()
                    .map(|c| *c + i as u8)
                    .map(|c| if c > 9 { c - 9 } else { c })
                    .collect_vec()
            })
        })
        .flatten()
        .collect_vec()
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input11"));
    let risk = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect_vec()
        })
        .collect_vec();
    // println!("risk : {:?}", risk);

    let (dist, _prec) = dijkstra(&risk, (0, 0));
    println!("Part 1 : {}", dist.last().unwrap().last().unwrap());

    let erisk = extend_risk(&risk, 5);
    println!("new size {}x{}", erisk.len(), erisk[0].len());
    let (dist, _prec) = dijkstra(&erisk, (0, 0));
    println!("Part 2 : {}", dist.last().unwrap().last().unwrap());
}
