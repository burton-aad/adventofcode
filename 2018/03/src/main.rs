#![allow(non_snake_case)]

#[macro_use] extern crate lazy_static;
extern crate regex;

use std::{env, fmt, fs, cmp};
use std::io::{BufRead,BufReader};
use regex::Regex;

#[derive(Debug)]
struct Rect {
    id: u32,
    x: usize,
    y: usize,
    w: usize,
    h: usize,
}

impl Rect {
    fn from_input (line: &String) -> Rect {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)").unwrap();
        }
        let cap = RE.captures(&line).unwrap();
        // println!("{:?}", cap);
        Rect {
            id: cap[1].parse().unwrap(),
            x:  cap[2].parse().unwrap(),
            y:  cap[3].parse().unwrap(),
            w:  cap[4].parse().unwrap(),
            h:  cap[5].parse().unwrap(),
        }
    }

    fn overlap(&self, other: &Rect) -> usize {
        let oxa = cmp::max(self.x, other.x);
        let oxb = cmp::min(self.x+self.w, other.x+other.w);
        let oya = cmp::max(self.y, other.y);
        let oyb = cmp::min(self.y+self.h, other.y+other.h);
        if self.id != other.id && oxa < oxb && oya < oyb {
            (oxb - oxa) * (oyb - oya)
        }
        else {
            0
        }
    }
}

#[derive(Debug)]
struct Fabric{v: Vec<Vec<char>>}

impl fmt::Display for Fabric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for v in self.v.iter() {
            let s: String = v.iter().collect();
            writeln!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl Fabric {
    fn new() -> Fabric {
        let mut v = Vec::new();
        let u = Vec::new();
        v.push(u);
        Fabric{v}
    }

    fn count(&self, val: char) -> usize {
        self.v.iter().map(|ref v| v.iter().filter(|&c| *c == val).count()).sum()
    }

    fn set_rec(&mut self, rec: &Rect) {
        let endw = rec.x + rec.w;
        let endh = rec.y + rec.h;
        if endh + 1 > self.v.len() {
            let s = self.v[0].len();
            self.v.resize(endh+1, vec!['.'; s])
        }
        if endw + 1 > self.v[0].len() {
            for v in self.v.iter_mut() {
                v.resize(endw+1, '.');
            }
        }

        for x in 0..rec.w {
            for y in 0..rec.h {
                let elt = self.v[rec.y+y].get_mut(rec.x+x).unwrap();
                *elt = if *elt == '.' { 'O' } else { 'X' };
            }
        }
    }
}


fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input"));
    let mut fab = Fabric::new();
    let mut claims: Vec<Rect> = Vec::new();

    let f = fs::File::open(input).unwrap();
    for line in BufReader::new(f).lines() {
        let rec = Rect::from_input(&line.unwrap());
        claims.push(rec);
        fab.set_rec(claims.last().unwrap());
    }

    // println!("{:?}", claims);
    // println!("{}", fab);
    println!("square inches of overalpping: {}", fab.count('X'));

    let mut score: Vec<usize> = vec![];
    for r in claims.iter() {
        score.push(claims.iter().map(|ref rec| r.overlap(&rec)).sum());
    }
	let rno = &claims[score.iter().position(|&v| v == 0).unwrap()];
    println!("Rect with no overlap: {:?}", rno);
    println!("Part 2: {:?}", rno.id);
}
