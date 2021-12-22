#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

use itertools::EitherOrBoth::{Both, Left, Right};
use itertools::{izip, Itertools};
use regex::Regex;
use std::io::{BufRead, BufReader};
use std::{env, fmt, fs};

#[derive(Default, Debug)]
struct Page {
    lines: Vec<Vec<bool>>,
}

macro_rules! izip_long {
    // Only case 2 is needed.
    ($first:expr, $second:expr $(,)*) => {
        $first.into_iter().zip_longest($second)
    };
}

impl Page {
    fn from_lines(lines: &[String]) -> Page {
        let mut p = Page {
            lines: vec![vec![]],
        };

        for (x, y) in lines.iter().filter(|l| l.contains(',')).map(|l| {
            l.split(',')
                .filter_map(|v| v.parse::<usize>().ok())
                .collect_tuple()
                .unwrap()
        }) {
            if x >= p.lines[0].len() {
                let s = vec![false; x - p.lines[0].len() + 1];
                for l in p.lines.iter_mut() {
                    l.extend(&s);
                }
            }
            if y >= p.lines.len() {
                for _ in p.lines.len()..=y {
                    p.lines.push(vec![false; p.lines[0].len()]);
                }
            }
            p.lines[y][x] = true;
        }

        p
    }

    fn fold_y(&mut self, line: usize) {
        self.lines = izip_long!(
            self.lines.iter().take(line).rev(),
            self.lines.iter().dropping(line + 1)
        )
        .map(|v| match v {
            Both(l, a) => izip!(l.iter(), a.iter())
                .map(|(&a, &b)| a || b)
                .collect_vec(),
            Right(l) | Left(l) => l.to_vec(),
        })
        .rev()
        .collect_vec();
    }

    fn transpose(&mut self) {
        let v = &self.lines;
        let mut r = vec![vec![false; v.len()]; v[0].len()];
        for x in 0..v.len() {
            for y in 0..v[0].len() {
                r[y][x] = v[x][y];
            }
        }
        self.lines = r;
    }

    fn fold_x(&mut self, col: usize) {
        self.transpose();
        self.fold_y(col);
        self.transpose();
    }

    fn fold(&mut self, typ: char, val: usize) {
        match typ {
            'y' => self.fold_y(val),
            'x' => self.fold_x(val),
            _ => panic!("Impossible"),
        }
    }
}

impl fmt::Display for Page {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for l in self.lines.iter() {
            for c in l.iter() {
                write!(f, "{}", if *c { '#' } else { '.' })?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input11"));
    let lines = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| l.unwrap())
        .collect_vec();

    let mut page = Page::from_lines(&lines);
    println!("Page {}x{}", page.lines[0].len(), page.lines.len());
    // println!("{}", page);

    lazy_static! {
        static ref RE: Regex = Regex::new(r"fold along (.)=(\d+)").unwrap();
    }

    let mut fold = Vec::new();
    for l in lines.iter() {
        match RE.captures(&l) {
            Some(cap) => fold.push((
                cap[1].chars().nth(0).unwrap(),
                cap[2].parse::<usize>().unwrap(),
            )),
            None => (),
        }
    }

    let (t, v) = fold[0];
    page.fold(t, v);
    println!(
        "Part 1 : {}",
        page.lines.iter().flatten().filter(|&&c| c).count()
    );

    for (t, v) in fold.iter().dropping(1) {
        // println!("fold_{}({})", *t, *v);
        page.fold(*t, *v);
    }
    println!("Part2 :");
    println!("{}", page);
}
