#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fmt, fs};

#[derive(Debug, Clone)]
enum SFNum {
    Regular(u8),
    Pair(Box<SFNum>, Box<SFNum>),
}

#[derive(Debug)]
struct SFNumReducer {
    add_prev: Option<u8>,
    add_next: Option<u8>,
    used: bool,
}

impl fmt::Display for SFNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SFNum::Regular(i) => write!(f, "{}", i)?,
            SFNum::Pair(first, second) => write!(f, "[{},{}]", *first, *second)?,
        }
        Ok(())
    }
}

impl SFNum {
    fn parse(line: &String) -> SFNum {
        // Suppose the line is correct
        fn rec_parse(it: &mut std::str::Chars) -> SFNum {
            let n = it.next().unwrap();
            if '0' <= n && n <= '9' {
                SFNum::Regular(n.to_digit(10).unwrap() as u8)
            } else if n == '[' {
                let first = rec_parse(it);
                let n = it.next().unwrap();
                if n != ',' {
                    panic!("Invalid parse ','");
                }
                let second = rec_parse(it);
                let n = it.next().unwrap();
                if n != ']' {
                    panic!("Invalid parse ']'");
                }
                SFNum::Pair(Box::new(first), Box::new(second))
            } else {
                panic!("Should not happens");
            }
        }

        rec_parse(&mut line.chars())
    }

    #[allow(dead_code)]
    fn add_left(&self, val: u8) -> SFNum {
        match self {
            SFNum::Regular(i) => SFNum::Regular(*i + val),
            SFNum::Pair(f, s) => SFNum::Pair(Box::new(f.add_left(val)), s.clone()),
        }
    }

    fn add_right(&self, val: u8) -> SFNum {
        match self {
            SFNum::Regular(i) => SFNum::Regular(*i + val),
            SFNum::Pair(f, s) => SFNum::Pair(f.clone(), Box::new(s.add_right(val))),
        }
    }

    // n + m -> n.add(m)
    fn add(&self, val: &SFNum) -> SFNum {
        SFNum::Pair(Box::new(self.clone()), Box::new(val.clone())).reduce()
    }

    fn magnitude(&self) -> usize {
        match self {
            SFNum::Regular(i) => *i as usize,
            SFNum::Pair(f, s) => 3 * f.magnitude() + 2 * s.magnitude(),
        }
    }

    fn reduce(&self) -> SFNum {
        let mut r = self.clone();
        loop {
            let mut reducer = SFNumReducer {
                add_prev: None,
                add_next: None,
                used: false,
            };
            r = reducer.reduce(&r);
            if !reducer.used {
                break;
            }
        }
        r
    }
}

impl SFNumReducer {
    fn explode(&mut self, num: &SFNum, depth: u32) -> SFNum {
        match num {
            SFNum::Regular(i) => {
                if self.add_next.is_some() {
                    // second part of explode
                    let n = self.add_next.unwrap();
                    self.add_next = None;
                    SFNum::Regular(*i + n)
                } else {
                    SFNum::Regular(*i)
                }
            }
            SFNum::Pair(first, second) => {
                if self.used {
                    SFNum::Pair(
                        Box::new(self.explode(first, depth + 1)),
                        Box::new(self.explode(second, depth + 1)),
                    )
                } else if depth < 4 {
                    let mut f = self.explode(first, depth + 1);
                    if self.used {
                        SFNum::Pair(Box::new(f), Box::new(self.explode(second, depth + 1)))
                    } else {
                        let s = self.explode(second, depth + 1);
                        if self.used && self.add_prev.is_some() {
                            // explode to last of first
                            let n = self.add_prev.unwrap();
                            self.add_prev = None;
                            f = first.add_right(n);
                        }
                        SFNum::Pair(Box::new(f), Box::new(s))
                    }
                } else {
                    self.add_prev = match **first {
                        SFNum::Regular(i) => Some(i),
                        SFNum::Pair(_, _) => panic!("Impossible"),
                    };
                    self.add_next = match **second {
                        SFNum::Regular(i) => Some(i),
                        SFNum::Pair(_, _) => panic!("Impossible"),
                    };
                    self.used = true;
                    SFNum::Regular(0)
                }
            }
        }
    }

    fn split(&mut self, num: &SFNum) -> SFNum {
        match num {
            SFNum::Regular(i) => {
                if !self.used && *i >= 10 {
                    // split
                    let n = *i / 2;
                    self.used = true;
                    SFNum::Pair(
                        Box::new(SFNum::Regular(n)),
                        Box::new(SFNum::Regular(n + (*i % 2))),
                    )
                } else {
                    SFNum::Regular(*i)
                }
            }
            SFNum::Pair(first, second) => {
                if self.used {
                    num.clone()
                } else {
                    SFNum::Pair(Box::new(self.split(first)), Box::new(self.split(second)))
                }
            }
        }
    }

    fn reduce(&mut self, num: &SFNum) -> SFNum {
        let r = self.explode(num, 0);
        if self.used {
            r
        } else {
            self.split(num)
        }
    }
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input18"));
    let inputs = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| SFNum::parse(&l.unwrap()))
        .collect_vec();
    // println!("inputs {:?}", inputs);

    println!(
        "Part 1: {}",
        inputs
            .iter()
            .skip(1)
            .fold(inputs[0].clone(), |acc, n| acc.add(n))
            .magnitude()
    );

    println!(
        "Part 2: {}",
        inputs
            .iter()
            .combinations(2)
            .map(|v| [v[0].add(v[1]).magnitude(), v[1].add(v[0]).magnitude()])
            .flatten()
            .max()
            .unwrap()
    );
}
