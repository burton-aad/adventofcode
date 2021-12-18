#![allow(non_snake_case)]

#[macro_use]
extern crate lazy_static;

use itertools::Itertools;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::{env, fs};

#[derive(Debug)]
pub enum ParseError {
    CorruptedLine { expected: char, found: char },
    IncompleteLine ( String ),
}


lazy_static! {
    static ref  PAIRS: HashMap<char, char> = "([{<"
        .chars()
        .zip(")]}>".chars())
        .collect();
}

fn parse_line(line: &String) -> Result<(), ParseError> {
    let mut v = String::new();
    for c in line.chars() {
        if ")]}>".contains(c) {
            let e = PAIRS[&v.pop().unwrap()];
            if c != e {
                return Err(ParseError::CorruptedLine {
                    expected: e,
                    found: c,
                });
            }
        } else {
            v.push(c);
        }
    }

	if v.is_empty() {
		Ok(())
	}
	else {
		Err(ParseError::IncompleteLine(v))
	}
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let lines = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .filter_map(Result::ok)
        .collect_vec();

	let points1 : HashMap::<_, _> = ")]}>".chars().zip([3, 57, 1197, 25137].iter()).collect();
	println!("Part 1 : {}", lines.iter().enumerate().map(
		|(_i, l)| match parse_line(&l) {
			Err(ParseError::CorruptedLine{expected: _, found}) => {
				// println!("line {} Corrupted: expected {}, found {}", i, expected, found)
				points1[&found]
			},
			_ => &0,
		}
	).sum::<usize>());

	let points2 : HashMap::<_, _> = ")]}>".chars().zip(1..).collect();
	let scores = lines.iter().filter_map(
			|l| match parse_line(&l) {
				Err(ParseError::IncompleteLine(rest)) => {
					// println!("Incomplete line : {}", rest);
					Some(rest.chars().rev().fold(0usize, |acc, c| acc*5 + points2[&PAIRS[&c]]))
				},
				_ => None,
			}
	).sorted().collect_vec();
	println!("Part 2 : {:?}", scores[scores.len() / 2]);
}
