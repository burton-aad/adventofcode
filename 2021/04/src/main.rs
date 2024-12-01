#![allow(non_snake_case)]

use std::io::{BufRead, BufReader};
use std::{env, fmt, fs};
use colored::*;

struct BingoCard {
    size: usize,
    nums: Vec<u32>,
    card: Vec<Option<u32>>,
}

impl BingoCard {
    fn from_lines(v: &[String]) -> BingoCard {
        let mut b = BingoCard {
            size: v.len(),
            nums: Vec::with_capacity(v.len() * v.len()),
            card: Vec::new(),
        };
        for l in v.iter().map(|l| {
            l.split_whitespace()
                .map(|v| v.trim().parse::<u32>().unwrap())
                .collect::<Vec<u32>>()
        }) {
            if l.len() != b.size {
                panic!("Non square board");
            }
            b.nums.extend(l);
        }
        b.reset();
		b
    }

	fn reset(&mut self) {
		self.card = self.nums.iter().map(|&i| Some(i)).collect();
	}

	fn draw(&mut self, value: u32) {
		if let Some(index) = self.card.iter().position(|v| *v == Some(value)) {
			self.card[index] = None;
		}
	}

	fn is_win(&self) -> bool {
		(0..self.size).any(|i| { (0..self.size).all(|j| self.card[i*self.size + j] == None)
								  || (0..self.size).all(|j| self.card[i + j*self.size] == None) })
	}

	fn sum_unmarked(&self) -> u32 {
		self.card.iter().map(|x| x.unwrap_or(0)).sum()
	}
}

impl fmt::Display for BingoCard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, (v, c)) in self.nums.iter().zip(self.card.iter()).enumerate() {
			let m = i % 5;
			if m > 0 { write!(f, " ")?; }
			match c {
				None => write!(f, "{}", format!("{:2}", v).bold())?,
				Some(_) => write!(f, "{:2}", v)?
			}
            if m == 4 { write!(f, "\n")?; }
        }
        Ok(())
    }
}

fn game<'a>(cards: &'a mut Vec<BingoCard>, nums: &Vec<u32>) -> (usize, u32)
{
	let mut index = None;
	let mut draw = 0;
	'outer: for d in nums.iter() {
		for (i, card) in cards.iter_mut().enumerate() {
			card.draw(*d);
			// if i % 5 == 4 { println!("{}", card); }
			if card.is_win() {
				index = Some(i);
				draw = *d;
				break 'outer;
			}
		}
	}
	match index {
		None => panic!("No Winner!"),
		Some(i) => return (i, draw)
	}
}

fn main() {
    let input = env::args().nth(1).expect("Usage: run <input>");
    let mut f = BufReader::new(fs::File::open(input).unwrap());
    let mut buf = String::new();

    f.read_line(&mut buf).expect("Cannot read line");
    let nums: Vec<u32> = buf
        .split(",")
        .map(|s| s.trim().parse::<u32>().unwrap())
        .collect();
    // println!("nums {:?}", nums);

    f.read_line(&mut buf).expect("Empty line");

    let mut cards = Vec::new();
    for lines in f
        .lines()
        .map(|l| l.unwrap())
        .filter(|l| !l.is_empty())
        .collect::<Vec<String>>()
        .chunks(5)
    {
        cards.push(BingoCard::from_lines(lines));
    }

	let (win_idx, draw) = game(&mut cards, &nums);
    println!("part 1 : {}\n{}", draw, cards[win_idx]);
	println!(" => {}", cards[win_idx].sum_unmarked() * draw);

	cards.remove(win_idx);
	while !cards.is_empty() {
		let (win_idx, draw) = game(&mut cards, &nums);
		if cards.len() == 1 {
			println!("part 2 : {}\n{}", draw, cards[win_idx]);
			println!(" => {}", cards[win_idx].sum_unmarked() * draw);
		}
		cards.remove(win_idx);
	}
}
