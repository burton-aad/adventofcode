#![allow(non_snake_case)]

use itertools::Itertools;
use std::io::{BufRead, BufReader};
use std::{env, fs};

#[derive(Default, Debug)]
struct Entry {
    signal: Vec<String>,
    values: Vec<String>,
    ival: Option<i32>,
}

fn translate(s: &str, trans: &Vec<char>) -> String {
    s.chars().map(|c| trans[(c as usize) - 97]).sorted().collect()
}

impl Entry {
    fn from_line(line: &String) -> Entry {
        let mut e: Entry = Default::default();

        let (sig, val) = line.split(" | ").collect_tuple().expect("2 values");

        for s in sig.split(" ") {
            e.signal.push(s.to_string());
        }
        assert!(e.signal.len() == 10);

        for s in val.split(" ") {
            e.values.push(s.to_string());
        }
        assert!(e.values.len() == 4);

        e
    }

    fn _solve(&self) -> i32 {
        // ref :
        //    aaaa
        //   b    c   0 : abcefg (6)  5 : abdfg (5)
        //   b    c   1 : cf (2)      6 : abdefg (6)
        //    dddd    2 : acdeg (5)   7 : acf (3)
        //   e    f   3 : acdfg (5)   8 : abcdefg (7)
        //   e    f   4 : bcdf (4)    9 : abcdfg (6)
        //    gggg
        let nums = [
            "abcdefg", "abcdfg", "abcefg", "abdefg", "abdfg", "acdeg", "acdfg", "acf", "bcdf", "cf",
        ];
        let nums_val = [8, 9, 0, 6, 5, 2, 3, 7, 4, 1];
        println!("solving {:?}", self);

        for trans in "abcdefg".chars().permutations(7) {
            // println!("trans {:?}", trans);
            let tsig = self.signal.iter()
                .map(|s| translate(s, &trans))
                .sorted()
                .collect::<Vec<_>>();
            // println!("tsig {:?}", tsig);
            if tsig == nums {
                return self.values.iter()
                    .map(|s| {
                        nums_val[nums.iter()
                            .position(|&n| n == translate(s, &trans))
                            .unwrap()]
                    })
                    .fold(0, |acc, it| acc * 10 + it);
            }
        }

        assert!(false, "Not possible");
        0
    }

    fn solve(&mut self) -> i32 {
		if self.ival.is_none() {
			self.ival = Some(self._solve());
		}
		self.ival.unwrap()
	}
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input04"));
    let f = BufReader::new(fs::File::open(input).unwrap());

    let mut entries: Vec<Entry> = f.lines().map(|l| Entry::from_line(&l.unwrap())).collect();
    // for e in entries.iter() {
    //     println!("entry {:?}", e);
    // }

    let c: usize = entries
        .iter()
        .map(|ent| {
            ent.values.iter()
                .filter(|v| v.len() == 2 || v.len() == 4 || v.len() == 3 || v.len() == 7)
                .count()
        })
        .sum();
    println!("Part 1 : {}", c);
    println!("Part 2 : {}", entries.iter_mut().map(|e| e.solve()).sum::<i32>());
}
