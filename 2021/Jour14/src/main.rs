#![allow(non_snake_case)]

use itertools::Itertools;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::{env, fs};

// Simple version
#[allow(dead_code)]
fn step(template: &String, inj: &HashMap<String, char>) -> String {
    template
        .chars()
        .interleave(
            template
                .chars()
                .tuple_windows()
                .map(|(c, d)| inj[&format!("{}{}", c, d)]),
        )
        .collect()
}

#[allow(dead_code)]
fn score(state: &String) -> usize {
    let c = state
        .chars()
        .counts()
        .iter()
        .sorted_by_key(|x| x.1)
        .map(|(_, v)| *v)
        .collect_vec();
    c.last().unwrap() - c[0]
}

// Optimized for size
#[derive(Default, Debug)]
struct State {
    injections: HashMap<String, char>,
    letters: HashMap<char, usize>,
    pairs: HashMap<String, usize>,
}

impl State {
    fn from(template: &String, inj: &HashMap<String, char>) -> State {
        let mut st = State {
            injections: inj.clone(),
			letters: template.chars().counts(),
            pairs: Default::default(),
        };
        for s in template
            .chars()
            .tuple_windows()
            .map(|(c, d)| format!("{}{}", c, d))
        {
            *st.pairs.entry(s).or_insert(0) += 1;
        }
        st
    }

    fn step(&mut self) {
		let mut st = HashMap::<String, usize>::new();
        for (p, cnt) in self.pairs.iter()
		{
			let c = self.injections[p];
			*self.letters.entry(c).or_insert(0) += cnt;
			let (a, b) = p.chars().collect_tuple().unwrap();
            *st.entry(format!("{}{}", a, c)).or_insert(0) += cnt;
            *st.entry(format!("{}{}", c, b)).or_insert(0) += cnt;
		}
		self.pairs = st;
    }

    fn score(&self) -> usize {
		let c = self.letters.values()
			.sorted()
			.collect_vec();
		*c.last().unwrap() - c[0]
	}
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input11"));
    let mut file = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| l.unwrap());

    let template = file.next().unwrap();
    file.next();
    let injections = file
        .map(|l| {
            l.split(" -> ")
                .collect_tuple::<(_, _)>()
                .map(|(s, r)| (s.to_string(), r.chars().nth(0).unwrap()))
                .unwrap()
        })
        .collect::<HashMap<_, _>>();

    // println!("template {:?}", template);
    // println!("injections {:?}", injections);

    let mut state = State::from(&template, &injections);
    for _ in 0..10 {
        state.step();
    }
    println!("Part 1 : {:?}", state.score());

    for _ in 10..40 {
    	state.step();
    }
    println!("Part 2 : {:?}", state.score());
}
