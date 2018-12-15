#![allow(non_snake_case)]

use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug)]
struct Unit {
    _type: char,
}

impl Unit {
    fn new<T: Into<Option<char>>>(c: T) -> Unit {
        let into_c = c.into();
        Unit{
            _type: into_c.unwrap_or('\0'),
        }
    }

    fn polarity(&self) -> bool {
        self._type.is_lowercase()
    }

    fn reacts(&self, o: &Unit) -> bool {
        self.polarity() != o.polarity()
            && self._type.to_uppercase().to_string() == o._type.to_uppercase().to_string()
    }
}

fn react(v: &Vec<Unit>) -> Vec<&Unit> {
    let mut r : Vec<&Unit> = Vec::new();
    let mut last = &Unit::new(None);

    for u in v {
        if last.reacts(u) {
            // println!("Reacts: {}{}", last._type, u._type);
            r.pop();
            last = r.last().unwrap();
        }
        else {
            r.push(u);
            last = u;
        }
    }

    r
}

fn jour05(s: &str) {
    let v: Vec<Unit> = s.chars().map(Unit::new).collect();

    let p1 = react(&v);
    // println!("{:?}", p1.iter().map(|ref u| {u._type}).collect::<String>());
    println!("Part 1: {:?}", p1.iter().count());
}

fn main() -> std::io::Result<()> {
    let input = env::args().nth(1).unwrap_or(String::from("input05"));
    let mut f = File::open(input)?;
	let mut s = String::new();
	f.read_to_string(&mut s)?;

    // println!("{:?}", s.trim());
    jour05(s.trim());

    Ok(())
}
