#![allow(non_snake_case)]

use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug)]
struct Unit {
    _type: char,
}
type Polymer = Vec<Unit>;

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

    fn t(&self) -> char {
        self._type.to_lowercase().next().unwrap()
    }

    fn reacts(&self, o: &Unit) -> bool {
        self.polarity() != o.polarity()
            && self.t() == o.t()
    }
}

fn react<'a, T: IntoIterator<Item = &'a Unit>>(v: T) -> Vec<&'a Unit> {
    let mut r : Vec<&Unit> = Vec::new();
    let n = Unit::new(None);
    let mut last = &n;

    for u in v {
        if last.reacts(u) {
            // println!("Reacts: {}{}", last._type, u._type);
            r.pop();
            last = r.last().unwrap_or(&&n);
        }
        else {
            r.push(u);
            last = u;
        }
    }

    r
}

fn jour05(s: &str) {
    let v: Polymer = s.chars().map(Unit::new).collect();

    let p1 = react(&v);
    // println!("{:?}", p1.iter().map(|ref u| {u.t()}).collect::<String>());
    println!("Part 1: {:?}", p1.iter().count());

    let alphabet = "abcdefghijklmnopqrstuvwxyz";
    let mut result = vec![0;26];
    for (i,l) in alphabet.chars().enumerate() {
        result[i] = react(v.iter().filter(|&c| { c.t() != l })).iter().count();
    }
    // println!("{:?}", result);
    let p2 = result.iter().enumerate().min_by_key(|&x|{x.1}).unwrap();
    println!("Part 2: Remove {} -> {:?}", alphabet.chars().nth(p2.0).unwrap(), p2.1);
}

fn main() -> std::io::Result<()> {
    let input = env::args().nth(1).unwrap_or(String::from("input"));
    let mut f = File::open(input)?;
	let mut s = String::new();
	f.read_to_string(&mut s)?;

    jour05(s.trim());
    Ok(())
}
