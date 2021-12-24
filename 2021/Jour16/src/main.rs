#![allow(non_snake_case)]

use itertools::Itertools;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::io::{BufRead, BufReader};
use std::{env, fs};

mod bits;

#[derive(Debug)]
enum PacketValue {
    Literal(usize),
    Operator { subs: Vec<Packet> },
}

#[derive(Debug, FromPrimitive)]
enum PacketId {
    Sum,
    Product,
    Minimum,
    Maximum,
    Literal,
    GreaterThan,
    LessThan,
    EqualTo,
}

#[derive(Debug)]
struct Packet {
    version: u8,
    id: PacketId,
    val: PacketValue,
}

impl Packet {
    fn parse_literal(bits: &mut bits::Bits) -> usize {
        let mut r = 0usize;
        while bits.take(1) == 1 {
            r = (r << 4) + bits.take(4);
        }
        // Last part to add
        (r << 4) + bits.take(4)
    }

    fn parse_sub_operator(bits: &mut bits::Bits) -> Vec<Packet> {
        let mut r = Vec::new();
        if bits.take(1) == 0 {
            let size = bits.take(15);
            let start = bits.pos();
            while bits.pos() < start + size {
                r.push(Packet::parse_bits(bits));
            }
        } else {
            // vec![Packet::parse_bits(bits); num]
            for _ in 0..bits.take(11) {
                r.push(Packet::parse_bits(bits));
            }
        }
        r
    }

    fn parse_bits(bits: &mut bits::Bits) -> Packet {
        let version = bits.take(3) as u8;
        let id: PacketId = FromPrimitive::from_usize(bits.take(3)).unwrap();
        match id {
            PacketId::Literal => Packet {
                version: version,
                id: id,
                val: PacketValue::Literal(Packet::parse_literal(bits)),
            },
            _ => Packet {
                version: version,
                id: id,
                val: PacketValue::Operator {
                    subs: Packet::parse_sub_operator(bits),
                },
            },
        }
    }

    fn from_entry(entry: &Vec<u8>) -> Packet {
        Packet::parse_bits(&mut bits::from_entry(&entry))
    }

    fn calc(&self) -> usize {
        match &self.val {
            PacketValue::Literal(v) => *v,
            PacketValue::Operator { subs: v } => match self.id {
                PacketId::Sum => v.iter().map(|p| p.calc()).sum::<usize>(),
                PacketId::Product => v.iter().map(|p| p.calc()).product::<usize>(),
                PacketId::Minimum => v.iter().map(|p| p.calc()).min().unwrap(),
                PacketId::Maximum => v.iter().map(|p| p.calc()).max().unwrap(),
                PacketId::GreaterThan => {
                    assert!(v.len() == 2);
                    (v[0].calc() > v[1].calc()) as usize
                }
                PacketId::LessThan => {
                    assert!(v.len() == 2);
                    (v[0].calc() < v[1].calc()) as usize
                }
                PacketId::EqualTo => {
                    assert!(v.len() == 2);
                    (v[0].calc() == v[1].calc()) as usize
                }
                _ => panic!("Impossible"),
            },
        }
    }
}

fn sum_version(packet: &Packet) -> usize {
    match &packet.val {
        PacketValue::Literal(_) => packet.version as usize,
        PacketValue::Operator { subs: v } => {
            packet.version as usize + v.iter().map(|p| sum_version(p)).sum::<usize>()
        }
    }
}

fn main() {
    let input = env::args().nth(1).unwrap_or(String::from("input16"));
    let line = BufReader::new(fs::File::open(input).unwrap())
        .lines()
        .map(|l| l.unwrap())
        .join("");
    // println!("line : {:?}", line);

    let entry = line
        .chars()
        .chunks(2)
        .into_iter()
        .map(|c| c.collect::<String>())
        .map(|x| u8::from_str_radix(&x, 16).unwrap())
        .collect_vec();
    // println!("entry : {:?}", entry);

    let packet = Packet::from_entry(&entry);
    // println!("parsing : {:?}", packet);
    println!("Part 1 : {:?}", sum_version(&packet));
    println!("Part 2 : {:?}", packet.calc());
}
