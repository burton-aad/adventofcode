#![allow(non_snake_case)]

use itertools::Itertools;
use std::collections::{HashMap, VecDeque};
use std::io::{BufRead, BufReader};
use std::{env, fmt, fs};

#[derive(Default, Debug)]
struct Node {
    name: String,
    reuse: bool,
    links: Vec<usize>,
}

impl Node {
    fn make(name: String) -> Node {
        let reuse = name.chars().all(|c| c.is_uppercase());
        Node {
            name: name,
            reuse: reuse,
            links: Vec::new(),
        }
    }
}

#[derive(Default, Debug)]
struct Graph {
    nodes: Vec<Node>,
    start: usize,
    end: usize,
}

impl Graph {
    fn from_rough_map(lines: &Vec<String>) -> Graph {
        // Put start and end in graph at start
        let mut graph = Graph {
            nodes: vec![
                Node {
                    name: "start".to_string(),
                    reuse: false,
                    links: Vec::new(),
                },
                Node {
                    name: "end".to_string(),
                    reuse: false,
                    links: Vec::new(),
                },
            ],
            start: 0,
            end: 1,
        };
        let mut map: HashMap<String, usize> =
            [(String::from("start"), 0), (String::from("end"), 1)]
                .iter()
                .cloned()
                .collect();

        let mut get_node = |graph: &mut Graph, n: &str| -> usize {
            if !map.contains_key(n) {
                graph.nodes.push(Node::make(n.to_string()));
                map.insert(n.to_string(), graph.nodes.len() - 1);
            }
            map[n]
        };

        for (a, b) in lines
            .iter()
            .map(|l| l.split("-").collect_tuple().expect("2 values"))
        {
            let an = get_node(&mut graph, &a);
            let bn = get_node(&mut graph, &b);
            graph.nodes[an].links.push(bn);
            graph.nodes[bn].links.push(an);
        }

        graph
    }

    fn iter_path(&self, one_double: bool) -> GraphIterPath<'_> {
        GraphIterPath {
            graph: &self,
            one_double: one_double,
            paths: vec![vec![self.start]].into_iter().collect::<VecDeque<_>>(),
        }
    }
}

#[derive(Debug)]
struct GraphIterPath<'a> {
    graph: &'a Graph,
    one_double: bool,
    paths: VecDeque<Vec<usize>>,
}

impl<'a> Iterator for GraphIterPath<'a> {
    type Item = Vec<&'a String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut r = None;

        while !self.paths.is_empty() && r.is_none() {
            let p = self.paths.pop_front().unwrap();
            let n = &self.graph.nodes[*p.last().unwrap()];
            for s in n.links.iter() {
                let mut c = p.clone();
                c.push(*s);
                if *s == self.graph.start {
                    continue;
                } else if *s == self.graph.end {
                    r = Some(c.iter().map(|&i| &self.graph.nodes[i].name).collect());
                } else if self.graph.nodes[*s].reuse
                    || (self.one_double
                        && p.iter()
                            .filter(|&&v| !self.graph.nodes[v].reuse)
                            .counts()
                            .values()
                            .all(|&c| c == 1))
                    || !p.contains(s)
                {
                    self.paths.push_back(c);
                }
            }
        }

        r
    }
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut v = vec![true; self.nodes.len()];
        let mut q = vec![self.start].into_iter().collect::<VecDeque<_>>();
        v[self.start] = false;
        while !q.is_empty() {
            let n = &self.nodes[q.pop_front().unwrap()];
            write!(f, "{}", n.name)?;
            if !n.links.is_empty() {
                write!(f, " -- ")?;
            }
            for (i, &idx) in n.links.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.nodes[idx].name)?;
                if v[idx] {
                    q.push_back(idx);
                    v[idx] = false;
                }
            }
            write!(f, "\n")?;
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
    // println!("lines {:?}", lines);

    let graph = Graph::from_rough_map(&lines);

    // println!("graph :\n{}", graph);
    // for path in graph.iter_path(true) {
    //     println!("paths : {:?}", path);
    // }

    println!("Part 1 : {}", graph.iter_path(false).count());
    println!("Part 2 : {}", graph.iter_path(true).count());
}
