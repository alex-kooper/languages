#![allow(dead_code)]

use std::collections::HashSet;
use std::iter::successors;
use rayon::prelude::*;

use crate::{point::Coordinate, polyomino::*};

type Set<T> = HashSet<T>;

pub fn generate(n: usize) -> Set<Polyomino> {
    let monomino: Polyomino = Polyomino::from([(0, 0)]);
    let domino: Polyomino = Polyomino::from([(0, 0), (1, 0)]);

    match n {
        0 => Set::new(),
        1 => Set::from([monomino]),
        2 => Set::from([domino]),
        n => successors(Some(Set::from([domino])), |polyominos| {
            Some(
                polyominos
                    .par_iter()
                    .flat_map(|p| generate_by_adding_one_point_to(p))
                    .collect(),
            )
        })
        .nth(n - 2)
        .unwrap(),
    }
}

fn generate_by_adding_one_point_to(polyomino: &Polyomino) -> Set<Polyomino> {
    const ADJACENT_POINT_DELTAS: [(Coordinate, Coordinate); 4] = [(-1, 0), (0, -1), (1, 0), (0, 1)];

    polyomino
        .iter()
        .par_bridge()
        .map(|point| {
            ADJACENT_POINT_DELTAS.par_iter().flat_map(|(dx, dy)| {
                let new_point = point.shift(*dx, *dy);

                if polyomino.contains(new_point) {
                    None
                } else {
                    Some(polyomino.add_point(new_point).to_canonical_form())
                }
            })
        })
        .flatten()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_generated_by_adding_one_point() {
        let polyomino = Polyomino::from([(0, 0), (1, 0)]).to_canonical_form();
        let polyominos: Set<Polyomino> = generate_by_adding_one_point_to(&polyomino);

        assert_eq!(polyominos.len(), 2)
    }

    #[test]
    pub fn test_generate_tetramino() {
        assert_eq!(generate(4).len(), 5)
    }
}
