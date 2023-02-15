#![allow(dead_code)]

use crate::{point::Coordinate, polyomino::*};

fn generate_by_adding_one_point(polyomino: &Polyomino) -> impl Iterator<Item = Polyomino> + '_ {
    const ADJACENT_POINT_DELTAS: [(Coordinate, Coordinate); 4] = [(-1, 0), (0, -1), (1, 0), (0, 1)];

    polyomino
        .iter()
        .map(|point| {
            ADJACENT_POINT_DELTAS.into_iter().flat_map(|(dx, dy)| {
                let new_point = point.shift(dx, dy);

                if polyomino.contains(new_point) {
                    None
                } else {
                    Some(polyomino.add_point(*point))
                }
            })
        })
        .flatten()
}
