#![allow(dead_code)]

use crate::{point::Coordinate, polyomino::*};

fn generate_by_adding_one_point_to(polyomino: &Polyomino) -> impl Iterator<Item = Polyomino> + '_ {
    const ADJACENT_POINT_DELTAS: [(Coordinate, Coordinate); 4] = [(-1, 0), (0, -1), (1, 0), (0, 1)];

    polyomino
        .iter()
        .map(|point| {
            ADJACENT_POINT_DELTAS.into_iter().flat_map(|(dx, dy)| {
                let new_point = point.shift(dx, dy);

                if polyomino.contains(new_point) {
                    None
                } else {
                    Some(polyomino.add_point(new_point).to_canonical_form())
                }
            })
        })
        .flatten()
}

#[cfg(test)]
mod tests {
    use im::OrdSet;
    use super::*;

    #[test]
    pub fn test_generated_by_adding_one_point() {
        let polyomino = Polyomino::from([(0, 0), (1, 0)]).to_canonical_form();
        let polyominos: OrdSet<Polyomino> = generate_by_adding_one_point_to(&polyomino).collect();
        
        assert_eq!(polyominos.len(), 2)
    }

}
