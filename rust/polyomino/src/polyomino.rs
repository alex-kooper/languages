#![allow(dead_code)]

use im::OrdSet;
use std::fmt;
use std::iter::successors;

use crate::point::*;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Polyomino(OrdSet<Point>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dimensions {
    pub width: Coordinate,
    pub height: Coordinate,
}

impl Polyomino {
    pub fn from<const N: usize>(arr: [(Coordinate, Coordinate); N]) -> Self {
        let points = arr.map(|(x, y)| Point::new(x, y));
        Polyomino(OrdSet::from(&points[..]))
    }

    pub fn add_point(&self, p: Point) -> Self {
        Self(self.0.update(p))
    }

    pub fn contains(&self, p: Point) -> bool {
        self.0.contains(&p)
    }

    pub fn shift(&self, dx: Coordinate, dy: Coordinate) -> Self {
        self.map(|p| p.shift(dx, dy))
    }

    pub fn rotate_right_around(&self, rotation_point: Point) -> Self {
        self.map(|p| p.rotate_right_around(rotation_point))
    }

    pub fn rotate_right(&self) -> Self {
        self.rotate_right_around(Point::ORIGIN)
    }

    pub fn rotate_left_around(&self, rotation_point: Point) -> Self {
        self.map(|p| p.rotate_left_around(rotation_point))
    }

    pub fn rotate_left(&self) -> Self {
        self.rotate_left_around(Point::ORIGIN)
    }

    pub fn reflect_vertically(&self, x: Coordinate) -> Self {
        self.map(|p| p.reflect_vertically(x))
    }

    pub fn reflect_over_the_y_axis(&self) -> Self {
        self.reflect_vertically(0)
    }

    pub fn reflect_horizontally(&self, x: Coordinate) -> Self {
        self.map(|p| p.reflect_horizontally(x))
    }

    pub fn reflect_over_the_x_axis(&self) -> Self {
        self.reflect_horizontally(0)
    }

    pub fn upper_left_corner(&self) -> Point {
        let min_x = self.0.iter().map(|p| p.x).min().unwrap();
        let min_y = self.0.iter().map(|p| p.y).min().unwrap();
        Point::new(min_x, min_y)
    }

    pub fn lower_right_corner(&self) -> Point {
        let max_x = self.0.iter().map(|p| p.x).max().unwrap();
        let max_y = self.0.iter().map(|p| p.y).max().unwrap();
        Point::new(max_x, max_y)
    }

    pub fn dimensions(&self) -> Dimensions {
        let ulc = self.upper_left_corner();
        let lrc = self.lower_right_corner();

        Dimensions {
            width: lrc.x - ulc.x + 1,
            height: lrc.y - ulc.y + 1,
        }
    }

    pub fn shift_to_origin(&self) -> Self {
        let point = self.upper_left_corner();
        self.shift(-point.x, -point.y)
    }

    pub fn all_rotations(&self) -> impl Iterator<Item = Polyomino> {
        successors(Some(self.shift_to_origin()), |p| {
            Some(p.rotate_right().shift_to_origin())
        })
        .take(4)
    }

    pub fn all_congruents(&self) -> OrdSet<Polyomino> {
        self.all_rotations()
            .chain(self.reflect_over_the_x_axis().all_rotations())
            .collect()
    }

    pub fn to_canonical_form(&self) -> Polyomino {
        self.all_congruents()
            .into_iter()
            .filter(|p| {
                let d = p.dimensions();
                d.width >= d.height
            })
            .max()
            .unwrap()
    }

    fn map<F: Fn(&Point) -> Point>(&self, f: F) -> Self {
        Polyomino(self.0.iter().map(f).collect())
    }
}

impl fmt::Display for Polyomino {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let polyomino = self.shift_to_origin();
        let d = polyomino.dimensions();
        f.write_str("\n")?;

        for y in 0..d.height {
            for x in 0..d.width {
                if polyomino.contains(Point::new(x, y)) {
                    f.write_str("[]")?
                } else {
                    f.write_str("  ")?
                }
            }

            f.write_str("\n")?
        }

        fmt::Result::Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const POINT_OF_ROTATION: Point = Point::new(3, 7);

    pub fn make_polyomino() -> Polyomino {
        Polyomino::from([(0, 0), (1, 0), (2, 0), (2, 1)])
    }

    #[test]
    pub fn test_rotate_180_oposite_directions() {
        assert_eq!(
            make_polyomino()
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION),
            make_polyomino()
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
        );
    }

    #[test]
    pub fn test_rotate_360() {
        assert_eq!(
            make_polyomino()
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION),
            make_polyomino()
        );

        assert_eq!(
            make_polyomino()
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION),
            make_polyomino()
        )
    }

    #[test]
    pub fn test_dimensions() {
        assert_eq!(
            make_polyomino().dimensions(),
            Dimensions {
                width: 3,
                height: 2
            }
        );

        assert_eq!(
            make_polyomino().dimensions(),
            make_polyomino()
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION)
                .dimensions()
        )
    }

    #[test]
    pub fn test_shift() {
        let p = make_polyomino();

        assert_eq!(p.dimensions(), p.shift(3, 7).dimensions());

        assert_eq!(
            p.shift(33, 37).shift_to_origin().upper_left_corner(),
            Point::ORIGIN
        )
    }

    #[test]
    pub fn test_display() {
        assert_eq!(
            format!("{}", make_polyomino().rotate_right()),
            "
  []
  []
[][]
"
        )
    }

    #[test]
    pub fn test_add_point() {
        assert_eq!(
            make_polyomino().add_point(Point::new(2, 2)),
            Polyomino::from([(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)])
        );
    }

    #[test]
    pub fn test_canonical_form() {
        assert_eq!(
            make_polyomino().rotate_left().shift(12, 17).to_canonical_form(),
            make_polyomino().rotate_right().shift(1, 2).to_canonical_form()
        )
    }
}
