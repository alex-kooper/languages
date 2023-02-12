#![allow(dead_code)]

use crate::point::*;
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Polyomino(BTreeSet<Point>);

#[derive(Debug, Clone, Copy)]
pub struct Dimensions {
    pub width: Coordinate,
    pub height: Coordinate,
}

impl Polyomino {
    pub fn from<const N: usize>(arr: [(Coordinate, Coordinate); N]) -> Self {
        Polyomino(BTreeSet::from(arr.map(|(x, y)| Point::new(x, y))))
    }

    pub fn contains(self: &Self, p: Point) -> bool {
        self.0.contains(&p)
    }

    pub fn shift(self: &Self, dx: Coordinate, dy: Coordinate) -> Polyomino {
        self.map(|p| p.shift(dx, dy))
    }

    pub fn rotate_right(self: &Self, rotation_point: Point) -> Self {
        self.map(|p| p.rotate_right(rotation_point))
    }

    pub fn rotate_left(self: &Self, rotation_point: Point) -> Self {
        self.map(|p| p.rotate_left(rotation_point))
    }

    pub fn reflect_vertically(self: &Self, x: Coordinate) -> Self {
        self.map(|p| p.reflect_vertically(x))
    }

    pub fn reflect_over_the_y_axis(self: &Self) -> Self {
        self.reflect_vertically(0)
    }

    pub fn reflect_horizontally(self: &Self, x: Coordinate) -> Self {
        self.map(|p| p.reflect_horizontally(x))
    }

    pub fn reflect_over_the_x_axis(self: &Self) -> Self {
        self.reflect_horizontally(0)
    }

    pub fn upper_left_corner(self: &Self) -> Point {
        let min_x = self.0.iter().map(|p| p.x).min().unwrap();
        let min_y = self.0.iter().map(|p| p.y).min().unwrap();
        Point::new(min_x, min_y)
    }

    pub fn lower_right_corner(self: &Self) -> Point {
        let max_x = self.0.iter().map(|p| p.x).max().unwrap();
        let max_y = self.0.iter().map(|p| p.y).max().unwrap();
        Point::new(max_x, max_y)
    }

    pub fn dimensions(self: &Self) -> Dimensions {
        let ulc = self.upper_left_corner();
        let lrc = self.lower_right_corner();

        Dimensions {
            width: lrc.x - ulc.x + 1,
            height: ulc.y - lrc.y + 1,
        }
    }

    pub fn shift_to_origin(self: &Self) -> Polyomino {
        let point = self.upper_left_corner();
        self.shift(-point.x, -point.y)
    }

    fn map<F: Fn(&Point) -> Point>(self: &Self, f: F) -> Self {
        Polyomino(self.0.iter().map(f).collect())
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
                .rotate_right(POINT_OF_ROTATION)
                .rotate_right(POINT_OF_ROTATION),
            make_polyomino()
                .rotate_left(POINT_OF_ROTATION)
                .rotate_left(POINT_OF_ROTATION)
        );
    }

    #[test]
    pub fn test_rotate_360() {
        assert_eq!(
            make_polyomino()
                .rotate_right(POINT_OF_ROTATION)
                .rotate_right(POINT_OF_ROTATION)
                .rotate_right(POINT_OF_ROTATION)
                .rotate_right(POINT_OF_ROTATION),
            make_polyomino()
        );

        assert_eq!(
            make_polyomino()
                .rotate_left(POINT_OF_ROTATION)
                .rotate_left(POINT_OF_ROTATION)
                .rotate_left(POINT_OF_ROTATION)
                .rotate_left(POINT_OF_ROTATION),
            make_polyomino()
        )
    }
}
