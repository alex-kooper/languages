#![allow(dead_code)]

pub type Coordinate = i16;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct Point {
    pub x: Coordinate,
    pub y: Coordinate,
}

impl Point {
    pub const ORIGIN: Point = Point::new(0, 0);

    pub const fn new(x: Coordinate, y: Coordinate) -> Self {
        Point { x, y }
    }

    pub fn shift(self, dx: Coordinate, dy: Coordinate) -> Self {
        Self::new(self.x + dx, self.y + dy)
    }

    pub fn rotate_right_around(self, rotation_point: Point) -> Self {
        Point {
            x: -(self.y - rotation_point.y) + rotation_point.x,
            y: (self.x - rotation_point.x) + rotation_point.y,
        }
    }

    pub fn rotate_left_around(self, rotation_point: Point) -> Self {
        Point {
            x: (self.y - rotation_point.y) + rotation_point.x,
            y: -(self.x - rotation_point.x) + rotation_point.y,
        }
    }

    pub fn reflect_vertically(self, x: Coordinate) -> Self {
        Point {
            x: 2 * x - self.x,
            y: self.y,
        }
    }

    pub fn reflect_horizontally(self, y: Coordinate) -> Self {
        Point {
            x: self.x,
            y: 2 * y - self.y,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const POINT: Point = Point::new(3, 7);
    const POINT_OF_ROTATION: Point = Point::new(-1, -3);

    #[test]
    pub fn test_one_rotation() {
        assert_ne!(POINT.rotate_right_around(POINT_OF_ROTATION), POINT);
        assert_ne!(POINT.rotate_left_around(POINT_OF_ROTATION), POINT);
    }

    #[test]
    pub fn test_rotate_90_oposite_directions() {
        assert_eq!(
            POINT
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION),
            POINT
        );

        assert_eq!(
            POINT
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION),
            POINT
        )
    }

    #[test]
    pub fn test_rotate_180_oposite_directions() {
        assert_eq!(
            POINT
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION),
            POINT
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
        );
    }

    #[test]
    pub fn test_rotate_360() {
        assert_eq!(
            POINT
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION)
                .rotate_right_around(POINT_OF_ROTATION),
            POINT
        );

        assert_eq!(
            POINT
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION)
                .rotate_left_around(POINT_OF_ROTATION),
            POINT
        )
    }

    #[test]
    pub fn test_reflection() {
        assert_eq!(POINT.reflect_vertically(7).reflect_vertically(7), POINT);
        assert_eq!(POINT.reflect_horizontally(7).reflect_horizontally(7), POINT)
    }
}
