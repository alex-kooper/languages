mod generator;
mod point;
mod polyomino;

fn main() {
    for p in generator::generate(10) {
        print!("{p}")
    }
}
