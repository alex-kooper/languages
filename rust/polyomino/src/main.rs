mod generator;
mod point;
mod polyomino;

fn main() {
    println!("Below is the list of all possible pentaminos");
    
    for p in generator::generate(5) {
        print!("{p}")
    }
}
