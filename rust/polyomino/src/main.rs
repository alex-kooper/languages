use std::io::{stdin, stdout, Write};
use std::time::Instant;

mod generator;
mod point;
mod polyomino;

fn read_usize(message: &str) -> Option<usize> {
    print!("{message}");
    stdout().flush().unwrap();

    let mut input_line = String::new();
    stdin().read_line(&mut input_line).unwrap();
    let input_line = input_line.trim();

    input_line.parse().ok()
}

fn main() {
    let n = loop {
        if let Some(n) = read_usize("Enter number of cells: ") {
            break n;
        }
    };

    let start = Instant::now();
    let polyominos = generator::generate(n);
    let duration = start.elapsed();

    println!(
        "\nThere are {} free polyominoes with {n} cells.",
        polyominos.len()
    );

    println!("It took {:?} to generate them.", duration);

    let answer = read_usize(
        "\nHow many of them would you like to be printed? [Press <Enter> to print all]: ",
    );

    if let Some(n) = answer {
        for p in polyominos.iter().take(n) {
            print!("{p}")
        }
    } else {
        for p in polyominos {
            print!("{p}")
        }
    }
}
