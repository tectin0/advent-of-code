mod languages;

use languages::LANGUAGES;

fn main() {
    let random_number = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_micros()
        % 50;

    let language = LANGUAGES[random_number as usize];

    println!("Your random language is {}", language);
}
