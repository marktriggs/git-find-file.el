// rustc -O gff-fastfilter.rs && mv gff-fastfilter $SOMEWHERE_ON_PATH

use std::env;
use std::io::{self, BufRead, Write};

fn matches_pattern(needle_chars: &Vec<u8>, haystack: &str) -> bool {
    if needle_chars.len() == 0 {
        return true;
    }

    let mut needle_pos = 0;
    let needle_len = needle_chars.len();

    for ch in haystack.bytes() {
        if needle_chars[needle_pos] == ch {
            needle_pos += 1;

            if needle_pos == needle_len {
                return true;
            }
        }
    }

    false
}

fn main() {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut out = stdout.lock();

    let newline = "\n".as_bytes();

    let patterns: Vec<Vec<u8>> = env::args().skip(1).map(|s| {
        s.to_ascii_lowercase().bytes().collect()
    }).collect();

    for line in stdin.lock().lines().map(Result::unwrap) {
        let lowercased = line.to_ascii_lowercase();

        if patterns.iter().all(|pattern| matches_pattern(&pattern, &lowercased)) {
            out.write(line.as_bytes()).expect("write");
            out.write(newline).expect("newline");
        }
    }
}
