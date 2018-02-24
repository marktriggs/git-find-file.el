// rustc -O gff-fastfilter.rs && mv gff-fastfilter $SOMEWHERE_ON_PATH

use std::env;
use std::io::{self, BufRead, Write};

fn matches_pattern(needle: &str, haystack: &str) -> bool {
    if needle == "" {
        return true;
    }

    let mut needle_pos = 0;
    let needle_len = needle.len();

    for ch in haystack.chars() {
        if needle.chars().nth(needle_pos).unwrap() == ch {
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

    let patterns: Vec<String> = env::args().skip(1).map(|s| s.to_ascii_lowercase()).collect();

    for line in stdin.lock().lines().map(Result::unwrap) {
        let lowercased = line.to_ascii_lowercase();

        if patterns.iter().all(|pattern| matches_pattern(&pattern, &lowercased)) {
            out.write(line.as_bytes()).expect("write");
            out.write(newline).expect("newline");
        }
    }
}
