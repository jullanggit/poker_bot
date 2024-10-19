use std::array;

use poker_bot::{
    simd::{simd_highest_possible_hand, SIMD_LANES},
    Card, Hand,
};

fn main() {
    for _ in 0..10000 {
        let mut input_cardss: [Vec<Card>; SIMD_LANES] = array::from_fn(|_| {
            vec![
                Card::random(),
                Card::random(),
                Card::random(),
                Card::random(),
                Card::random(),
                Card::random(),
                Card::random(),
            ]
        });
        let _ = simd_highest_possible_hand(&mut input_cardss, Some(Hand::random()));
        // coz::progress!();
    }
}
