use std::array;

use poker_bot::{
    simd::{self, SIMD_LANES},
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
        let _ = simd::highest_possible_hand(&mut input_cardss, Some(Hand::random()));
        // coz::progress!();
    }
}
