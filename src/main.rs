use std::array;

use poker_bot::{
    calculate,
    io::{best_bet, get_cards, get_min_max_bet, get_player_count, get_pot},
    simd::{self, SIMD_LANES},
    Card, Hand,
};

fn main() {
    cli();
}

fn cli() {
    let present_cards = get_cards().unwrap();
    let player_count = get_player_count().unwrap();
    let pot = get_pot().unwrap();
    let (min_bet, max_bet) = get_min_max_bet().unwrap();

    let win_chance = calculate(Some(&present_cards));
    let win_chance = win_chance.powi(player_count as i32);

    println!("Win chance: {}%", win_chance * 100.);
    let (best_bet, ev) = best_bet(win_chance, pot, min_bet, max_bet);
    println!("Best bet: {best_bet}, ev: {ev}",);
}

fn loop_highest_possible_hand() {
    for _ in 0..10000 {
        let mut input_cardss: [[Card; 7]; SIMD_LANES] = array::from_fn(|_| {
            [
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
