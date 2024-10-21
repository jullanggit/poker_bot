use crate::{
    common::{card_is_flush, diff_considerig_ace, StraightStuff},
    Card, CardValue, Hand, OptInvertedColor,
};
use itertools::Itertools;
use std::{
    array,
    ops::{Index, IndexMut},
    simd::{
        cmp::{SimdOrd, SimdPartialEq, SimdPartialOrd},
        Mask, Simd,
    },
};

pub const SIMD_LANES: usize = 32;

#[allow(non_camel_case_types)]
type u8s = Simd<u8, SIMD_LANES>;

#[allow(non_camel_case_types)]
pub type i8s = Simd<i8, SIMD_LANES>;

/// Returns `HighCard` if no `Hand` >= `player_hand` is found
/// TODO: See if accepting impl Iterator<Item = Vec<Card>> would be faster
#[must_use]
pub fn highest_possible_hand(input_cardss: &mut [Vec<Card>], player_hand: Option<Hand>) -> i8s {
    assert!(input_cardss.len() == SIMD_LANES);
    assert!(input_cardss
        .iter()
        .all(|input_cards| input_cards.len() == 7));

    let highest_hand = player_hand.unwrap_or(Hand::HighCard);

    input_cardss
        .iter_mut()
        .for_each(|input_cards| input_cards.sort_unstable_by_key(|card| card.value()));

    let simd_cardss: [u8s; 7] =
        array::from_fn(|x| array::from_fn(|y| input_cardss[y][x].inner).into());

    // Vector containing only values
    let simd_valuess = simd_cardss.map(|simd_cards| simd_cards & u8s::splat(0b0000_1111));

    // Vector containing only colors
    let simd_colorss = simd_cardss.map(|simd_cards| simd_cards >> u8s::splat(4));

    let mut final_hand = i8s::splat(0);

    // Flush
    let mut colors_counters = [i8s::splat(0); 4];
    simd_colorss.iter().for_each(|simd_colors| {
        for i in 0..4 {
            colors_counters[i as usize] += simd_colors.simd_eq(u8s::splat(i)).to_int();
        }
    });

    let flush_threshold = i8s::splat(-5);
    let flush: i8s = (0..4)
        .map(|i| {
            colors_counters[i as usize]
                .simd_le(flush_threshold)
                .to_int()
                * i8s::splat(i + 1)
        })
        .sum();

    let mut is_straight: Mask<i8, SIMD_LANES> = Mask::splat(false);
    for (index, input_cards) in input_cardss.iter().enumerate() {
        let flush = OptInvertedColor::from_i8(*flush.index(index));

        let mut straight_stuff_iter = input_cards
            .iter()
            .rev()
            .take_while(|card| card.value() == CardValue::Ace)
            .chain(input_cards.iter());

        let mut straight_stuff: StraightStuff = (
            *straight_stuff_iter
                .next()
                .expect("Iterator always has at least 7 elements"),
            flush,
        )
            .into();

        // TODO: See if into_iter is faster
        for cur_card in straight_stuff_iter.copied() {
            let diff = diff_considerig_ace(cur_card.value(), straight_stuff.end);
            if diff == 0 {
                if straight_stuff.unsure && card_is_flush(cur_card, flush) {
                    straight_stuff.unsure = false;
                }
            // If the current card is consecutive to the end of the current straight
            } else if diff == 1 {
                straight_stuff.end = cur_card.value();

                if straight_stuff.unsure {
                    straight_stuff.flush_counter = 0;
                    straight_stuff.flush_end = None;
                }

                if card_is_flush(cur_card, flush) {
                    straight_stuff.flush_counter += 1;
                    straight_stuff.flush_end = Some(cur_card.value());
                // Set unsure because maybe theres another card of the same value but with the right suit
                // Dont set unsure if there already is a straight flush
                } else if straight_stuff.flush_counter < 5 {
                    straight_stuff.unsure = true;
                }
            } else if diff > 1 {
                if straight_stuff.is_straight() {
                    break;
                }
                straight_stuff = (cur_card, flush).into();
            }
        }
        let is_straight_local = straight_stuff.is_straight();

        if is_straight_local {
            // TODO: set_unchecked should be possible
            is_straight.set(index, true);
        }

        let is_straight_flush = is_straight_local && straight_stuff.is_flush();

        // Royal flush
        if is_straight_flush && straight_stuff.flush_end == Some(CardValue::Ace) {
            // No .min() necessary because RoyalFlush is the best possible hand
            *final_hand.index_mut(index) = -(Hand::RoyalFlush as i8)
        // Straight Flush
        } else if is_straight_flush {
            // Also, no .min() necessary because straight flush is the best possible hand at this point
            *final_hand.index_mut(index) = -(Hand::StraightFlush as i8)
        }
    }

    // Four of a Kind
    // TODO: See if using a .to_bitmask is better
    let is_four_of_a_kind = simd_valuess
        .iter()
        .tuple_windows()
        .fold(Mask::splat(false), |mask, (a, b, c, d)| {
            mask | (a.simd_eq(*b) & a.simd_eq(*c) & a.simd_eq(*d))
        });

    final_hand =
        final_hand.simd_min(is_four_of_a_kind.to_int() * i8s::splat(Hand::FourOfAKind as i8));

    if highest_hand >= Hand::FourOfAKind {
        return final_hand;
    }

    // // Up here because necessary for full house
    // TODO: See if using a .to_bitmask is better
    let is_three_of_a_kind = simd_valuess
        .iter()
        .tuple_windows()
        .fold(Mask::splat(false), |mask, (a, b, c)| {
            mask | (a.simd_eq(*b) & a.simd_eq(*c))
        });

    // Up here because necessary for full house
    // Values inverted (2 pairs => -2)
    let pairs = simd_valuess
        .iter()
        .tuple_windows()
        .fold(i8s::splat(0), |acc, (a, b)| acc + a.simd_eq(*b).to_int());

    // Works because four of a kind is already checked for, so one pair is in the three of a kind,
    // and the other somewhere else -> full house
    // TODO: See if using a .to_bitmask is better
    let is_full_house = is_three_of_a_kind & pairs.simd_le(i8s::splat(-3));

    final_hand = final_hand.simd_min(is_full_house.to_int() * i8s::splat(Hand::FullHouse as i8));

    if highest_hand >= Hand::FullHouse {
        return final_hand;
    }

    // Flush
    final_hand =
        final_hand.simd_min(flush.simd_ne(i8s::splat(0)).to_int() * i8s::splat(Hand::Flush as i8));

    if highest_hand >= Hand::Flush {
        return final_hand;
    }

    // Straight
    final_hand = final_hand.simd_min(is_straight.to_int() * i8s::splat(Hand::Straight as i8));

    if highest_hand >= Hand::Straight {
        return final_hand;
    }

    // Three of a kind
    final_hand =
        final_hand.simd_min(is_three_of_a_kind.to_int() * i8s::splat(Hand::ThreeOfAKind as i8));

    if highest_hand >= Hand::ThreeOfAKind {
        return final_hand;
    }

    // Two pair
    final_hand = final_hand
        .simd_min(pairs.simd_le(i8s::splat(-2)).to_int() * i8s::splat(Hand::TwoPair as i8));

    if highest_hand >= Hand::TwoPair {
        return final_hand;
    }

    // Pair
    final_hand =
        final_hand.simd_min(pairs.simd_eq(i8s::splat(-1)).to_int() * i8s::splat(Hand::Pair as i8));

    final_hand
}
