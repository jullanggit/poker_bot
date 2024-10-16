#![feature(let_chains)]
#![feature(portable_simd)]
#![feature(iter_array_chunks)]
#![feature(transmutability)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use array_macro::array;
use io::{get_cards, get_min_max_bet, get_player_count, get_pot};
use itertools::Itertools;
use rand::{thread_rng, Rng};
use rayon::iter::{ParallelBridge, ParallelIterator};
use seq_macro::seq;
use std::{
    fmt::Display,
    mem::{Assume, TransmuteFrom},
    ops::{Index, IndexMut},
    simd::{
        cmp::{SimdOrd, SimdPartialEq, SimdPartialOrd},
        Mask, Simd,
    },
    sync::atomic::{self, AtomicU32},
};

mod io;

pub const SIMD_LANES: usize = 32;

#[allow(non_camel_case_types)]
type u8s = Simd<u8, SIMD_LANES>;

#[allow(non_camel_case_types)]
type i8s = Simd<i8, SIMD_LANES>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Card {
    // First 4 bits = Value
    // Jack - Ace = 11 - 14
    //
    // Next two bits = Color
    // 00 = Hearts
    // 01 = Diamonds
    // 10 = Clubs
    // 11 = Spades
    inner: u8,
}
impl Card {
    #[must_use]
    pub const fn new(value: CardValue, color: Color) -> Self {
        // Safe because of conversion to u8 from valid values
        unsafe { Self::from_num_unchecked(value as u8, color as u8) }
    }
    /// # Panics
    /// - If value isnt inside of 2..=14
    /// - If color isnt inside of 0..=3
    #[must_use]
    pub fn from_num(value: u8, color: u8) -> Self {
        assert!((2..=14).contains(&value));
        assert!((0..=3).contains(&color));

        unsafe { Self::from_num_unchecked(value, color) }
    }
    /// Only valid if value is inside of 2..=14 and color is inside 0..=3
    const unsafe fn from_num_unchecked(value: u8, color: u8) -> Self {
        Self {
            inner: value + ((color) << 4),
        }
    }
    pub fn random() -> Self {
        let mut rng = thread_rng();
        let value = rng.gen_range(2..=14);
        let color = rng.gen_range(0..=3);

        Self::from_num(value, color)
    }
    fn value(self) -> CardValue {
        // Safe because CardValue doesnt have any special invariants
        // Valid because Card always contains a valid CardValue
        unsafe {
            TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(
                self.inner & 0b0000_1111,
            )
        }
    }
    fn color(self) -> Color {
        // Safe because Color doesnt have any special invariants
        // Valid because Card always contains a valid Color
        unsafe {
            TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(self.inner >> 4)
        }
    }
}
impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}, {:?}", self.color(), self.value())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum CardValue {
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
    Ten = 10,
    Jack = 11,
    Queen = 12,
    King = 13,
    Ace = 14,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[repr(u8)]
pub enum Color {
    Hearts = 0,
    Diamonds = 1,
    Clubs = 2,
    Spades = 3,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
#[repr(i8)]
pub enum OptInvertedColor {
    None = 0,
    Hearts = -1,
    Diamonds = -2,
    Clubs = -3,
    Spades = -4,
}
impl OptInvertedColor {
    fn from_i8(num: i8) -> Self {
        assert!((-4..=0).contains(&num));
        // Validity guaranteed by the above assertion
        unsafe { Self::from_i8_unchecked(num) }
    }
    /// Caller must guarantee that the i8 contains a bit-valid OptInvertedColor
    unsafe fn from_i8_unchecked(num: i8) -> Self {
        // Safety guaranteed because InvertedColor doesnt have any special invariants
        // Validity guaranteed by caller
        unsafe { TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(num) }
    }
}
impl From<Color> for OptInvertedColor {
    fn from(value: Color) -> Self {
        match value {
            Color::Hearts => Self::Hearts,
            Color::Diamonds => Self::Diamonds,
            Color::Clubs => Self::Clubs,
            Color::Spades => Self::Spades,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum Hand {
    RoyalFlush = 9,
    StraightFlush = 8,
    FourOfAKind = 7,
    FullHouse = 6,
    Flush = 5,
    Straight = 4,
    ThreeOfAKind = 3,
    TwoPair = 2,
    Pair = 1,
    HighCard = 0,
}
impl Hand {
    /// Converts an inverted Hand value (from highest_possible_hand), for example -8
    /// # Safety
    /// Caller must ensure validity of passed num (must be in -9..0)
    pub unsafe fn from_inverted(num: i8) -> Self {
        // Safe because Hand doesnt have any special invariants
        // Validity ensured by caller
        unsafe {
            TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(num.abs())
        }
    }

    pub fn random() -> Self {
        let mut rng = thread_rng();
        let value = rng.gen_range(0..=9);

        // Validity guaranteed because of above range bounds
        unsafe { TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(value) }
    }
}

#[derive(Default, Debug)]
struct WinsLosses {
    wins: AtomicU32,
    losses: AtomicU32,
}
impl WinsLosses {
    fn percentage(self) -> f64 {
        let wins = self.wins.load(atomic::Ordering::Acquire);
        let losses = self.losses.load(atomic::Ordering::Acquire);
        f64::from(wins) / f64::from(wins + losses.max(1))
    }
}

// PLAN: For every possible pool, calculate the highest possible hand for the player, then for
// every possible other hand, look if it is higher than the player's one. Agregate the wins and
// losses to calculate the chance that one of the others has a higher hand, then raise this
// chance to the amount of other players. Average these results over every hand to get the
// final result, do this by keeping track of the current average and the count of chances, the
// on each iteration add (chance - average chance)/count to the average chance
pub fn calculate(interactive: bool) {
    let mut deck = create_deck();

    let present_cards = get_cards(interactive).unwrap();

    let player_amount = get_player_count(interactive).unwrap();
    let pot = get_pot(interactive).unwrap();
    let (min_bet, max_bet) = get_min_max_bet(interactive).unwrap();

    // Remove the present cards from the deck
    deck.retain(|deck_card| !present_cards.contains(deck_card));

    let (player_cards, pool) = present_cards.split_at(2);

    let wins_losses = WinsLosses::default();
    deck.clone()
        .into_iter()
        // Calculate possible pools
        .combinations(5 - pool.len())
        // TODO: Handle remainder
        .array_chunks::<SIMD_LANES>()
        .par_bridge()
        .for_each(|remaining_pools| {
            let entire_pools = remaining_pools
                .iter()
                .map(|remaining_pool| pool.iter().copied().chain(remaining_pool.clone()));
            // let entire_pool = pool.iter().copied().chain(remaining_pool.clone());
            let mut player_combined = entire_pools
                .clone()
                .map(|entire_pool| {
                    entire_pool
                        .chain(player_cards.iter().copied())
                        .collect_vec()
                })
                .collect_vec();
            let player_hands = highest_possible_hand(&mut player_combined, None);
            // Calculate win percentage

            for (i, remaining_pool) in remaining_pools.iter().enumerate() {
                deck.clone()
                    .into_iter()
                    // Filter out cards already in the pool
                    .filter(|deck_card| !remaining_pool.contains(deck_card))
                    // Get possible other hand cards
                    .combinations(2)
                    .map(|other_cards| {
                        entire_pools
                            .clone()
                            .nth(i)
                            .expect("remaining_pools and entire_pools have the same length")
                            .clone()
                            .chain(other_cards)
                            .collect()
                    })
                    // TODO: Handle remainder
                    .array_chunks::<SIMD_LANES>()
                    .for_each(|mut other_combined| {
                        let player_hand_i8 = player_hands.index(i);

                        debug_assert!(
                            *player_hand_i8 <= -(Hand::HighCard as i8)
                                && *player_hand_i8 >= -(Hand::RoyalFlush as i8)
                        );
                        // Valid because values returnes from highest_possible_hand are just
                        // inverted Hand values, so highest_possible_hand(...).abs() is always a
                        // valid hand
                        let player_hand = unsafe { Hand::from_inverted(*player_hand_i8) };

                        let other_hand =
                            highest_possible_hand(&mut other_combined, Some(player_hand));

                        // Aggregate wins, losses and draws
                        // Less than because the better the hand, the lower the value
                        // TODO: Consider draws
                        i8s::splat(*player_hand_i8)
                            .simd_lt(other_hand)
                            .to_array()
                            .into_iter()
                            .for_each(|win| {
                                if win {
                                    wins_losses.wins.fetch_add(1, atomic::Ordering::Relaxed);
                                } else {
                                    wins_losses.losses.fetch_add(1, atomic::Ordering::Relaxed);
                                }
                            });
                    });
            }
        });
    let win_chance = wins_losses.percentage().powi(player_amount);

    if interactive {
        println!("Win chance: {}%", win_chance * 100.);
        let (best_bet, ev) = best_bet(win_chance, pot, min_bet, max_bet);
        println!("Best bet: {best_bet}, ev: {ev}",);
    }
}

fn create_deck() -> Vec<Card> {
    let mut deck = Vec::new();
    for color in 0..=3 {
        for value in 2..=14 {
            // Safe because of above loop bounds
            deck.push(Card::from_num(value, color));
        }
    }
    deck
}

fn expected_value(win_chance: f64, pot: f64, bet: f64) -> f64 {
    win_chance.mul_add(pot + bet, -((1. - win_chance) * bet))
}

fn best_bet(win_chance: f64, pot: f64, min_bet: u32, max_bet: u32) -> (u32, f64) {
    (min_bet..=max_bet)
        .map(|bet| (bet, expected_value(win_chance, pot, f64::from(bet))))
        .max_by(|(_, a_ev), (_, b_ev)| a_ev.total_cmp(b_ev))
        .unwrap()
}

#[derive(Debug, Clone, Copy)]
struct StraightStuff {
    start: CardValue,
    end: CardValue,
    flush_counter: u8,
    flush_end: Option<CardValue>,
    unsure: bool,
}
impl From<(Card, OptInvertedColor)> for StraightStuff {
    fn from((card, flush_color): (Card, OptInvertedColor)) -> Self {
        let card_is_flush = card_is_flush(card, flush_color);

        Self {
            start: card.value(),
            end: card.value(),
            flush_counter: u8::from(card_is_flush),
            flush_end: if card_is_flush {
                Some(card.value())
            } else {
                None
            },
            unsure: !card_is_flush,
        }
    }
}
impl StraightStuff {
    fn is_straight(self) -> bool {
        diff_considerig_ace(self.end, self.start) >= 4
    }
    const fn is_flush(self) -> bool {
        self.flush_counter >= 5
    }
}
fn card_is_flush(card: Card, flush_color: OptInvertedColor) -> bool {
    match flush_color {
        OptInvertedColor::None => false,
        _ => flush_color == card.color().into(),
    }
}
fn diff_considerig_ace(a: CardValue, b: CardValue) -> u8 {
    (a as u8)
        .checked_sub(b as u8)
        // If the previous card was an ace and the current isnt, treat the ace as a 1
        .unwrap_or(a as u8 - 1)
}

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
        array![x => array![y => input_cardss[y][x].inner; SIMD_LANES].into();7];

    // Vector containing only values
    let simd_valuess = simd_cardss.map(|simd_cards| simd_cards & u8s::splat(0b0000_1111));

    // Vector containing only colors
    let simd_colorss = simd_cardss.map(|simd_cards| simd_cards >> u8s::splat(4));

    let mut final_hand = i8s::splat(0);

    // Flush
    let mut colors_counters = [i8s::splat(0); 4];
    simd_colorss.iter().for_each(|simd_colors| {
        seq!(i in 0..4 {
            colors_counters[i] += simd_colors
                .simd_eq(u8s::splat(i))
                .to_int();
        });
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
