#![feature(let_chains)]
#![feature(portable_simd)]
#![feature(iter_array_chunks)]
#![feature(transmutability)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use itertools::Itertools;
use rand::{thread_rng, Rng};
use simd::{i8s, SIMD_LANES};
use std::{
    fmt::Display,
    mem::{Assume, TransmuteFrom},
    ops::Index,
    simd::cmp::SimdPartialOrd,
    sync::atomic::{self, AtomicU32},
    thread,
};

mod common;
pub mod io;
mod scalar;
pub mod simd;

const THREADS: usize = 12;

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
    #[inline(always)]
    fn value(self) -> CardValue {
        // Safe because CardValue doesnt have any special invariants
        // Valid because Card always contains a valid CardValue
        unsafe {
            TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(
                self.inner & 0b0000_1111,
            )
        }
    }
    #[inline(always)]
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
    #[inline]
    fn from_i8(num: i8) -> Self {
        assert!((-4..=0).contains(&num));
        // Validity guaranteed by the above assertion
        unsafe { Self::from_i8_unchecked(num) }
    }
    #[inline]
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
impl From<Option<Color>> for OptInvertedColor {
    fn from(value: Option<Color>) -> Self {
        match value {
            Some(value) => match value {
                Color::Hearts => Self::Hearts,
                Color::Diamonds => Self::Diamonds,
                Color::Clubs => Self::Clubs,
                Color::Spades => Self::Spades,
            },
            None => Self::None,
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
    pub fn random() -> Self {
        let mut rng = thread_rng();
        let value = rng.gen_range(0..=9);

        // Validity guaranteed because of above range bounds
        unsafe { TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(value) }
    }
    pub fn from_num(num: i8) -> Self {
        assert!(num >= Self::HighCard as i8 && num <= Self::RoyalFlush as i8);

        // Safe because of the above assertion
        unsafe { Self::from_num_unchecked(num) }
    }
    /// # Safety
    /// Caller must ensure num contains a bit-valid Hand
    pub unsafe fn from_num_unchecked(num: i8) -> Self {
        unsafe { TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(num) }
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
    fn win(&mut self) {
        self.wins.fetch_add(1, atomic::Ordering::Relaxed);
    }
    fn loss(&mut self) {
        self.losses.fetch_add(1, atomic::Ordering::Relaxed);
    }
}

fn num_combinations(n: usize, r: usize) -> usize {
    // There are no combinations if r > n
    if r > n {
        0
    } else {
        // ncr(n, r) == ncr(n, n-r)
        (1..=r.min(n - r)).fold(1, |acc, val| acc * (n - val + 1) / val)
    }
}

// PLAN: For every possible pool, calculate the highest possible hand for the player, then for
// every possible other hand, look if it is higher than the player's one. Agregate the wins and
// losses to calculate the chance that one of the others has a higher hand, then raise this
// chance to the amount of other players. Average these results over every hand to get the
// final result, do this by keeping track of the current average and the count of chances, the
// on each iteration add (chance - average chance)/count to the average chance
pub fn calculate(present_cards: Option<Vec<Card>>) -> f64 {
    let mut deck = create_deck();

    let present_cards = present_cards.unwrap_or_else(|| {
        // let num = thread_rng().gen_range(4..=7);
        let mut cards = Vec::with_capacity(5);
        for _ in 0..5 {
            cards.push(Card::random());
        }
        cards
    });

    deck.retain(|deck_card| !present_cards.contains(deck_card));

    let (player_cards, pool) = present_cards.split_at(2);

    let combinations = deck
        .clone()
        .into_iter()
        // Calculate possible pools
        .combinations(5 - pool.len());

    let num_combinations = num_combinations(deck.len(), 5 - pool.len());
    let chunk_size = num_combinations / THREADS;
    // Make the number divisible by the amount of SIMD-Lanes
    let chunk_size = chunk_size - (chunk_size % SIMD_LANES);

    thread::scope(|scope| {
        // THREADS + 1 because of remainder
        for index in 0..(THREADS + 1) {
            let part = combinations
                .clone()
                .skip(index * chunk_size)
                .take(chunk_size);
            scope.spawn(|| {
                let chunks = part
                    .array_chunks::<SIMD_LANES>()
                    .map(|remaining_pools| {
                        (
                            remaining_pools,
                            remaining_pools.map(|remaining_pool| {
                                pool.iter().copied().chain(remaining_pool.clone())
                            }),
                        )
                    })
                    .map(|(remaining_pools, entire_pools)| {
                        (
                        compute_wins_losses(
                            player_combineds,
                            remaining_pools,
                            &deck,
                            entire_pools,
                            &mut wins_losses,
                        );

                for (remaining_pools, entire_pools, player_combineds) in chunks {
                    let player_hands_i8 = simd::highest_possible_hand(&mut player_combineds, None);
                    let player_hands = player_hands_i8
                        .to_array()
                        .map(|hand_i8| Hand::from_inverted(hand_i8));

                    for (i, remaining_pool) in remaining_pools.iter().enumerate() {
                        let other_combineds = deck
                            .clone()
                            .into_iter()
                            // Filter out cards already in the pool
                            .filter(|deck_card| !remaining_pool.contains(deck_card))
                            // Get possible other hand cards
                            .combinations(2)
                            .map(|other_cards| entire_pools[i].clone().chain(other_cards).collect())
                            // TODO: Handle remainder
                            .array_chunks::<SIMD_LANES>();
fn compute_wins_losses(
    mut player_combineds: [Vec<Card>; 32],
    remaining_pools: [Vec<Card>; 32],
    deck: &[Card],
    entire_pools: [std::iter::Chain<std::iter::Copied<std::slice::Iter<'_, Card>>, std::vec::IntoIter<Card>>;
        32],
    wins_losses: &mut WinsLosses,
) {
    let player_hands_i8 = simd::highest_possible_hand(&mut player_combineds, None);
    let player_hands = player_hands_i8.to_array().map(Hand::from_num);

    for (i, remaining_pool) in remaining_pools.iter().enumerate() {
        let other_combineds = deck
            .iter()
            .copied()
            // Filter out cards already in the pool
            .filter(|deck_card| !remaining_pool.contains(deck_card))
            // Get possible other hand cards
            .combinations(2)
            .map(|other_cards| entire_pools[i].clone().chain(other_cards).collect())
            // TODO: Handle remainder
            .array_chunks::<SIMD_LANES>();
        let other_combineds = other_combineds
            .clone()
            .chain(other_combineds.into_remainder().map(padd));

        for mut other_combined in other_combineds.clone() {
            let other_hand =
                simd::highest_possible_hand(&mut other_combined, Some(player_hands[i]));

            // Aggregate wins, losses and draws
            // Less than because the better the hand, the lower the value
            // TODO: Consider draws
            i8s::splat(player_hands_i8[i])
                .simd_lt(other_hand)
                .to_array()
                .into_iter()
                .for_each(|win| {
                    if win {
                        wins_losses.wins += 1;
                    } else {
                        wins_losses.losses += 1;
                    }
                });
        }
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
