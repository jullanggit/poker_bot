#![feature(let_chains)]
#![feature(portable_simd)]
#![feature(iter_array_chunks)]
#![feature(transmutability)]
#![feature(generic_const_exprs)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_slice)]
#![feature(array_try_from_fn)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use itertools::Itertools;
use rand::{Rng, thread_rng};
use simd::{SIMD_LANES, i8s, padd};
use std::{
    array,
    fmt::{Debug, Display},
    mem::{Assume, MaybeUninit, TransmuteFrom, transmute},
    ops::AddAssign,
    simd::cmp::SimdPartialOrd,
    thread,
};

pub mod io;
pub mod simd;

const THREADS: usize = 12;
const FULL_DECK_SIZE: usize = 52;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    /// # Panics
    /// - If value isnt inside of 2..=14
    /// - If color isnt inside of 0..=3
    #[must_use]
    pub fn from_tuple((value, color): (u8, u8)) -> Self {
        Self::from_num(value, color)
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
impl Debug for Card {
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
    wins: u32,
    losses: u32,
}
impl WinsLosses {
    fn percentage(self) -> f64 {
        f64::from(self.wins) / f64::from(self.wins + self.losses.max(1))
    }
}
impl AddAssign for WinsLosses {
    fn add_assign(&mut self, rhs: Self) {
        self.wins += rhs.wins;
        self.losses += rhs.losses;
    }
}

const fn num_combinations(n: usize, r: usize) -> usize {
    if r > n {
        0
    } else {
        let mut result = 1;
        let k = if r < n - r { r } else { n - r }; // Choose the smaller of r and n-r for optimization
        let mut i = 0;

        while i < k {
            result *= (n - i) / (i + 1);
            i += 1;
        }

        result
    }
}

macro_rules! match_len {
    ($present_cards:expr, $($num:expr),+) => {
        match $present_cards.len() {
            $(
                $num => {
                    let present_cards = $present_cards.try_into().unwrap();
                    calculate_inner::<{$num-2}, {7-$num}>(Some(present_cards))
                }
            ),+
            _ => unreachable!()
        }
    };
}

// PLAN: For every possible pool, calculate the highest possible hand for the player, then for
// every possible other hand, look if it is higher than the player's one. Agregate the wins and
// losses to calculate the chance that one of the others has a higher hand, then raise this
// chance to the amount of other players. Average these results over every hand to get the
// final result, do this by keeping track of the current average and the count of chances, the
// on each iteration add (chance - average chance)/count to the average chance
// TODO: Remove the need to specify remaining pool size
#[inline]
pub fn calculate(present_cards: Option<&[Card]>) -> f64 {
    match present_cards {
        Some(present_cards) => {
            match_len!(present_cards, 2, 3, 4, 5, 6, 7)
        }
        None => {
            let mut present_cards = [MaybeUninit::uninit(); 5];

            for index in 0..present_cards.len() {
                let mut card = Card::random();

                while unsafe { MaybeUninit::slice_assume_init_ref(&present_cards[0..index]) }
                    .contains(&card)
                {
                    card = Card::random();
                }
                present_cards[index].write(card);
            }

            calculate_inner::<3, 2>(Some(unsafe {
                MaybeUninit::array_assume_init(present_cards)
            }))
        }
    }
}
fn calculate_inner<const POOL_SIZE: usize, const REMAINING_POOL_SIZE: usize>(
    present_cards: Option<[Card; POOL_SIZE + 2]>,
) -> f64
where
    [(); 54 - POOL_SIZE]:,
    [(); FULL_DECK_SIZE - 2 - POOL_SIZE]:,
{
    assert!(POOL_SIZE == 5 - REMAINING_POOL_SIZE);

    let present_cards: [Card; POOL_SIZE + 2] =
        present_cards.unwrap_or_else(|| array::from_fn(|_| Card::random()));

    let deck = create_deck_without_present_cards::<POOL_SIZE>(present_cards)
        .expect("Failed to create deck without present cards");

    let player_cards = [present_cards[0], present_cards[1]];
    let pool: [Card; POOL_SIZE] = array::from_fn(|index| present_cards[index]);

    let combinations = deck
        .into_iter()
        // TODO: Use tuple_combinations here
        .combinations(5 - POOL_SIZE)
        .map(|vec| TryInto::<[Card; REMAINING_POOL_SIZE]>::try_into(vec).unwrap());

    let num_combinations: usize =
        const { num_combinations(56 - POOL_SIZE - 2, REMAINING_POOL_SIZE) };
    let chunk_size = num_combinations / THREADS;
    // Make the number divisible by the amount of SIMD-Lanes
    let chunk_size = chunk_size - (chunk_size % SIMD_LANES);

    let wins_losses = thread::scope(|scope| {
        // THREADS + 1 because of remainder
        let handles: [_; THREADS + 1] = array::from_fn(|index| {
            let part = combinations
                .clone()
                .skip(index * chunk_size)
                .take(chunk_size);
            scope.spawn({
                move || {
                    let mut wins_losses = WinsLosses::default();

                    let chunks = part.array_chunks::<SIMD_LANES>();
                    // TODO: Maybe add condition "if last_thread"
                    let chunks = chunks.clone().chain(chunks.into_remainder().map(padd));

                    for remaining_pools in chunks {
                        let entire_pools: [[Card; 5]; SIMD_LANES] =
                            remaining_pools.map(|remaining_pool| {
                                array::from_fn(|index| {
                                    if index < POOL_SIZE {
                                        pool[index]
                                    } else {
                                        remaining_pool[index - POOL_SIZE]
                                    }
                                })
                            });
                        let player_combineds: [[Card; 7]; SIMD_LANES] =
                            entire_pools.map(|entire_pool| {
                                array::from_fn(|index| {
                                    if index < 5 {
                                        entire_pool[index]
                                    } else {
                                        player_cards[index - 5]
                                    }
                                })
                            });
                        compute_wins_losses::<REMAINING_POOL_SIZE>(
                            player_combineds,
                            remaining_pools,
                            &deck,
                            entire_pools,
                            &mut wins_losses,
                        );
                    }
                    wins_losses
                }
            })
        });

        let mut wins_losses = WinsLosses::default();
        for handle in handles {
            wins_losses += handle.join().unwrap();
        }
        wins_losses
    });
    wins_losses.percentage()
}

fn compute_wins_losses<const REMAINING_POOL_SIZE: usize>(
    mut player_combineds: [[Card; 7]; SIMD_LANES],
    // TODO: Specify the length of this vec using generics
    remaining_pools: [[Card; REMAINING_POOL_SIZE]; SIMD_LANES],
    deck: &[Card],
    entire_pools: [[Card; 5]; SIMD_LANES],
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
            .tuple_combinations::<(_, _)>()
            .map(|tuple| unsafe { transmute(tuple) })
            .map(|other_cards: [Card; 2]| {
                array::from_fn(|index| {
                    if index < 2 {
                        other_cards[index]
                    } else {
                        entire_pools[i][index - 2]
                    }
                })
            })
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

/// Creates a full poker deck, without the given present cards in it.
/// Returns None, if there are any duplicates in the present cards
fn create_deck_without_present_cards<const POOL_SIZE: usize>(
    present_cards: [Card; POOL_SIZE + 2],
) -> Option<[Card; FULL_DECK_SIZE - 2 - POOL_SIZE]> {
    let mut iter = (2..=14)
        .flat_map(|value| (0..=3).map(move |color| (value, color)))
        .map(Card::from_tuple)
        .filter(|card| !present_cards.contains(card));

    let deck = array::try_from_fn(|_| iter.next());

    // If the iterator isnt used up
    if iter.next().is_some() { None } else { deck }
}
