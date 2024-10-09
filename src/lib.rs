#![feature(let_chains)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use io::{get_cards, get_min_max_bet, get_player_count, get_pot};
use itertools::Itertools;
use rand::{thread_rng, Rng};
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{
    fmt::Display,
    mem::transmute,
    ops::AddAssign,
    sync::atomic::{self, AtomicU32},
};
use strum_macros::EnumCount;

mod io;

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
    /// - If color isnt inside of 2..=3
    #[must_use]
    pub fn from_num(value: u8, color: u8) -> Self {
        assert!((2..=14).contains(&value));
        assert!((0..=3).contains(&color));

        unsafe { Self::from_num_unchecked(value, color) }
    }
    const unsafe fn from_num_unchecked(value: u8, color: u8) -> Self {
        Self {
            inner: value + ((color) << 4),
        }
    }
    fn random() -> Self {
        let mut rng = thread_rng();
        let value = rng.gen_range(2..=14);
        let color = rng.gen_range(0..=3);

        Self::from_num(value, color)
    }
    const fn value(self) -> CardValue {
        // Safe because of the manually set discriminants
        unsafe { transmute(self.inner & 0b0000_1111) }
    }
    const fn color(self) -> Color {
        // Safe because of the manually set discriminants
        unsafe { transmute(self.inner >> 4) }
    }
}
impl Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Color: {:?}, Value: {:?}", self.color(), self.value())
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
pub enum Color {
    Hearts = 0,
    Diamonds = 1,
    Clubs = 2,
    Spades = 3,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy, EnumCount)]
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
        .par_bridge()
        .for_each(|remaining_pool| {
            let entire_pool = pool.iter().copied().chain(remaining_pool.clone());
            let player_combined = entire_pool
                .clone()
                .chain(player_cards.iter().copied())
                .collect();
            let player_hand = highest_possible_hand(player_combined, None);
            // Calculate win percentage

            deck.clone()
                .into_iter()
                // Filter out cards already in the pool
                .filter(|deck_card| !remaining_pool.contains(deck_card))
                // Get possible other hand cards
                .combinations(2)
                .for_each(|other_cards| {
                    let other_combined = entire_pool.clone().chain(other_cards).collect();
                    let other_hand = highest_possible_hand(other_combined, Some(player_hand));

                    // Aggregate wins, losses and draws
                    if player_hand as u8 > other_hand as u8 {
                        wins_losses.wins.fetch_add(1, atomic::Ordering::Relaxed);
                    } else {
                        wins_losses.losses.fetch_add(1, atomic::Ordering::Relaxed);
                    }
                });
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

#[derive(Default)]
struct ColorsCounter {
    hearts: u8,
    diamonds: u8,
    clubs: u8,
    spades: u8,
}
impl ColorsCounter {
    const fn flush(self) -> Option<Color> {
        if self.hearts >= 5 {
            Some(Color::Hearts)
        } else if self.diamonds >= 5 {
            Some(Color::Diamonds)
        } else if self.clubs >= 5 {
            Some(Color::Clubs)
        } else if self.spades >= 5 {
            Some(Color::Spades)
        } else {
            None
        }
    }
}
impl AddAssign<Color> for ColorsCounter {
    fn add_assign(&mut self, rhs: Color) {
        match rhs {
            Color::Hearts => self.hearts += 1,
            Color::Diamonds => self.diamonds += 1,
            Color::Clubs => self.clubs += 1,
            Color::Spades => self.spades += 1,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct StraightStuff {
    start: CardValue,
    end: CardValue,
    flush_counter: u8,
    flush_end: Option<CardValue>,
    unsure: bool,
}
impl From<(Card, Option<Color>)> for StraightStuff {
    fn from((card, flush_color): (Card, Option<Color>)) -> Self {
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
fn card_is_flush(card: Card, flush_color: Option<Color>) -> bool {
    flush_color.map_or(false, |flush_color| card.color() == flush_color)
}
fn diff_considerig_ace(a: CardValue, b: CardValue) -> u8 {
    (a as u8)
        .checked_sub(b as u8)
        // If the previous card was an ace and the current isnt, treat the ace as a 1
        .unwrap_or(a as u8 - 1)
}

/// Returns `HighCard` if no `Hand` >= `player_hand` is found
#[must_use]
pub fn highest_possible_hand(mut input_cards: Vec<Card>, player_hand: Option<Hand>) -> Hand {
    assert!(input_cards.len() == 7);
    let highest_hand = player_hand.unwrap_or(Hand::HighCard);

    input_cards.sort_unstable_by_key(|card| card.value());

    let flush = input_cards
        .iter()
        .fold(ColorsCounter::default(), |mut counter, card| {
            counter += card.color();
            counter
        })
        .flush();

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
    for cur_card in straight_stuff_iter {
        let diff = diff_considerig_ace(cur_card.value(), straight_stuff.end);
        if diff == 0 {
            if straight_stuff.unsure && card_is_flush(*cur_card, flush) {
                straight_stuff.unsure = false;
            }
        // If the current card is consecutive to the end of the current straight
        } else if diff == 1 {
            straight_stuff.end = cur_card.value();

            if straight_stuff.unsure {
                straight_stuff.flush_counter = 0;
                straight_stuff.flush_end = None;
            }

            if card_is_flush(*cur_card, flush) {
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
            straight_stuff = (*cur_card, flush).into();
        }
    }
    let is_straight = straight_stuff.is_straight();

    let is_straight_flush = is_straight && straight_stuff.is_flush();

    // Royal flush
    if is_straight_flush && straight_stuff.flush_end == Some(CardValue::Ace) {
        return Hand::RoyalFlush;
    }

    // Straight Flush
    if is_straight_flush {
        return Hand::StraightFlush;
    }

    if highest_hand > Hand::FourOfAKind {
        return Hand::HighCard;
    }

    // Four of a Kind
    if input_cards.iter().tuple_windows().any(|(a, b, c, d)| {
        let (a, b, c, d) = (a.value(), b.value(), c.value(), d.value());
        a == b && a == c && a == d
    }) {
        return Hand::FourOfAKind;
    }

    if highest_hand > Hand::FullHouse {
        return Hand::HighCard;
    }

    // Up here because necessary for full house
    let is_three_of_a_kind = input_cards.iter().tuple_windows().any(|(a, b, c)| {
        let (a, b, c) = (a.value(), b.value(), c.value());
        a == b && a == c
    });

    // Up here because necessary for full house
    let pairs = input_cards
        .iter()
        .tuple_windows()
        .filter(|(a, b)| a == b)
        .count();

    // Works because four of a kind is already checked for, so one pair is in the three of a kind,
    // and the other somewhere else -> full house
    if is_three_of_a_kind && pairs >= 2 {
        return Hand::FullHouse;
    }

    if highest_hand > Hand::Flush {
        return Hand::HighCard;
    }

    // Flush
    if flush.is_some() {
        return Hand::Flush;
    }

    if highest_hand > Hand::Straight {
        return Hand::HighCard;
    }

    // Straight
    if is_straight {
        return Hand::Straight;
    }

    if highest_hand > Hand::ThreeOfAKind {
        return Hand::HighCard;
    }

    // Three of a kind
    if is_three_of_a_kind {
        return Hand::ThreeOfAKind;
    }

    if highest_hand > Hand::TwoPair {
        return Hand::HighCard;
    }

    // Two pair
    if pairs >= 2 {
        return Hand::TwoPair;
    }

    if pairs == 1 {
        return Hand::Pair;
    }

    Hand::HighCard
}
