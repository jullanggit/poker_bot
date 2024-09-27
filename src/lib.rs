#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use itertools::Itertools;

use rand::{thread_rng, Rng};
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{
    io,
    mem::transmute,
    num::{ParseFloatError, ParseIntError},
    sync::atomic::{self, AtomicU32},
};
use strum_macros::EnumCount;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
struct Card {
    // First 4 bits = Value
    // Jack - Ace = 11 - 14
    //
    // Next two bits = Color
    // 00 = Hearts
    // 01 = Diamonds
    // 10 = Clubs
    // 11 = Spades
    value: u8,
}
impl TryFrom<&str> for Card {
    type Error = &'static str;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        // Split the string into Color and Value
        let card_parts: Vec<&str> = s.split(',').collect();
        if card_parts.len() != 2 {
            return Err("Invalid card format");
        }

        let value = match card_parts[0] {
            "Ace" | "14" | "a" | "A" => Ok(14),
            "2" => Ok(2),
            "3" => Ok(3),
            "4" => Ok(4),
            "5" => Ok(5),
            "6" => Ok(6),
            "7" => Ok(7),
            "8" => Ok(8),
            "9" => Ok(9),
            "10" => Ok(10),
            "Jack" | "11" | "j" | "J" => Ok(11),
            "Queen" | "12" | "q" | "Q" => Ok(12),
            "King" | "13" | "k" | "K" => Ok(13),
            // maybe add error handling
            _otherwise => Err("Failed to parse value"),
        }?;
        let color = match card_parts[1] {
            "Hearts" | "1" => Ok(1),
            "Diamonds" | "2" => Ok(2),
            "Clubs" | "3" => Ok(3),
            "Spades" | "4" => Ok(4),
            // maybe add error handling
            _otherwise => Err("Failed to parse color"),
        }?;

        Ok(Self::new(value, color))
    }
}
impl Card {
    const fn new(value: u8, color: u8) -> Self {
        Self {
            value: value + (color << 4),
        }
    }
    fn random() -> Self {
        let mut rng = thread_rng();
        let value = rng.gen_range(2..=14);
        let color = rng.gen_range(0..=3);

        Self::new(value, color)
    }
    const fn value(&self) -> u8 {
        self.value & 0b0000_1111
    }
    const fn color(&self) -> u8 {
        self.value >> 4
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy, EnumCount)]
#[repr(u8)]
enum Hand {
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
    fn lower(self) -> Self {
        let lowered_u8 = (self as u8).saturating_sub(1);

        unsafe { transmute(lowered_u8) }
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
        wins as f64 / (wins + losses.max(1)) as f64
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
            let entire_pool = pool.iter().chain(remaining_pool.iter());
            let player_combined = entire_pool.clone().chain(player_cards);
            let player_hand = highest_possible_hand(player_combined, None);
            // Calculate win percentage

            deck.iter()
                // Filter out cards already in the pool
                .filter(|deck_card| !remaining_pool.contains(deck_card))
                // Get possible other hand cards
                .combinations(2)
                .for_each(|other_cards| {
                    let other_combined = entire_pool.clone().chain(other_cards);
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

fn get_cards(interactive: bool) -> Result<Vec<Card>, &'static str> {
    if interactive {
        // get input for cards
        println!("Please input your handcards and the cards in the middle in the following format, separated by whitespace: Value,Color");
        let mut hand_buffer = String::new();
        let _ = io::stdin().read_line(&mut hand_buffer);

        // Seperate the Cards
        let cards_string: Vec<&str> = hand_buffer.split_whitespace().collect();

        if cards_string.len() > 7 {
            return Err("Please input at max 7 cards");
        }

        cards_string
            .iter()
            .map(|&string| string.try_into())
            .collect()
    } else {
        // let num = thread_rng().gen_range(4..=7);
        let mut cards = Vec::new();
        for _ in 0..5 {
            cards.push(Card::random());
        }
        Ok(cards)
    }
}

fn get_player_count(interactive: bool) -> Result<i32, ParseIntError> {
    if interactive {
        println!("Please input the amount of other players participating");
        let mut player_count_buffer = String::new();
        let _ = io::stdin().read_line(&mut player_count_buffer);

        player_count_buffer.trim_end().parse::<i32>()
    } else {
        Ok(thread_rng().gen_range(1..7))
    }
}

fn get_pot(interactive: bool) -> Result<f64, ParseFloatError> {
    if interactive {
        println!("Please input how much is currently in the pot");
        let mut input_buffer = String::new();
        let _ = io::stdin().read_line(&mut input_buffer);

        input_buffer.trim_end().parse::<f64>()
    } else {
        Ok(thread_rng().gen_range(10.0..100_000.0))
    }
}

fn get_min_max_bet(interactive: bool) -> Result<(u32, u32), ParseIntError> {
    if interactive {
        println!("Please input the minimum and maximum bet, seperated by spaces");

        let mut input_buffer = String::new();
        let _ = io::stdin().read_line(&mut input_buffer);

        let mut min_max = input_buffer.split_whitespace();

        let min = min_max.next().unwrap().parse()?;
        let max = min_max.next().unwrap().parse()?;

        Ok((min, max))
    } else {
        let min = thread_rng().gen_range(1..1000);
        let max = min + thread_rng().gen_range(1..5000);
        Ok((min, max))
    }
}

fn create_deck() -> Vec<Card> {
    let mut deck = Vec::new();
    for color in 0..=3 {
        for value in 2..=14 {
            deck.push(Card::new(value, color));
        }
    }
    deck
}

fn expected_value(win_chance: f64, pot: f64, bet: f64) -> f64 {
    win_chance.mul_add(pot + bet, -((1. - win_chance) * bet))
}

fn best_bet(win_chance: f64, pot: f64, min_bet: u32, max_bet: u32) -> (u32, f64) {
    (min_bet..=max_bet)
        .map(|bet| (bet, expected_value(win_chance, pot, bet as f64)))
        .max_by(|(_, a_ev), (_, b_ev)| a_ev.total_cmp(b_ev))
        .unwrap()
}

fn highest_possible_hand<'a, T>(input_cards: T, player_hand: Option<Hand>) -> Hand
where
    T: Iterator<Item = &'a Card>,
{
    let mut highest_hand = player_hand.map_or(Hand::HighCard, Hand::lower);
    for mut cards in input_cards.combinations(5) {
        // Sort cards by their position in SEQUENCE
        cards.sort_by_key(|card| Card::value(card));

        let is_straight = is_straight(&cards);
        let is_flush = is_flush(&cards);
        let mut is_three_of_a_kind_bool = false;
        let mut pairs = 0;

        // royal flush
        if is_straight && is_flush {
            if cards.iter().any(|card| card.value() == 13)
                && cards.iter().any(|card| card.value() == 14)
            {
                highest_hand = Hand::RoyalFlush;
                break;
            }
            highest_hand = Hand::StraightFlush;
            continue;
        } else if highest_hand > Hand::FourOfAKind
            && cards
                .iter()
                .combinations(4)
                .any(|local_cards| is_four_of_a_kind(&local_cards))
        {
            highest_hand = Hand::FourOfAKind;
            continue;
        } else if highest_hand > Hand::FullHouse
            && is_full_house(&cards, &mut is_three_of_a_kind_bool, &mut pairs)
        {
            highest_hand = Hand::FullHouse;
            continue;
        } else if highest_hand > Hand::Flush && is_flush {
            highest_hand = Hand::Flush;
            continue;
        } else if highest_hand > Hand::Straight && is_straight {
            highest_hand = Hand::Straight;
            continue;
        } else if highest_hand > Hand::ThreeOfAKind && is_three_of_a_kind_bool {
            highest_hand = Hand::ThreeOfAKind;
            continue;
        } else if highest_hand > Hand::TwoPair && pairs >= 2 {
            highest_hand = Hand::TwoPair;
            continue;
        } else if highest_hand > Hand::Pair && pairs >= 1 {
            highest_hand = Hand::Pair;
        }
    }
    highest_hand
}

const fn is_four_of_a_kind(cards: &[&&Card]) -> bool {
    cards[0].value() == cards[1].value()
        && cards[0].value() == cards[2].value()
        && cards[0].value() == cards[3].value()
}

fn is_full_house(
    cards: &[&Card],
    is_three_of_a_kind_bool: &mut bool,
    pairs_usize: &mut usize,
) -> bool {
    let mut pair_value = 0;
    let mut three_of_a_kind_value = 0;
    *pairs_usize = cards
        .iter()
        .combinations(2)
        .filter(|cards| is_pair(&cards[0..2], &mut pair_value))
        .count();
    *is_three_of_a_kind_bool = cards
        .iter()
        .combinations(3)
        .any(|cards| is_three_of_a_kind(&cards[0..3], &mut three_of_a_kind_value));
    *is_three_of_a_kind_bool && *pairs_usize > 0 && pair_value != three_of_a_kind_value
}

const fn is_flush(cards: &[&Card]) -> bool {
    cards[0].color() == cards[1].color()
        && cards[0].color() == cards[2].color()
        && cards[0].color() == cards[3].color()
        && cards[0].color() == cards[4].color()
}

const fn is_straight(cards: &[&Card]) -> bool {
    // normal case
    (cards[1].value() == cards[0].value() + 1
        && cards[2].value() == cards[0].value() + 2
        && cards[3].value() == cards[0].value() + 3
        && cards[4].value() == cards[0].value() + 4)
        // edge case Ace,2,3,4,5
        || (cards[0].value() == 2
            && cards[1].value() == 3
            && cards[2].value() == 4
            && cards[3].value() == 5
            && cards[4].value() == 14)
}

fn is_three_of_a_kind(cards: &[&&Card], value: &mut u8) -> bool {
    if cards[0].value() == cards[1].value() && cards[0].value() == cards[2].value() {
        *value = cards[0].value();
        true
    } else {
        false
    }
}

fn is_pair(cards: &[&&Card], value: &mut u8) -> bool {
    if cards[0].value() == cards[1].value() {
        *value = cards[0].value();
        true
    } else {
        false
    }
}
