use std::ops::AddAssign;

use itertools::Itertools;

use crate::{
    common::{card_is_flush, diff_considerig_ace, StraightStuff},
    Card, CardValue, Color, Hand, OptInvertedColor,
};

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
/// Returns `HighCard` if no `Hand` >= `player_hand` is found
#[must_use]
pub fn highest_possible_hand(input_cards: &mut [Card], player_hand: Option<Hand>) -> Hand {
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
    let flush = flush.into();

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
    if flush != OptInvertedColor::None {
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
