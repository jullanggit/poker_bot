#![feature(portable_simd)]

use insta::assert_debug_snapshot;
use poker_bot::{
    simd::{self, SIMD_LANES},
    Card, Hand,
};
use std::{array, ops::Index};

macro_rules! hand_test {
    ($name:ident, $(($value:expr, $suit:expr)),+, $expected_output:literal) => {
        #[test]
        fn $name() {
            let mut input_cards: [[Card; 7]; SIMD_LANES] = array::from_fn(|_| {
                [
                    $(Card::from_num($value, $suit)),+
                ]
            });
            let highest_possible_hands = simd::highest_possible_hand(&mut input_cards, None);
            let highest_possible_hand =  Hand::from_num(*highest_possible_hands.index(0)) ;
            assert_debug_snapshot!(highest_possible_hand, @$expected_output);
        }
    };
}
hand_test!(
    high_card,
    (2, 3),
    (3, 3),
    (4, 3),
    (10, 2),
    (6, 2),
    (7, 1),
    (8, 1),
    "HighCard"
);

hand_test!(
    pair,
    (2, 3),
    (3, 3),
    (4, 3),
    (10, 2),
    (10, 2),
    (7, 1),
    (8, 1),
    "Pair"
);

hand_test!(
    two_pair,
    (2, 3),
    (4, 3),
    (4, 3),
    (10, 2),
    (10, 2),
    (7, 1),
    (8, 1),
    "TwoPair"
);

hand_test!(
    three_of_a_kind,
    (2, 3),
    (3, 3),
    (10, 3),
    (10, 2),
    (10, 2),
    (7, 1),
    (8, 1),
    "ThreeOfAKind"
);

hand_test!(
    straight,
    (2, 3),
    (3, 3),
    (4, 3),
    (5, 2),
    (6, 2),
    (8, 1),
    (9, 1),
    "Straight"
);

hand_test!(
    straight_starting_with_ace,
    (14, 2),
    (2, 3),
    (3, 3),
    (4, 3),
    (5, 2),
    (8, 1),
    (9, 1),
    "Straight"
);

hand_test!(
    straight_starting_with_ace_at_the_end,
    (2, 3),
    (3, 3),
    (4, 3),
    (5, 2),
    (8, 1),
    (9, 1),
    (14, 2),
    "Straight"
);

hand_test!(
    flush,
    (2, 3),
    (3, 3),
    (4, 3),
    (5, 3),
    (7, 3),
    (8, 1),
    (9, 1),
    "Flush"
);

hand_test!(
    full_house,
    (2, 3),
    (2, 3),
    (2, 3),
    (4, 2),
    (5, 2),
    (9, 1),
    (9, 1),
    "FullHouse"
);

hand_test!(
    four_of_a_kind,
    (2, 3),
    (4, 3),
    (4, 3),
    (4, 2),
    (4, 2),
    (8, 1),
    (9, 1),
    "FourOfAKind"
);

hand_test!(
    straight_flush,
    (2, 3),
    (3, 3),
    (4, 3),
    (5, 3),
    (6, 3),
    (8, 1),
    (9, 1),
    "StraightFlush"
);

hand_test!(
    royal_flush,
    (2, 3),
    (3, 3),
    (10, 1),
    (11, 1),
    (12, 1),
    (13, 1),
    (14, 1),
    "RoyalFlush"
);
