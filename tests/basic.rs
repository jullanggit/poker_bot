use insta::assert_debug_snapshot;
use poker_bot::{highest_possible_hand, Card};

#[test]
fn test_high_card() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(4, 3),
        Card::from_num(10, 2),
        Card::from_num(6, 2),
        Card::from_num(7, 1),
        Card::from_num(8, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"HighCard");
}
#[test]
fn test_pair() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(4, 3),
        Card::from_num(10, 2),
        Card::from_num(10, 2),
        Card::from_num(7, 1),
        Card::from_num(8, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"Pair");
}
#[test]
fn test_two_pair() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(4, 3),
        Card::from_num(4, 3),
        Card::from_num(10, 2),
        Card::from_num(10, 2),
        Card::from_num(7, 1),
        Card::from_num(8, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"TwoPair");
}
#[test]
fn test_three_of_a_kind() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(10, 3),
        Card::from_num(10, 2),
        Card::from_num(10, 2),
        Card::from_num(7, 1),
        Card::from_num(8, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"ThreeOfAKind");
}
#[test]
fn test_straight() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(4, 3),
        Card::from_num(5, 2),
        Card::from_num(6, 2),
        Card::from_num(8, 1),
        Card::from_num(9, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"Straight");
}
#[test]
fn test_flush() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(4, 3),
        Card::from_num(5, 3),
        Card::from_num(7, 3),
        Card::from_num(8, 1),
        Card::from_num(9, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"Flush");
}
#[test]
fn test_four_of_a_kind() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(4, 3),
        Card::from_num(4, 3),
        Card::from_num(4, 2),
        Card::from_num(4, 2),
        Card::from_num(8, 1),
        Card::from_num(9, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"FourOfAKind");
}
#[test]
fn test_straight_flush() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(4, 3),
        Card::from_num(5, 3),
        Card::from_num(6, 3),
        Card::from_num(8, 1),
        Card::from_num(9, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"StraightFlush");
}
#[test]
fn test_royal_flush() {
    let input_cards = vec![
        Card::from_num(2, 3),
        Card::from_num(3, 3),
        Card::from_num(10, 1),
        Card::from_num(11, 1),
        Card::from_num(12, 1),
        Card::from_num(13, 1),
        Card::from_num(14, 1),
    ];
    assert_debug_snapshot!(highest_possible_hand(input_cards, None), @"RoyalFlush");
}
