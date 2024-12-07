use insta::assert_debug_snapshot;
use poker_bot::{Card, CardValue, Color, combinations::CardCombinations};

#[test]
fn combinations() {
    // Some random cards
    let cards = [
        Card::new(CardValue::Two, Color::Diamonds),
        Card::new(CardValue::Jack, Color::Clubs),
        Card::new(CardValue::Ace, Color::Spades),
    ];
    let combinations: Vec<_> = CardCombinations::<3, 2>::new(&cards).collect();

    assert_debug_snapshot!(combinations);
}
