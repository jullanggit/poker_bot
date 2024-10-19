use crate::{Card, CardValue, OptInvertedColor};

#[derive(Debug, Clone, Copy)]
pub struct StraightStuff {
    pub start: CardValue,
    pub end: CardValue,
    pub flush_counter: u8,
    pub flush_end: Option<CardValue>,
    pub unsure: bool,
}
impl From<(Card, OptInvertedColor)> for StraightStuff {
    #[inline]
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
    #[inline]
    pub fn is_straight(self) -> bool {
        diff_considerig_ace(self.end, self.start) >= 4
    }
    #[inline]
    pub const fn is_flush(self) -> bool {
        self.flush_counter >= 5
    }
}
#[inline]
pub fn card_is_flush(card: Card, flush_color: OptInvertedColor) -> bool {
    match flush_color {
        OptInvertedColor::None => false,
        _ => flush_color == card.color().into(),
    }
}
#[inline(always)]
pub fn diff_considerig_ace(a: CardValue, b: CardValue) -> u8 {
    (a as u8)
        .checked_sub(b as u8)
        // If the previous card was an ace and the current isnt, treat the ace as a 1
        .unwrap_or(a as u8 - 1)
}
