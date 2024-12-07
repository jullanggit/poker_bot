use std::array;

use crate::Card;

#[derive(Clone)]
pub struct CardCombinations<'a, const N: usize, const K: usize> {
    indices: [usize; K],
    cards: &'a [Card; N],
    first: bool,
}
impl<'a, const N: usize, const K: usize> CardCombinations<'a, N, K> {
    pub fn new(cards: &'a [Card; N]) -> Self {
        assert!(K <= N);

        Self {
            indices: array::from_fn(|index| index),
            cards,
            first: true,
        }
    }
    fn get_current_combination(&self) -> [Card; K] {
        array::from_fn(|index| self.cards[self.indices[index]])
    }
}
impl<'a, const N: usize, const K: usize> Iterator for CardCombinations<'_, N, K> {
    type Item = [Card; K];
    fn next(&mut self) -> Option<Self::Item> {
        if self.first {
            self.first = false;
            return Some(self.get_current_combination());
        }

        // Increment the indices
        let mut indices_index = K;

        // Decrement indices index to find one that isnt at its maximum allowed value
        while self.indices[indices_index] == indices_index + N - K {
            if indices_index > 0 {
                indices_index -= 1;
            } else {
                // Last combination reached
                return None;
            }
        }
        // Increment the found index
        self.indices[indices_index] += 1;
        // And reset the ones to its right
        for right_index in indices_index + 1..K {
            self.indices[right_index] = self.indices[right_index - 1] + 1;
        }

        // Return the current combination
        Some(self.get_current_combination())
    }
}
