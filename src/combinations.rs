use std::array;

use crate::Card;

struct CardCombinations<const N: usize, const K: usize> {
    indices: [usize; K],
    cards: [Card; N],
}
impl<const N: usize, const K: usize> CardCombinations<N, K> {
    fn new(cards: [Card; N]) -> Self {
        assert!(K <= N);
        Self {
            indices: array::from_fn(|index| index),
            cards,
        }
    }
    fn increment_indices(&mut self) {
        // For each index
        for index in (0..K).rev() {
            // See if it can be incremented
            if self.indices[index] != index + N - K {
                self.indices[index] += 1;

                // All indices to the right are at the maximum allowed value, so reset them
                for right_index in index + 1..K {
                    self.indices[right_index] = self.indices[right_index - 1] + 1;
                }

                break;
            }
        }
    }
}
impl<const N: usize, const K: usize> Iterator for CardCombinations<N, K> {
    type Item = [Card; K];
    fn next(&mut self) -> Option<Self::Item> {}
}
