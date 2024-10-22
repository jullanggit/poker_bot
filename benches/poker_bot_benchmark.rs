use criterion::{criterion_group, criterion_main, Criterion};
use poker_bot::{
    calculate,
    simd::{self, SIMD_LANES},
    Card, Hand,
};
use std::array;

fn bench_calculate(c: &mut Criterion) {
    c.bench_function("calculate", |b| {
        b.iter(|| calculate(None));
    });
}
fn bench_highest_possible_hand(c: &mut Criterion) {
    c.bench_function("highest_possible_hand", |b| {
        b.iter(|| {
            let mut input_cardss: [Vec<Card>; SIMD_LANES] = array::from_fn(|_| {
                vec![
                    Card::random(),
                    Card::random(),
                    Card::random(),
                    Card::random(),
                    Card::random(),
                    Card::random(),
                    Card::random(),
                ]
            });
            simd::highest_possible_hand(&mut input_cardss, Some(Hand::random()))
        });
    });
}

// Define the Criterion groups and main function
criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200);
    targets = bench_calculate, bench_highest_possible_hand
}
criterion_main!(benches);
