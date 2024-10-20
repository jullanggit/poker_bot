use criterion::{criterion_group, criterion_main, Criterion};
use poker_bot::{calculate, highest_possible_hand, Card, Hand};
use seq_macro::seq;

fn bench_calculate(c: &mut Criterion) {
    c.bench_function("calculate", |b| {
        b.iter(|| calculate(false));
    });
}
fn bench_highest_possible_hand(c: &mut Criterion) {
    c.bench_function("highest_possible_hand", |b| {
        b.iter(|| {
            seq!(_ in 0..7 {
                let input_cardss = vec!(
                    #(
                        Card::random(),
                    )*
                );
            });
            highest_possible_hand(input_cardss, Some(Hand::random()))
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
