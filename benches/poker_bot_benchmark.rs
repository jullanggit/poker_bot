use criterion::{criterion_group, criterion_main, Criterion};
use poker_bot::calculate;

// Define the benchmark function
fn benchmark_deck_evaluation(c: &mut Criterion) {
    c.bench_function("deck_evaluation", |b| {
        b.iter(|| calculate(false));
    });
}

// Define the Criterion groups and main function
criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(200);
    targets = benchmark_deck_evaluation
}
criterion_main!(benches);
