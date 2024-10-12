use crate::{Card, CardValue, Color};
use rand::{Rng, thread_rng};
use std::{
    io,
    num::{ParseFloatError, ParseIntError},
    str::FromStr,
};

impl FromStr for Card {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Split the string into Color and Value
        let card_parts: Vec<&str> = s.split(',').collect();
        if card_parts.len() != 2 {
            return Err("Invalid card format");
        }

        let value = card_parts[0].parse()?;
        let color = card_parts[1].parse()?;

        Ok(Self::new(value, color))
    }
}

impl FromStr for CardValue {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "2" => Ok(Self::Two),
            "3" => Ok(Self::Three),
            "4" => Ok(Self::Four),
            "5" => Ok(Self::Five),
            "6" => Ok(Self::Six),
            "7" => Ok(Self::Seven),
            "8" => Ok(Self::Eight),
            "9" => Ok(Self::Nine),
            "10" => Ok(Self::Ten),
            "Jack" | "11" | "j" | "J" => Ok(Self::Jack),
            "Queen" | "12" | "q" | "Q" => Ok(Self::Queen),
            "King" | "13" | "k" | "K" => Ok(Self::King),
            "Ace" | "1" | "14" | "a" | "A" => Ok(Self::Ace),
            // maybe add error handling
            _otherwise => Err("Failed to parse value"),
        }
    }
}

impl FromStr for Color {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Hearts" | "1" => Ok(Self::Hearts),
            "Diamonds" | "2" => Ok(Self::Diamonds),
            "Clubs" | "3" => Ok(Self::Clubs),
            "Spades" | "4" => Ok(Self::Spades),
            // maybe add error handling
            _otherwise => Err("Failed to parse color"),
        }
    }
}

pub fn get_cards(interactive: bool) -> Result<Vec<Card>, &'static str> {
    if interactive {
        // get input for cards
        println!(
            "Please input your handcards and the cards in the middle in the following format, separated by whitespace: Value,Color"
        );
        let mut hand_buffer = String::new();
        let _ = io::stdin().read_line(&mut hand_buffer);

        // Seperate the Cards
        let cards_string: Vec<&str> = hand_buffer.split_whitespace().collect();

        if cards_string.len() > 7 {
            return Err("Please input at max 7 cards");
        }

        cards_string.iter().map(|&string| string.parse()).collect()
    } else {
        // let num = thread_rng().gen_range(4..=7);
        let mut cards = Vec::new();
        for _ in 0..5 {
            cards.push(Card::random());
        }
        Ok(cards)
    }
}

pub fn get_player_count(interactive: bool) -> Result<i32, ParseIntError> {
    if interactive {
        println!("Please input the amount of other players participating");
        let mut player_count_buffer = String::new();
        let _ = io::stdin().read_line(&mut player_count_buffer);

        player_count_buffer.trim_end().parse::<i32>()
    } else {
        Ok(thread_rng().gen_range(1..7))
    }
}

pub fn get_pot(interactive: bool) -> Result<f64, ParseFloatError> {
    if interactive {
        println!("Please input how much is currently in the pot");
        let mut input_buffer = String::new();
        let _ = io::stdin().read_line(&mut input_buffer);

        input_buffer.trim_end().parse::<f64>()
    } else {
        Ok(thread_rng().gen_range(10.0..100_000.0))
    }
}

pub fn get_min_max_bet(interactive: bool) -> Result<(u32, u32), ParseIntError> {
    if interactive {
        println!("Please input the minimum and maximum bet, seperated by spaces");

        let mut input_buffer = String::new();
        let _ = io::stdin().read_line(&mut input_buffer);

        let mut min_max = input_buffer.split_whitespace();

        let min = min_max.next().unwrap().parse()?;
        let max = min_max.next().unwrap().parse()?;

        Ok((min, max))
    } else {
        let min = thread_rng().gen_range(1..1000);
        let max = min + thread_rng().gen_range(1..5000);
        Ok((min, max))
    }
}
