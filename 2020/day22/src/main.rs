use std::collections::{HashSet, VecDeque};

use day22::aoclib::*;

fn main() {
    println!("Score of the winning Player: {}", solve1("input"));
    println!(
        "Score of the winning Player recursively: {}",
        solve2("input")
    );
}

#[derive(Debug, Clone)]
struct Player {
    deck: VecDeque<u64>,
    name: String,
}

impl Player {
    fn from<'a, T>(lines: &mut T) -> Self
    where
        T: Iterator<Item = &'a str>,
    {
        let name = String::from(lines.next().unwrap().trim());
        let deck: VecDeque<_> = lines
            .take_while(|x| x.trim() != "")
            .map(|x| x.parse::<u64>().unwrap())
            .collect();
        Player { deck, name }
    }

    fn play_card(&mut self) -> u64 {
        self.deck.pop_front().unwrap()
    }

    fn has_lost(&self) -> bool {
        self.deck.len() == 0
    }

    fn score(&self) -> u64 {
        self.deck
            .iter()
            .rev()
            .enumerate()
            .map(|(x, y)| (x + 1) as u64 * y)
            .sum()
    }

    fn push_cards(&mut self, c1: u64, c2: u64) {
        self.deck.push_back(c1);
        self.deck.push_back(c2);
    }

    fn clone_first(&self, cards: u64) -> Player {
        let new_deck = self.deck.iter().cloned().take(cards as usize).collect();
        Player {
            name: self.name.clone(),
            deck: new_deck,
        }
    }
}
struct RecursiveGame {
    p1: Player,
    p2: Player,
    //history_p1: HashSet<VecDeque<u64>>,
    //history_p2: HashSet<VecDeque<u64>>,
}

impl RecursiveGame {
    fn from(p1: Player, p2: Player) -> RecursiveGame {
        //let mut history_p1 = HashSet::new();
        //let mut history_p2 = HashSet::new();

        RecursiveGame {
            p1: p1,
            p2: p2,
            // history_p1,
            // history_p2,
        }
    }

    fn initial_play_round(&mut self) -> Player {
        self.play_round(&mut self.p1.clone(), &mut self.p2.clone())
    }

    fn play_round(&mut self, p1: &mut Player, p2: &mut Player) -> Player {
        let mut p1_history: HashSet<VecDeque<u64>> = HashSet::new();
        let mut p2_history: HashSet<VecDeque<u64>> = HashSet::new();
        loop {
            if p1_history.contains(&p1.deck) || p2_history.contains(&p2.deck) {
                break p1.clone();
            }
            p1_history.insert(p1.deck.clone());
            p2_history.insert(p2.deck.clone());

            let card_p1 = p1.play_card();
            let card_p2 = p2.play_card();
            if p1.deck.len() >= card_p1 as usize && p2.deck.len() >= card_p2 as usize {
                let mut new_p1 = p1.clone_first(card_p1);
                let mut new_p2 = p2.clone_first(card_p2);
                let winner = self.play_round(&mut new_p1, &mut new_p2);
                if winner.name == p1.name {
                    p1.push_cards(card_p1, card_p2);
                } else {
                    p2.push_cards(card_p2, card_p1);
                }
            } else {
                if card_p1 < card_p2 {
                    // P2 wins the round
                    p2.push_cards(card_p2, card_p1);
                } else {
                    // P1 wins the round
                    p1.push_cards(card_p1, card_p2);
                }
            }

            if p1.has_lost() {
                break p2.clone();
            } else if p2.has_lost() {
                break p1.clone();
            }
        }
    }
}

struct Game {
    p1: Player,
    p2: Player,
}

impl Game {
    fn from(p1: Player, p2: Player) -> Game {
        Game { p1, p2 }
    }

    fn play_round(&mut self) -> Option<&Player> {
        let p1_card = self.p1.play_card();
        let p2_card = self.p2.play_card();
        if p1_card < p2_card {
            // P2 wins the round
            self.p2.push_cards(p2_card, p1_card);
        } else {
            // P1 wins the round
            self.p1.push_cards(p1_card, p2_card);
        }

        if self.p1.has_lost() {
            Some(&self.p2)
        } else if self.p2.has_lost() {
            Some(&self.p1)
        } else {
            None
        }
    }
}

fn solve1(fname: &str) -> u64 {
    let decks = read_file(fname).unwrap();
    let mut decks = decks.lines();
    let mut game = Game::from(Player::from(&mut decks), Player::from(&mut decks));

    loop {
        if let Some(p) = game.play_round() {
            println!("Player {} wins!", p.name);
            break p.score();
        }
    }
}

fn solve2(fname: &str) -> u64 {
    let decks = read_file(fname).unwrap();
    let mut decks = decks.lines();
    let mut rec_game = RecursiveGame::from(Player::from(&mut decks), Player::from(&mut decks));

    rec_game.initial_play_round().score()
}

#[test]
fn test_player_from() {
    let decks = read_file("test").unwrap();
    let mut decks = decks.lines();
    let p1 = Player::from(&mut decks);
    let p2 = Player::from(&mut decks);

    assert_eq!(p1.deck, VecDeque::from(vec![9, 2, 6, 3, 1]));
    assert_eq!(p2.deck, VecDeque::from(vec![5, 8, 4, 7, 10]));
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 306);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test"), 291);
}
