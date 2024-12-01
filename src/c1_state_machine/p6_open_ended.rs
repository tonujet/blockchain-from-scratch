//! Now is your chance to get creative. Choose a state machine that interests you and model it here.
//! Get as fancy as you like. The only constraint is that it should be simple enough that you can
//! realistically model it in an hour or two.
//!
//! Here are some ideas:
//! * Board games:
//!   * Chess
//!   * Checkers
//!   * Tic tac toe
//! * Beaurocracies:
//!   * Beauro of Motor Vehicles - maintains driving licenses and vehicle registrations.
//!   * Public Utility Provider - Customers open accounts, consume the utility, pay their bill periodically, maybe utility prices fluctuate
//!   * Land ownership registry
//! * Tokenomics:
//!   * Token Curated Registry
//!   * Prediction Market
//!   * There's a game where there's a prize to be split among players and the prize grows over time. Any player can stop it at any point and take most of the prize for themselves.
//! * Social Systems:
//!   * Social Graph
//!   * Web of Trust
//!   * Reputation System

use super::StateMachine;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use GameState::*;
use TicTacToeUnit::*;
use Transition::*;

#[derive(Default, Debug, PartialEq, Clone)]
pub enum TicTacToeUnit {
    O(usize, usize),
    X(usize, usize),
    #[default]
    Blank,
}

impl AsRef<TicTacToeUnit> for TicTacToeUnit {
    fn as_ref(&self) -> &TicTacToeUnit {
        self
    }
}

pub enum Transition {
    Move(TicTacToeUnit),
    Restart,
}

impl Display for TicTacToeUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            O(_, _) => "o",
            X(_, _) => "x",
            Blank => "_",
        };
        write!(f, "{symbol}")
    }
}

#[derive(Debug, PartialEq, Clone)]
enum GameState {
    Playing(TicTacToeUnit),
    Finished(TicTacToeUnit),
}

impl GameState {
    fn finish(&self) -> Self {
        match self {
            Playing(unit) | Finished(unit) => Finished(unit.to_owned()),
        }
    }
}

impl Default for GameState {
    fn default() -> Self {
        Playing(Blank)
    }
}

// TODO make last_unit as a ref to the value in the board
#[derive(Default, Debug, PartialEq, Clone)]
pub struct TicTacToe {
    board: [[TicTacToeUnit; 3]; 3],
    game_state: GameState,
}

impl TicTacToe {
    fn is_winning_line<T: AsRef<TicTacToeUnit>>(&self, line: &[T; 3]) -> bool {
        mem::discriminant(line[0].as_ref()) == mem::discriminant(line[1].as_ref())
            && mem::discriminant(line[1].as_ref()) == mem::discriminant(line[2].as_ref())
            && !matches!(*line[0].as_ref(), Blank)
    }

    fn try_assign_winning_unit<T: AsRef<TicTacToeUnit>>(&self, line: &[T; 3], winning_unit: &mut Option<TicTacToeUnit>) -> bool {
        let is_winning = self.is_winning_line(line);
        if let Some(Playing(unit) | Finished(unit)) = Some(&self.game_state).filter(|_| is_winning)
        {
            let _ = winning_unit.insert(unit.to_owned());
            return true;
        }
        false
    }
    
    
    fn apply_finish_unit(mut self, unit: TicTacToeUnit) -> Self {
        self.game_state = Finished(unit);
        self
    }

    fn finish_game_if_possible(mut self) -> Self {
        let mut finish_unit = None;

        for row in &self.board {
            if self.try_assign_winning_unit(row, &mut finish_unit) {
                break
            }
        }
        if finish_unit.is_some() {
            return self.apply_finish_unit(finish_unit.unwrap())    
        }

        for i in 0..3 {
            let col = [&self.board[0][i], &self.board[1][i], &self.board[2][i]];
            if self.try_assign_winning_unit(&col, &mut finish_unit) {
                break
            }
        }
        if finish_unit.is_some() {
            return self.apply_finish_unit(finish_unit.unwrap())
        }


        let left_diagonal = [&self.board[0][0], &self.board[1][1], &self.board[2][2]];
        if self.try_assign_winning_unit(&left_diagonal, &mut finish_unit) {
            return self.apply_finish_unit(finish_unit.unwrap())
        }

        let right_diagonal = [&self.board[0][2], &self.board[1][1], &self.board[2][0]];
        if self.try_assign_winning_unit(&right_diagonal, &mut finish_unit) {
            return self.apply_finish_unit(finish_unit.unwrap())
        }

        let is_board_filled = self
            .board
            .iter()
            .flatten()
            .any(|unit| matches!(unit, Blank));
        if !is_board_filled {
            return self.apply_finish_unit(Blank);
        }
        
        self
    }
}

impl Display for TicTacToe {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut board = String::new();
        for row in &self.board {
            for item in row {
                board.push_str(item.to_string().as_str());
                board.push_str(" ");
            }
            board.push_str("\n");
        }

        write!(f, "{board}")
    }
}

impl StateMachine for TicTacToe {
    type State = Self;
    type Transition = Transition;

    fn next_state(starting: &Self::State, t: &Self::Transition) -> Self::State {
        let move_unit = match t {
            Move(unit) => unit,
            Restart => return Self::State::default(),
        };

        let validated = match (move_unit, &starting.game_state) {
            (_, Finished(_)) => false,
            (O(_, _), Playing(O(_, _))) | (X(_, _), Playing(X(_, _))) | (Blank, _) => false,
            (O(y, x), _) | (X(y, x), _) => matches!(&starting.board[*y][*x], Blank),
            _ => true,
        };
        let mut next_state = starting.to_owned();

        if !validated {
            return next_state;
        }

        next_state.game_state = Playing(move_unit.clone());
        if let O(y, x) | X(y, x) = move_unit {
            next_state.board[*y][*x] = move_unit.clone()
        }
        
        next_state.finish_game_if_possible()
    }
}

#[test]
fn make_first_move() {
    let mut expected_game = TicTacToe {
        board: Default::default(),
        game_state: Playing(X(0, 0)),
    };
    expected_game.board[0][0] = X(0, 0);

    let game = TicTacToe::next_state(&TicTacToe::default(), &Move(X(0, 0)));

    assert_eq!(game, expected_game);
}

#[test]
fn make_second_valid_move() {
    let mut expected_game = TicTacToe {
        board: Default::default(),
        game_state: Playing(O(1, 0)),
    };
    expected_game.board[0][0] = X(0, 0);
    expected_game.board[1][0] = O(1, 0);

    let game = TicTacToe::next_state(&TicTacToe::default(), &Move(X(0, 0)));
    let game = TicTacToe::next_state(&game, &Move(O(1, 0)));

    assert_eq!(game, expected_game);
}

#[test]
fn make_second_move_in_the_same_cell_fails() {
    let mut expected_game = TicTacToe {
        board: Default::default(),
        game_state: Playing(X(0, 0)),
    };

    expected_game.board[0][0] = X(0, 0);

    let game = TicTacToe::next_state(&TicTacToe::default(), &Move(X(0, 0)));
    let game = TicTacToe::next_state(&game, &Move(O(0, 0)));

    assert_eq!(game, expected_game);
}

#[test]
fn make_second_move_with_the_same_unit_fails() {
    let mut expected_game = TicTacToe {
        board: Default::default(),
        game_state: Playing(X(0, 0)),
    };

    expected_game.board[0][0] = X(0, 0);

    let game = TicTacToe::next_state(&TicTacToe::default(), &Move(X(0, 0)));
    let game = TicTacToe::next_state(&game, &Move(X(1, 0)));

    assert_eq!(game, expected_game);
}

#[test]
fn use_restart_game() {
    let expected_game = TicTacToe::default();

    let game = TicTacToe {
        board: [
            [O(0, 0), X(0, 1), X(0, 2)],
            [Blank, O(1, 1), Blank],
            [Blank, Blank, Blank],
        ],
        game_state: Playing(O(1, 1)),
    };

    let game = TicTacToe::next_state(&game, &Restart);
    assert_eq!(game, expected_game);
}

#[test]
fn win_game_for_x_diagonally() {
    let expected_game = TicTacToe {
        board: [
            [X(0, 0), O(0, 1), X(0, 2)],
            [O(1, 0), X(1, 1), O(1, 2)],
            [X(2, 0), Blank, Blank],
        ],
        game_state: Finished(X(2, 0)),
    };

    let game = TicTacToe {
        board: [
            [X(0, 0), O(0, 1), X(0, 2)],
            [O(1, 0), X(1, 1), O(1, 2)],
            [Blank, Blank, Blank],
        ],
        game_state: Playing(O(1, 2)),
    };

    let game = TicTacToe::next_state(&game, &Move(X(2, 0)));
    assert_eq!(game, expected_game);
}

#[test]
fn win_game_for_x_vertically() {
    let expected_game = TicTacToe {
        board: [
            [Blank, O(0, 1), X(0, 2)],
            [Blank, O(1, 1), X(1, 2)],
            [Blank, Blank, X(2, 2)],
        ],
        game_state: Finished(X(2, 2)),
    };

    let game = TicTacToe {
        board: [
            [Blank, O(0, 1), X(0, 2)],
            [Blank, O(1, 1), X(1, 2)],
            [Blank, Blank, Blank],
        ],
        game_state: Playing(O(1, 1)),
    };

    let game = TicTacToe::next_state(&game, &Move(X(2, 2)));
    assert_eq!(game, expected_game);
}

#[test]
fn win_game_for_o_horizontally() {
    let expected_game = TicTacToe {
        board: [
            [Blank, X(0, 1), X(0, 2)],
            [O(1, 0), O(1, 1), O(1, 2)],
            [Blank, Blank, X(2, 2)],
        ],
        game_state: Finished(O(1, 2)),
    };

    let game = TicTacToe {
        board: [
            [Blank, X(0, 1), X(0, 2)],
            [O(1, 0), O(1, 1), Blank],
            [Blank, Blank, X(2, 2)],
        ],
        game_state: Playing(X(0, 2)),
    };

    let game = TicTacToe::next_state(&game, &Move(O(1, 2)));
    assert_eq!(game, expected_game);
}

#[test]
fn draw_game() {
    let expected_game = TicTacToe {
        board: [
            [O(0, 0), X(0, 1), O(0, 2)],
            [O(1, 0), X(1, 1), X(1, 2)],
            [X(2, 0), O(2, 1), O(2, 2)],
        ],
        game_state: Finished(Blank),
    };

    let game = TicTacToe {
        board: [
            [O(0, 0), X(0, 1), O(0, 2)],
            [O(1, 0), X(1, 1), X(1, 2)],
            [X(2, 0), O(2, 1), Blank],
        ],
        game_state: Playing(X(1, 2)),
    };

    let game = TicTacToe::next_state(&game, &Move(O(2, 2)));
    assert_eq!(game, expected_game);
}
