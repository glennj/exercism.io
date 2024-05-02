import gleam/result

pub type Player {
  Black
  White
}

pub type Game {
  Game(
    white_captured_stones: Int,
    black_captured_stones: Int,
    player: Player,
    error: String,
  )
}

pub fn apply_rules(
  game: Game,
  rule1: fn(Game) -> Result(Game, String),
  rule2: fn(Game) -> Game,
  rule3: fn(Game) -> Result(Game, String),
  rule4: fn(Game) -> Result(Game, String),
) -> Game {
  let final_result =
    Ok(game)
    |> result.try(rule1)
    |> result.map(rule2)
    |> result.try(rule3)
    |> result.try(rule4)

  case final_result {
    Ok(g) -> next_player(g)   // all rules satisfied
    Error(e) -> Game(..game, error: e)
  }
}

fn next_player(game: Game) -> Game {
  case game.player {
    Black -> Game(..game, player: White)
    White -> Game(..game, player: Black)
  }
}
