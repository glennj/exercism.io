win_masks = {
  tonumber('111000000', 2),
  tonumber('000111000', 2),
  tonumber('000000111', 2),
  tonumber('100100100', 2),
  tonumber('010010010', 2),
  tonumber('001001001', 2),
  tonumber('100010001', 2),
  tonumber('001010100', 2)
}

{
  gamestate: (board) ->
    cells = table.concat board
    num = X: 0, O: 0, ' ': 0
    num[c] += 1 for c in cells\gmatch '.'
    
    value = (player) ->
      val = cells\gsub '.', (c) -> c == player and '1' or '0'
      tonumber val, 2

    values = {c, value(c) for c in *{'X', 'O'}}

    wins = X: false, O: false
    for mask in *win_masks
      for player in *{'X', 'O'}
        if values[player] & mask == mask
          wins[player] = true 

    assert not (wins.X and wins.O), 'Impossible board: game should have ended after the game was won'
    assert num.X <= num.O + 1, 'Wrong turn order: X went twice'
    assert num.O <= num.X, 'Wrong turn order: O started'

    if wins.X or wins.O then 'win'
    elseif num.X + num.O == 9 then 'draw'
    else 'ongoing'     
}
