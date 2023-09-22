# Create a new score board with an initial entry.
#   input: none.
#   output: a score board object with a default player and score.
def create_score_board:
  {"The Best Ever": 1000000};

# Add a player to a score board.
#   input: a score board object.
#   output: the score board with the new player added.
def add_player(player; score):
  #.[player] = score;
  {(player): score} + .;

# Remove a player from a score board.
#   input: a score board object.
#   output: the score board with the player removed, if they exist.
def remove_player(player):
  del(.[player]);

# Increase a player's score by the given amount.
#   input: a score board object.
#   output: the score board with the player's score increased, if they exist.
def update_score(player; points):
  .[player] += points;

# Apply 100 bonus points to all players on the board.
#   input: a score board object.
#   output: the score board with each player's score increased.
def apply_monday_bonus:
  map_values(. + 100);

# Calculate the total score of all players.
#   input: a score board object.
#   output: the sum of all scores, or zero for an empty board.
#
# Notes:
# 1. passing an object to `.[]` returns a stream of the object's values.
# 2. `add` is only documented to work on arrays but it also works to add an object's values.
#    It is implemented as (https://github.com/jqlang/jq/blob/8f81668014f4df2654aa9ab674b5498aa9446441/src/builtin.jq#L11C1-L12C1):
#       def add: reduce .[] as $x (null; . + $x);
#       
def total_score:
  add // 0;
