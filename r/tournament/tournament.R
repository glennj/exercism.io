tournament <- tournament_take2

tournament_take2 <- function(input) {
  # utility function to succinctly create a data.frame
  df <- function(team, win = 0, loss = 0, draw = 0) {
    data.frame(Team = team, W = win, L = loss, D = draw)
  }

  # initialize the data.frame with a dummy team
  standings <- df("__dummy__", 0, 0, 0)

  # and then remove the dummy team from the data.frame
  standings <- standings[standings$Team != "__dummy__", ]

  for (record in strsplit(input, ";")) {
    if (length(record) != 3) next

    home   <- record[1]
    away   <- record[2]
    result <- record[3]

    if (result == "win") {
      standings <- rbind(standings, df(home, win  = 1),
                                    df(away, loss = 1))
    }
    if (result == "loss") {
      standings <- rbind(standings, df(home, loss = 1),
                                    df(away, win  = 1))
    }
    if (result == "draw") {
      standings <- rbind(standings, df(home, draw = 1),
                                    df(away, draw = 1))
    }
  }

  # sum up all the individual items in the standings
  standings <- aggregate(. ~ Team, standings, sum)

  # calculate the MP and P columns
  attach(standings)
  standings$MP <- W + D + L
  standings$P  <- 3 * W + D
  detach(standings)

  # sort
  standings <- standings[with(standings, order(-P, Team)), ]

  # reset row.names
  row.names(standings) <- NULL

  # return, setting the desired column order
  standings[, c("Team", "MP", "W", "D", "L", "P")]
}


### Take 1
# Construct separate vectors for Team, MP, etc.
# Only create the data.frame near the end of the function.
#
tournament_take1 <- function(input) {
  valid_results <- c("win", "loss", "draw")

  Team <- c()
  W <- c()
  L <- c()
  D <- c()

  for (record in strsplit(input, ";")) {
    # ignore invalid records
    if (length(record) != 3) next

    home <- record[1]
    away <- record[2]
    result <- record[3]

    # ignore invalid result string
    if (!(result %in% valid_results)) next

    if (!(home %in% Team)) {
      Team <- append(Team, home)
      W    <- append(W, 0)
      L    <- append(L, 0)
      D    <- append(D, 0)
    }
    if (!(away %in% Team)) {
      Team <- append(Team, away)
      W    <- append(W, 0)
      L    <- append(L, 0)
      D    <- append(D, 0)
    }

    home_idx <- which(Team == home)
    away_idx <- which(Team == away)

    if (result == "win") {
      W[home_idx] <- W[home_idx] + 1
      L[away_idx] <- L[away_idx] + 1
    } else if (result == "loss") {
      L[home_idx] <- L[home_idx] + 1
      W[away_idx] <- W[away_idx] + 1
    } else if (result == "draw") {
      D[home_idx] <- D[home_idx] + 1
      D[away_idx] <- D[away_idx] + 1
    }
  }

  # input contained no valid results
  if (length(Team) < 2) return

  # calculate the Matches Playes and Points vectors
  MP <- W + D + L
  P  <- W * 3 + D

  standings <- data.frame(Team, MP, W, D, L, P)
  sorted <- standings[with(standings, order(-P, Team)), ]
  row.names(sorted) <- NULL     # reset row.names
  sorted
}


# insane community solution
# https://exercism.io/tracks/r/exercises/tournament/solutions/d9569bbfb26848ca88f4fb77778da222

library(dplyr)

tournament <- function(input) {

  # Detect valid inputs
  clean_input <- grepl('^[a-zA-Z ]+;[a-zA-Z ]+;(win|loss|draw)$', input)

  # Built matrix with valid input
  games_df <- matrix(unlist(strsplit(input[clean_input], split = ';')),
                     ncol = 3,
                     byrow = TRUE)

  # Expand results for each team
  tally <- data.frame(
    Team = c(games_df[,1], games_df[,2]),
    player = c(rep("player1", nrow(games_df)), rep("player2", nrow(games_df))),
    result = rep(games_df[,3], 2)) %>%

    # calculate tally results as numeric
    group_by(Team) %>%
    summarise(
      MP = as.numeric(n()),
      W = as.numeric(sum((result == 'win'  & player == 'player1')
                        |(result == 'loss' & player == 'player2'))),
      D = as.numeric(sum(result == 'draw')),
      L = as.numeric(sum((result == 'loss' & player == 'player1')
                        |(result == 'win'  & player == 'player2')))) %>%
    mutate(P = (W*3 + D)) %>%
    arrange(desc(P), Team)

  return(tally)
}
