# MEJORA AL ALGORITMO MINIMAX DESARROLLADO POR codertime.org
# SIMPLEMENTE MODIFICANDO LOS VALORES DE LAS PIEZAS

# codertime.org (no sé el nombre del autor/a) desarrolló un algoritmo minimax (la función minimax_move) para jugar al ajedrez
# basandose en el valor que se usa habitualmente en las piezas: peon = 1, caballo = 3, alfil = 3, torre = 5 y dama = 9
# (esta escala es solo una guia, toda pieza vale de acuerdo a su utilidad)
# yo modifique el algoritmo de acuerdo a distintas escalas:

#  1. minimax_moveBD basandome en la escala obtenida por Rasmus Bååth utilizando BigData (1 millon de jugadas de la base de datos "the Million Base 2.2")
#     CONCLUSION:
#     minimax_moveBD fue superada por el algoritmo original (con negras perdio 6 y empató 2 de 10 partidos; y con blancas perdió 8 de 10)
#
#  2. minimax_moveAZ basandome en la escala utilizada por AlphaZero
#     CONCLUSION:
#     minimax_moveAZ superó al algoritmo original (con negras gano 6 y empató 2 de 10 partidos; y con blancas ganó 6 de 10)



# Load the rchess package: (paquete que "sabe" cuales son las movidas permitidas)
library(rchess)
# Create a new game:
chess_game <- Chess$new()
# Plot the current position:
plot(chess_game)

chess_game$moves()

chess_game$turn()

# Random move generator for a given position:
get_random_move <- function(chess_game) {
  return(sample(chess_game$moves(), size = 1))
}

# Set up a new game
chess_game <- Chess$new()
# Perform random legal moves until the game ends in mate or draw
while (!chess_game$game_over()) {
  chess_game$move(get_random_move(chess_game))
}
# Plot the final position
plot(chess_game)

chess_game$fen()
chess_game$game_over()
chess_game$in_checkmate()
chess_game$in_check()


#Let’s implement these simple heuristics in R:
# Position evaluation function
evaluate_position <- function(position) {
  # Test if black won
  if (position$in_checkmate() & (position$turn() == "w")) {
    return(-1000)
  }
  # Test if white won
  if (position$in_checkmate() & (position$turn() == "b")) {
    return(1000)
  }
  # Test if game ended in a draw
  if (position$game_over()) {
    return(0)
  }
  # Compute material advantage
  position_fen <- strsplit(strsplit(position$fen(), split = " ")[[1]][1], split = "")[[1]]
  white_score <- length(which(position_fen == "Q")) * 9 + length(which(position_fen == "R")) * 5 + length(which(position_fen == "B")) * 3 + length(which(position_fen == "N")) * 3 + length(which(position_fen == "P"))
  black_score <- length(which(position_fen == "q")) * 9 + length(which(position_fen == "r")) * 5 + length(which(position_fen == "b")) * 3 + length(which(position_fen == "n")) * 3 + length(which(position_fen == "p"))
  # Evaluate king safety
  check_score <- 0
  if (position$in_check() & (position$turn() == "w")) check_score <- -1
  if (position$in_check() & (position$turn() == "b")) check_score <- 1
  # Return final position score
  return(white_score - black_score + check_score)
}

evaluate_position(chess_game)


#Improving our chess logic: minimax search
# Score position via minimax strategy
minimax_scoring <- function(chess_game, depth) {
  # If the game is already over or the depth limit is reached
  # then return the heuristic evaluation of the position
  if (depth == 0 | chess_game$game_over()) {
    return(evaluate_position(chess_game))
  }
  # Run the minimax scoring recursively on every legal next move, making sure the search depth is not exceeded
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    next_move_scores[i] <- minimax_scoring(chess_game, depth - 1)
    chess_game$undo()
  }
  # White will select the move that maximizes the position score
  # Black will select the move that minimizes the position score
  if (chess_game$turn() == "w") {
    return(max(next_move_scores))
  } else {
    return(min(next_move_scores))
  }
}

#This completes the minimax_scoring function that returns a score given an input position. Now we just need a wrapper function to evaluate all legal next moves via minimax_scoring and return the optimal choice:


# Select the next move based on the minimax scoring
get_minimax_move <- function(chess_game) {
  # Score all next moves via minimax
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    # To ensure fast execution of the minimax function we select a depth of 1
    # This depth can be increased to enable stronger play at the expense of longer runtime
    next_move_scores[i] <- minimax_scoring(chess_game, 1)
    chess_game$undo()
  }
  # For white return the move with maximum score
  # For black return the move with minimum score
  # If the optimal score is achieved by multiple moves, select one at random
  # This random selection from the optimal moves adds some variability to the play
  if (chess_game$turn() == "w") {
    return(sample(next_moves[which(next_move_scores == max(next_move_scores))], size = 1))
  } else {
    return(sample(next_moves[which(next_move_scores == min(next_move_scores))], size = 1))
  }
}


# Function that takes a side as input ("w" or "b") and plays 10 games
# The selected side will choose moves based on the minimax algorithm
# The opponent will use the random move generator
play_10_games <- function(minimax_player) {
  game_results <- vector(length = 10)
  for (i in 1:10) {
    chess_game <- Chess$new()
    while (!chess_game$game_over()) {
      if (chess_game$turn() == minimax_player) {
        # Selected player uses the minimax strategy
        chess_game$move(get_minimax_move(chess_game))
      } else {
        # Opponent uses the random move generator
        chess_game$move(get_random_move(chess_game))
      }
    }
    # Record the result of the current finished game
    # If mate: the losing player is recorded
    # If draw: record a 0
    if (chess_game$in_checkmate()) {
      game_results[i] <- chess_game$turn()
    } else {
      game_results[i] <- "0"
    }
  }
  # Print the outcome of the 10 games
  print(table(game_results))
}



play_10_games("w")
## game_results
##  b
## 10

play_10_games("b")
## game_results
##  w
## 10

# codertime.org demostró que su algoritmo le gana al generador de movidas al azar
# también comenta que se desempeña bien contra jugadores casuales.

















################################################################################
# modificación 1
# basado en
# y redondeando de acuerdo a mi criterio
#p=1
#c=1.4
#a=1.6
#t=2.3
#D=3.9




evaluate_positionBD <- function(position) {
  # Test if black won
  if (position$in_checkmate() & (position$turn() == "w")) {
    return(-1000)
  }
  # Test if white won
  if (position$in_checkmate() & (position$turn() == "b")) {
    return(1000)
  }
  # Test if game ended in a draw
  if (position$game_over()) {
    return(0)
  }
  # Compute material advantage
  position_fen <- strsplit(strsplit(position$fen(), split = " ")[[1]][1], split = "")[[1]]
  white_scoreBD <- length(which(position_fen == "Q")) * 3.9 + length(which(position_fen == "R")) * 2.3 + length(which(position_fen == "B")) * 1.6 + length(which(position_fen == "N")) * 1.4 + length(which(position_fen == "P"))
  black_scoreBD <- length(which(position_fen == "q")) * 3.9 + length(which(position_fen == "r")) * 2.3 + length(which(position_fen == "b")) * 1.6 + length(which(position_fen == "n")) * 1.4 + length(which(position_fen == "p"))
  # Evaluate king safety
  check_score <- 0
  if (position$in_check() & (position$turn() == "w")) check_score <- -1
  if (position$in_check() & (position$turn() == "b")) check_score <- 1
  # Return final position score
  return(white_scoreBD - black_scoreBD + check_score)
}

evaluate_positionBD(chess_game)


#Improving our chess logic: minimax search
# Score position via minimax strategy
minimax_scoringBD <- function(chess_game, depth) {
  # If the game is already over or the depth limit is reached
  # then return the heuristic evaluation of the position
  if (depth == 0 | chess_game$game_over()) {
    return(evaluate_positionBD(chess_game))
  }
  # Run the minimax scoring recursively on every legal next move, making sure the search depth is not exceeded
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    next_move_scores[i] <- minimax_scoringBD(chess_game, depth - 1)
    chess_game$undo()
  }
  # White will select the move that maximizes the position score
  # Black will select the move that minimizes the position score
  if (chess_game$turn() == "w") {
    return(max(next_move_scores))
  } else {
    return(min(next_move_scores))
  }
}

#This completes the minimax_scoring function that returns a score given an input position. Now we just need a wrapper function to evaluate all legal next moves via minimax_scoring and return the optimal choice:


# Select the next move based on the minimax scoring
get_minimax_moveBD <- function(chess_game) {
  # Score all next moves via minimax
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    # To ensure fast execution of the minimax function we select a depth of 1
    # This depth can be increased to enable stronger play at the expense of longer runtime
    next_move_scores[i] <- minimax_scoringBD(chess_game, 1)
    chess_game$undo()
  }
  # For white return the move with maximum score
  # For black return the move with minimum score
  # If the optimal score is achieved by multiple moves, select one at random
  # This random selection from the optimal moves adds some variability to the play
  if (chess_game$turn() == "w") {
    return(sample(next_moves[which(next_move_scores == max(next_move_scores))], size = 1))
  } else {
    return(sample(next_moves[which(next_move_scores == min(next_move_scores))], size = 1))
  }
}


# Function that takes a side as input ("w" or "b") and plays 10 games
# The selected side will choose moves based on the minimax algorithm
# The opponent will use the random move generator
play_10_games <- function(minimax_player) {
  game_results <- vector(length = 10)
  for (i in 1:10) {
    chess_game <- Chess$new()
    while (!chess_game$game_over()) {
      if (chess_game$turn() == minimax_player) {
        # Selected player uses the minimax strategy
        chess_game$move(get_minimax_move(chess_game))
      } else {
        # Opponent uses the random move generator
        chess_game$move(get_minimax_moveBD(chess_game))
      }
    }
    # Record the result of the current finished game
    # If mate: the losing player is recorded
    # If draw: record a 0
    if (chess_game$in_checkmate()) {
      game_results[i] <- chess_game$turn()
    } else {
      game_results[i] <- "0"
    }
  }
  # Print the outcome of the 10 games
  print(table(game_results))
}


play_10_games("w")         #BD perdio 6 como negras y gano2
## game_results
## 0 b w
## 2 6 2

play_10_games("b")        #BD perdio 8 como blancas y gano2
## game_results
## b w
## 2 8

# los valores basados en BD se desenpeñan peor que los valores standard




















######################################
# modificación 2 basado en las conclusiones de AlphaZero: https://arxiv.org/pdf/2009.04374.pdf
#P=1 C=3.05	A=3.33	T=5.63	D=9.5		AlphaZero
#Tomašev, N., Paquet, U., Hassabis, D., & Kramnik, V. (2020). Assessing game balance with AlphaZero: Exploring alternative rule sets in chess. arXiv preprint arXiv:2009.04374.

C = 3.05
A = 3.33
T = 5.63
D = 9.5







evaluate_positionAZ <- function(position) {
  # Test if black won
  if (position$in_checkmate() & (position$turn() == "w")) {
    return(-1000)
  }
  # Test if white won
  if (position$in_checkmate() & (position$turn() == "b")) {
    return(1000)
  }
  # Test if game ended in a draw
  if (position$game_over()) {
    return(0)
  }
  # Compute material advantage
  position_fen <- strsplit(strsplit(position$fen(), split = " ")[[1]][1], split = "")[[1]]
  white_scoreAZ <- length(which(position_fen == "Q")) * D + length(which(position_fen == "R")) * T + length(which(position_fen == "B")) * A + length(which(position_fen == "N")) * C + length(which(position_fen == "P"))
  black_scoreAZ <- length(which(position_fen == "q")) * D + length(which(position_fen == "r")) * T + length(which(position_fen == "b")) * A + length(which(position_fen == "n")) * C + length(which(position_fen == "p"))
  # Evaluate king safety
  check_score <- 0
  if (position$in_check() & (position$turn() == "w")) check_score <- -1
  if (position$in_check() & (position$turn() == "b")) check_score <- 1
  # Return final position score
  return(white_scoreAZ - black_scoreAZ + check_score)
}

evaluate_positionAZ(chess_game)


#Improving our chess logic: minimax search
# Score position via minimax strategy
minimax_scoringAZ <- function(chess_game, depth) {
  # If the game is already over or the depth limit is reached
  # then return the heuristic evaluation of the position
  if (depth == 0 | chess_game$game_over()) {
    return(evaluate_positionAZ(chess_game))
  }
  # Run the minimax scoring recursively on every legal next move, making sure the search depth is not exceeded
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    next_move_scores[i] <- minimax_scoringAZ(chess_game, depth - 1)
    chess_game$undo()
  }
  # White will select the move that maximizes the position score
  # Black will select the move that minimizes the position score
  if (chess_game$turn() == "w") {
    return(max(next_move_scores))
  } else {
    return(min(next_move_scores))
  }
}

#This completes the minimax_scoring function that returns a score given an input position. Now we just need a wrapper function to evaluate all legal next moves via minimax_scoring and return the optimal choice:


# Select the next move based on the minimax scoring
get_minimax_moveAZ <- function(chess_game) {
  # Score all next moves via minimax
  next_moves <- chess_game$moves()
  next_move_scores <- vector(length = length(next_moves))
  for (i in 1:length(next_moves)) {
    chess_game$move(next_moves[i])
    # To ensure fast execution of the minimax function we select a depth of 1
    # This depth can be increased to enable stronger play at the expense of longer runtime
    next_move_scores[i] <- minimax_scoringAZ(chess_game, 1)
    chess_game$undo()
  }
  # For white return the move with maximum score
  # For black return the move with minimum score
  # If the optimal score is achieved by multiple moves, select one at random
  # This random selection from the optimal moves adds some variability to the play
  if (chess_game$turn() == "w") {
    return(sample(next_moves[which(next_move_scores == max(next_move_scores))], size = 1))
  } else {
    return(sample(next_moves[which(next_move_scores == min(next_move_scores))], size = 1))
  }
}


# Function that takes a side as input ("w" or "b") and plays 10 games
# The selected side will choose moves based on the minimax algorithm
# The opponent will use the random move generator
play_10_games <- function(minimax_player) {
  game_results <- vector(length = 10)
  for (i in 1:10) {
    chess_game <- Chess$new()
    while (!chess_game$game_over()) {
      if (chess_game$turn() == minimax_player) {
        # Selected player uses the minimax strategy
        chess_game$move(get_minimax_move(chess_game))
      } else {
        # Opponent uses the random move generator
        chess_game$move(get_minimax_moveAZ(chess_game))
      }
    }
    # Record the result of the current finished game
    # If mate: the losing player is recorded
    # If draw: record a 0
    if (chess_game$in_checkmate()) {
      game_results[i] <- chess_game$turn()
    } else {
      game_results[i] <- "0"
    }
  }
  # Print the outcome of the 10 games
  print(table(game_results))
}


play_10_games("w")         #AZ
## game_results
## 0 b w
## 2 2 6

play_10_games("b")        #AZ
game_results
## b w
## 6 4

# los valores basados en AZ se desenpeñan mejor que los valores standard
# en un algoritmo simple como es el minimax



















