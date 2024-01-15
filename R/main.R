library(tidyr)
library(readr)
library(dplyr)
library(BradleyTerry2)
library(tidyverse)

TennisTidyr <- function(startyear, endyear, testset, numgames) {
  #' Get data and unique player names from year range
  #' @param startyear Year to start from
  #' @param endyear Last year to include
  #' @param testset Year of the test set
  #' @param numgames Minimum number of games per player
  #' @returns 2 datasets, accessible by $train and $test containing the
  #' training set and test set, and $main_names, the unique players.
  # Adding errors for if years will not match the dataset
  if (!(startyear > 2012 & startyear < 2023)) {
    stop("Error: Start year must be between 2013 and 2022")
  }
  if (!(endyear > 2012 & endyear < 2023)) {
    stop("Error: End year must be between 2013 and 2022")
  }
  if (!(testset > 2012 & testset < 2024)) {
    stop("Error: Test set must be between 2013 and 2023")
  }
  if (startyear > endyear) {
    stop("Error: Start year must be less than end year")
  }
  if (endyear > testset) {
    stop("Error: End year must be less than test set")
  }

  datasets <- list("2013" = wta_matches_2013, "2014" = wta_matches_2014,
                   "2015" = wta_matches_2015, "2016" = wta_matches_2016,
                   "2017" = wta_matches_2017, "2018" = wta_matches_2018,
                   "2019" = wta_matches_2019, "2020" = wta_matches_2020,
                   "2021" = wta_matches_2021, "2022" = wta_matches_2022,
                   "2023" = wta_matches_2023)

  #Creating blank dataset for training set, then appending each year
  train <- tibble()
  num_names <- c()

  for (i in startyear:endyear) {
    data <- as.data.frame(datasets[i-2012])
    colnames(data) <- colnames(wta_matches_2013)
    num_names <- append(num_names, c(as.matrix(data[, "winner_name"]),
                                     as.matrix(data[, "loser_name"])))
    train <- rbind(train, data)
  }
  #List of all unique names in that time period
  names <- unique(num_names)
  #Number of games played per player
  n_names <- length(names)
  num_games <- rep(0, n_names)
  for (i in 1:n_names){
    num_games[i] <- length(which(num_names == names[i]))
  }
  #Players who played in the test set (2023)
  test_set <- as.data.frame(datasets[testset-2012])
  colnames(test_set) <- colnames(wta_matches_2013)
  test_names <- c(as.matrix(test_set[, "winner_name"]),
                  as.matrix(test_set[, "loser_name"]))

  #List of 'significant' players
  main_names <- names[num_games > numgames & names %in% test_names]
  #Only selecting columns we currently care about
  train <- train[, c("winner_id", "winner_name", "winner_hand", "winner_ht", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon",
                     "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "loser_id", "loser_name", "loser_hand", "loser_ht",
                     "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved",
                     "l_bpFaced", "surface","winner_rank_points","loser_rank_points","tourney_date")]
  test <- test_set[,c("winner_id", "winner_name", "winner_hand", "winner_ht", "w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon",
                      "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced", "loser_id", "loser_name", "loser_hand", "loser_ht",
                      "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved",
                      "l_bpFaced", "surface","winner_rank_points","loser_rank_points","tourney_date")]

  test

  #Only selecting players we care about
  train <- train[train$winner_name %in% main_names, ]
  train <- train[train$loser_name %in% main_names, ]
  test <- test[test$winner_name %in% main_names, ]
  test <- test[test$loser_name %in% main_names, ]
  return(list(train=train, test=test, main_names=main_names))
}


time_weighting <- function(t, weight) {
  #' Return the weighting parameter for a given time
  #' @param t time since match
  #' @param weight weight to be applied
  #' @returns weight to be applied to match
  return(min(weight, weight^t))
}

func <- function(matches, data, indices) {
  #' Gives cumulative data about each player across all matches
  #' @param matches a dataframe with 2 columns: winner and loser
  #' @param data is the dataframe containing all the data
  #' @param indices is a vector of row numbers in 'data' corresponding to the matches in 'matches'
  #' @returns list(w_output, l_output),
  #' w_output: dataframe of all the data for the winner in each match
  #' l_output: dataframe of all the data for the loser in each match
  players <- unique( c(matches[,1], matches[,2]) )
  # 'scores' keeps track of each player's numbers so far.
  scores <- data.frame(Player=players,
                       Ace=rep(0, length(players)), Df=rep(0, length(players)),
                       Svpt=rep(0, length(players)), FirstIn=rep(0, length(players)),
                       FirstWon=rep(0, length(players)), SecondWon=rep(0, length(players)),
                       SvGms=rep(0, length(players)), BpSaved=rep(0, length(players)),
                       BpFaced=rep(0, length(players)) )

  # Extract relevant columns from 'data'
  w_numbers <- data[indices, 5:13]
  l_numbers <- data[indices, 18:26]

  # 'w_output' and 'l_output' are the variables we want
  w_output <- matrix( 0, dim(matches)[1], 9 ) # because there are 9 variables
  w_output <- as.data.frame(w_output)
  colnames(w_output) <- colnames(scores[-1])
  l_output <- w_output

  # Print to check dimensions are correct
  #View(w_numbers)
  #View(l_numbers)
  #View(players)
  #View(scores)
  #View(w_output)
  #View(l_output)


  for (n in 2:dim(matches)[1]) {
    prev_winner <- which(scores$Player==matches[n-1,1]) # gives row number of player who won the previous match
    scores[prev_winner, 2:10] = w_numbers[n-1,] # replaces previous match winner's score with the numbers obtained from previous match data
    # Same for losers
    prev_loser <- which(scores$Player==matches[n-1,2])
    scores[prev_loser, 2:10] = l_numbers[n-1,]

    curr_player1 <- which(scores$Player==matches[n,1]) # gives row number of 1st player in upcoming match
    curr_player2 <- which(scores$Player==matches[n,2]) # same for 2nd player
    w_output[n,] <- scores[curr_player1, 2:10] # get current scores of 1st player in upcoming match
    l_output[n,] <- scores[curr_player2, 2:10] # same for 2nd player
  }

  return( list(w_output=w_output, l_output=l_output) )
}

func_surface <- function(MATCHES, PREV_WINS) {

  players <- unique( c(MATCHES[,1], MATCHES[,2]) )
  scores <- data.frame(Player=players, Score=rep(0, length(players))) # 'scores' keeps track of their # previous wins on a surface
  PREV_WINS <- matrix( 0, dim(MATCHES)[1], 2)

  # Printing to see if it works
  #View(matches_clay)
  #View(matches_grass)
  #View(matches_hard)
  #View(prev_wins_clay)
  #View(prev_wins_grass)
  #View(prev_wins_hard)
  #View(players)
  #View(scores)
  #View(PREV_WINS)

  for (n in 2:dim(PREV_WINS)[1]) {
    prev_winner <- which(scores$Player==MATCHES[n-1,1]) # gives row number of previous match winner in 'scores' df
    scores[prev_winner, 2] = scores[prev_winner, 2] + 1 # updates previous match winner's score

    curr_player1 <- which(scores$Player==MATCHES[n,1]) # gives row number of 1st player in upcoming match
    curr_player2 <- which(scores$Player==MATCHES[n,2]) # same for 2nd player
    PREV_WINS[n,] = c(scores[curr_player1, 2], scores[curr_player2, 2]) # gives current scores of players in upcomin match (unchanged if neither player were in previous match)
  }

  return(PREV_WINS)
}

get_recency_weights <- function(train_set, recency_weighting) {
  #' Get weights to be applied to each match based on recency
  #' @param train_set dataframe of training set
  #' @param recency_weighting weight to be applied to each match
  #' @returns vector of weights to be applied to each match
  #' Note: if train_set does not contain a 'tourney_date' column, returns NA
  tryCatch(
    {
      train_set$tourney_date
    },
    error = function(cond) {
      message("train_set does not contain a 'tourney_date' column.")
      return(NA)
    }
  )

  tourney_year = sapply(train_set$tourney_date, function(x) {as.numeric(substr(x, 1, 4))})
  time <- 2023-tourney_year
  train_set$time <- time
  weights <- sapply(train_set$time, function(x) time_weighting(x, recency_weighting))
  return(weights)
}


predict_ <- function(player1, player2, df_coeff) {
  #' Predicts the probability that player1 will beat player2
  #' @param player1 name of 1st player
  #' @param player2 name of 2nd player
  #' @param df_coeff dataframe of coefficients
  #' @returns: vector of prediction probabilities [1] and boolean of whether player1 won [2]

  lambda1 <- df_coeff[player1,1]
  lambda2 <- df_coeff[player2,1]
  if (is.na(lambda1)) {
    cat(player1, " ", player2, "\n")
    cat(lambda1, " ", lambda2, "\n")
  }
  pred <- exp(lambda1) / (exp(lambda1) + exp(lambda2))
  return(c(pred>0.5, pred))
}


predict_matches <- function(test_set, names, model) {
  #' Predicts the probability that player1 will beat player2 for each match in test_set
  #' @param test_set dataframe of test set
  #' @param names vector of names of players in test set
  #' @param model BTm model
  #' @returns dataframe of predictions for each match in test_set
  #'
  df_coeff <- as.data.frame(BTabilities(model))

  # make predictions
  draws <- test_set[,c("winner_name","loser_name")]
  draws <- draws[draws$winner_name %in% names & draws$loser_name %in% names, ]
  draws$pred <- NA

  for (i in 1:nrow(draws)) {
    player1 <- draws$winner_name[i]
    player2 <- draws$loser_name[i]
    pred <- predict_(player1, player2, df_coeff)
    draws$pred[i] <- pred[2]
  }

  return(draws)
}


standard_error <- function(x) sd(x) / sqrt(length(x))


score_predictions <- function(predictions) {
  #' Scores predictions
  #' @param predictions dataframe of predictions
  #' @returns list of accuracy, avg probability, avg probability standard error,
  #'         avg log probability, avg log probability standard error
  #' Note: predictions must contain a 'pred' column
  #'
  # accuracy
  predictions$correct <- predictions$pred > 0.5
  num <-nrow(filter(predictions, predictions$correct))
  accuracy <- num / nrow(predictions)

  # avg probability
  winner_prob <- predictions[predictions$pred > 0.5,]$pred
  avg_probability <- mean(winner_prob)
  avg_prob_se <- standard_error(winner_prob)

  #avg log probability
  avg_log_probability <- mean(log(winner_prob))
  avg_log_prob_se <- standard_error(log(winner_prob))

  return(list(accuracy = accuracy, avg_probability = avg_probability,
              avg_prob_se = avg_prob_se,
              avg_log_probability = avg_log_probability,
              abg_log_prob_se = avg_log_prob_se))
}
