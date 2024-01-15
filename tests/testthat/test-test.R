library(testthat)
library(tidyverse)
library(BradleyTerry2)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# test the package

# load data
data <- TennisTidyr(2013, 2022, 2023, 20)
train <- data$train
test <- data$test
players <- data$main_names

test_that("data loading works", {

  # check invalid years
  expect_error(TennisTidyr(2012, 2022, 2023, 20), "Error: Start year must be between 2013 and 2022")
  expect_error(TennisTidyr(2013, 2022, 2024, 20), "Error: Test set must be between 2013 and 2023")
  expect_error(TennisTidyr(2013, 2023, 2023, 20), "Error: End year must be between 2013 and 2022")

  # check invalid number of games
  expect_error(TennisTidyr(2013, 2022, 2023, 0), "Error: Number of games must be greater than 0")
})

# test time_weighting()
test_that("time weighting works", {
  expect_equal(time_weighting(0.5, 0.8), 0.8)
  expect_equal(time_weighting(2, 0.5), 0.25)
  expect_error(time_weighting(2,2), "Error: Weight must be between 0 and 1")
  expect_error(time_weighting(-1, 0.5), "Error: Time must be positive")

  # test get_recency_weights()
  expect_no_error(get_recency_weights(train, 0.8))
})

test_that("training and prediction work", {
  # test the predict_() function
  df_coeff <- matrix(c(0.8, 0.2), 2, 1)
  df_coeff <- as.data.frame(df_coeff)
  rownames(df_coeff) <- c("P1", "P2")

  expect_equal(predict_("P1", "P2", df_coeff), c(1, 0.6456563))
  expect_error(predict_("P1", "P3", df_coeff), "Player 2 is not in the coefficient dataframe")

  model <- BTm(outcome=1, factor(winner_name, levels=players),
               factor(loser_name, levels=players), data=train)
  expect_no_error(predict_matches(test, players, model))


  # test the score_predictions() function
  predictions <- predict_matches(test, players, model)
  expect_no_error(score_predictions(predictions))
  score <- score_predictions(predictions)
  score
  expect_equal(score_predictions(predictions)$accuracy, 0.6340694)
  expect_equal(score_predictions(predictions)$avg_probability, 0.6746671, tolerance = 0.0001)
})
