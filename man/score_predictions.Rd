% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{score_predictions}
\alias{score_predictions}
\title{Scores predictions}
\usage{
score_predictions(predictions)
}
\arguments{
\item{predictions}{dataframe of predictions}
}
\value{
list of accuracy, avg probability, avg probability standard error,
        avg log probability, avg log probability standard error
Note: predictions must contain a 'pred' column
}
\description{
Scores predictions
}
