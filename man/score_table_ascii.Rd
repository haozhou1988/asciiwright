\name{score_table_ascii}
\alias{score_table_ascii}
\title{Create a Winsteps-style raw score-to-measure ASCII table}
\usage{
score_table_ascii(
  items,
  include_extremes = TRUE,
  columns = 3L,
  digits = 2L,
  se_digits = 2L,
  score_digits = 0L,
  extreme_adjust = 0.3,
  title = NULL
)
}
\arguments{
  \item{items}{Numeric vector or data frame of item difficulties.}
  \item{include_extremes}{Whether to include minimum and maximum possible scores.}
  \item{columns}{Number of side-by-side table blocks.}
  \item{digits}{Digits shown for measures.}
  \item{se_digits}{Digits shown for standard errors.}
  \item{score_digits}{Digits shown for score labels.}
  \item{extreme_adjust}{Extreme score adjustment used for minimum and maximum scores under the dichotomous Rasch model.}
  \item{title}{Optional title override.}
}
\value{
An object of class \code{ascii_score_table}.
}
\description{
Generates a fixed-width ASCII raw score-to-measure table inspired by Winsteps
Table 20.1 using item difficulties under a dichotomous Rasch model.
}
\examples{
dat <- example_wright_data()
tbl <- score_table_ascii(dat$items)
print(tbl)
}
