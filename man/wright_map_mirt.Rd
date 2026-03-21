\name{wright_map_mirt}
\alias{wright_map_mirt}
\title{Create an ASCII Wright map directly from a mirt model}
\usage{
wright_map_mirt(
  model,
  response_data = NULL,
  theta = NULL,
  theta_method = "EAP",
  map_type = c("auto", "items", "thresholds", "range"),
  include_scores = TRUE,
  freq_column = "auto",
  ...
)
}
\arguments{
  \item{model}{A fitted unidimensional \code{mirt} model object.}
  \item{response_data}{Optional response matrix or data frame used to compute raw scores and detect frequency columns. When omitted, the data stored in the model is used.}
  \item{theta}{Optional numeric vector or one-column matrix/data frame of person measures. When omitted, \code{mirt::fscores()} is used.}
  \item{theta_method}{Method passed to \code{mirt::fscores()} when \code{theta} is not supplied.}
  \item{map_type}{Which map to create. \code{"items"} shows one location per item, \code{"thresholds"} plots the model's \code{b1}, \code{b2}, ... category boundaries, and \code{"range"} plots bottom/center/top summaries from those boundaries. \code{"auto"} chooses \code{"thresholds"} for all-polytomous models and \code{"items"} otherwise.}
  \item{include_scores}{When \code{TRUE}, raw scores are computed from \code{response_data} and passed through to the rendered map.}
  \item{freq_column}{Name of a frequency column for aggregated response data. \code{"auto"} detects common names such as \code{"freq"} and \code{"weight"}.}
  \item{...}{Additional arguments passed to \code{wright_map_ascii()} for \code{"items"}/\code{"thresholds"} maps or to the internal range-map renderer for \code{"range"} maps.}
}
\value{
An object of class \code{ascii_wright_map}.
}
\description{
Builds an ASCII Wright map from a fitted \code{mirt} model by extracting item
locations from \code{coef(model, IRTpars = TRUE)} and person measures from
\code{mirt::fscores()}. Aggregated response tables with a \code{freq} or
\code{weight} column are detected automatically and expanded before drawing the
person distribution.
}
\examples{
if (requireNamespace("mirt", quietly = TRUE)) {
  data("Science", package = "mirt")
  mod <- mirt::mirt(Science, 1, itemtype = "graded", verbose = FALSE)

  map <- wright_map_mirt(
    mod,
    map_type = "thresholds",
    person_display = "distribution",
    label_width = 18,
    name_trunc = 18
  )
  print(map)
}
}
