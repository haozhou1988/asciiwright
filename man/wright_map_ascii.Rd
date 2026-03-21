\name{wright_map_ascii}
\alias{wright_map_ascii}
\title{Create a Winsteps-inspired ASCII Wright map}
\usage{
wright_map_ascii(
  persons,
  items,
  person_labels = NULL,
  item_labels = NULL,
  person_scores = NULL,
  score_method = c("rasch", "observed"),
  score_extreme_adjust = 0.3,
  lines_per_logit = 2L,
  measure_range = NULL,
  person_title = "PERSON",
  item_title = "ITEM",
  person_display = c("labels", "distribution"),
  item_display = c("labels", "distribution"),
  distribution_style = c("winsteps", "hashdot", "x"),
  person_hash_size = NULL,
  item_hash_size = NULL,
  axis_style = c("single", "double"),
  right_measure = FALSE,
  measure_header = "MEASURE",
  score_header = "SCORE",
  more_label = "<more>",
  rare_label = "<rare>",
  less_label = "<less>",
  freq_label = "<freq>",
  label_width = 32L,
  name_trunc = 8L,
  label_abbrev = c("truncate", "smart"),
  label_overrides = NULL,
  digits = 0L,
  score_digits = 0L
)
}
\arguments{
  \item{persons}{Numeric vector or data frame of person measures.}
  \item{items}{Numeric vector or data frame of item measures.}
  \item{person_labels}{Optional labels for person measures.}
  \item{item_labels}{Optional labels for item measures.}
  \item{person_scores}{Optional raw person scores. If \code{persons} is a data frame, a \code{score} column is used automatically unless overridden here. Each unique score is shown once at the representative location of that score.}
  \item{score_method}{How to place raw score labels on the latent scale. \code{"rasch"} estimates the score location from the item difficulties under a dichotomous Rasch model; \code{"observed"} uses the average observed person measure for each score group.}
  \item{score_extreme_adjust}{Adjustment used for extreme raw scores when \code{score_method = "rasch"}.}
  \item{lines_per_logit}{Number of text rows per logit interval.}
  \item{measure_range}{Optional numeric vector of length 2 giving the plot range.}
  \item{person_title}{Heading shown for the person side.}
  \item{item_title}{Heading shown for the item side.}
  \item{person_display}{How to render the person side: \code{"labels"} or \code{"distribution"}.}
  \item{item_display}{How to render the item side: \code{"labels"} or \code{"distribution"}.}
  \item{distribution_style}{How distribution counts are compacted. \code{"winsteps"} uses \code{X} when counts fit and automatically switches to grouped \code{\#}/\code{.} when they do not; \code{"hashdot"} always uses grouped \code{\#}/\code{.}; \code{"x"} uses one \code{X} per case.}
  \item{person_hash_size}{Optional override for the number of persons represented by one \code{\#} in grouped distributions.}
  \item{item_hash_size}{Optional override for the number of items represented by one \code{\#} in grouped distributions.}
  \item{axis_style}{Whether to render the center as a single axis (\code{"+"}/\code{"|"}) or a double axis (\code{"++"}/\code{"||"}), closer to Winsteps mirrored tables.}
  \item{right_measure}{Show a mirrored measure column on the far right.}
  \item{measure_header}{Heading shown above the measure column.}
  \item{score_header}{Heading shown above the optional score column.}
  \item{more_label}{Top-left direction label.}
  \item{rare_label}{Top-right direction label.}
  \item{less_label}{Bottom-left direction label.}
  \item{freq_label}{Bottom-right direction label.}
  \item{label_width}{Character width reserved for each side.}
  \item{name_trunc}{Maximum width of each individual label.}
  \item{label_abbrev}{Label abbreviation strategy. \code{"truncate"} clips labels at the available width; \code{"smart"} removes low-information words and compresses longer words before truncating.}
  \item{label_overrides}{Optional user-supplied label abbreviations. This can be a named character vector of exact replacements or a function. Functions receive \code{label}, \code{width}, and \code{side}.}
  \item{digits}{Digits shown for measure labels.}
  \item{score_digits}{Digits shown for score labels when scores are numeric.}
}
\value{
An object of class \code{ascii_wright_map}. Use \code{print()} or
\code{as.character()} to render the text output.
}
\description{
Creates a fixed-width ASCII Wright map that places persons and items on the
same latent-variable axis and annotates the axis with \code{M}, \code{S}, and
\code{T} markers for the mean and one or two standard deviations. Each side can
be rendered either as labels or as a compact distribution column. Optional
person raw scores can also be rendered in a dedicated \code{SCORE} column, and
mirrored layouts can show a second \code{MEASURE} column on the right. By
default, score locations are estimated from item difficulties under a
dichotomous Rasch model.
}
\examples{
dat <- example_wright_data()
map <- wright_map_ascii(dat$persons, dat$items, person_title = "KID", item_title = "TAP")
print(map)

map2 <- wright_map_ascii(
  dat$persons,
  dat$items,
  person_display = "distribution",
  item_display = "distribution",
  person_scores = dat$person_scores,
  distribution_style = "winsteps",
  label_width = 10
)
print(map2)

map3 <- wright_map_ascii(
  dat$persons,
  dat$items,
  person_scores = dat$person_scores,
  person_display = "distribution",
  item_display = "distribution",
  distribution_style = "winsteps",
  axis_style = "double",
  right_measure = TRUE,
  label_width = 8
)
print(map3)

map4 <- wright_map_ascii(
  dat$persons,
  c(`Read books on animals` = -0.8, `Find where animal lives` = -1.2),
  person_display = "distribution",
  label_width = 18,
  name_trunc = 18,
  label_abbrev = "smart",
  label_overrides = c(`Find where animal lives` = "Animal habitat")
)
print(map4)
}
