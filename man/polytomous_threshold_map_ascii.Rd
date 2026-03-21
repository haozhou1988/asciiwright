\name{polytomous_threshold_map_ascii}
\alias{polytomous_threshold_map_ascii}
\title{Create a Winsteps-style polytomous threshold Wright map}
\usage{
polytomous_threshold_map_ascii(
  persons,
  items,
  steps,
  mode = c("halfpoint", "thurstonian", "andrich", "center"),
  category_codes = NULL,
  low_adj = 0.25,
  high_adj = 0.25,
  person_title = "PERSON",
  item_title = "ITEM",
  ...
)
}
\arguments{
  \item{persons}{Numeric vector or data frame of person measures.}
  \item{items}{Numeric vector or data frame of item measures.}
  \item{steps}{Step thresholds for each item. This can be a shared numeric vector, a matrix/data frame with one row per item, or a list with one numeric vector per item. Thresholds are Rasch-Andrich thresholds relative to item difficulty.}
  \item{mode}{Which polytomous map to produce: \code{"halfpoint"}, \code{"thurstonian"}, \code{"andrich"}, or \code{"center"}.}
  \item{category_codes}{Labels used in the suffixes appended to each item.}
  \item{low_adj}{Adjustment for the bottom extreme category in \code{"center"} mode.}
  \item{high_adj}{Adjustment for the top extreme category in \code{"center"} mode.}
  \item{person_title}{Heading shown for the person side.}
  \item{item_title}{Base heading shown for the item side.}
  \item{...}{Additional arguments passed to \code{wright_map_ascii()}, such as \code{label_abbrev}, \code{label_overrides}, \code{person_display}, or \code{right_measure}.}
}
\value{
An object of class \code{ascii_wright_map}.
}
\description{
Expands each polytomous item into multiple threshold or category points and
renders them in a Winsteps-style ASCII Wright map. The supported modes align
with the main threshold-style maps shown in Winsteps Table 1.5 to 1.8.
}
\examples{
pdat <- example_polytomous_data()
map <- polytomous_threshold_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  mode = "halfpoint",
  person_display = "distribution",
  label_width = 18,
  label_abbrev = "smart"
)
print(map)
}
