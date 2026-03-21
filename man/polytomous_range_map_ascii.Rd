\name{polytomous_range_map_ascii}
\alias{polytomous_range_map_ascii}
\title{Create a Winsteps-style polytomous range map}
\usage{
polytomous_range_map_ascii(
  persons,
  items,
  steps,
  table_style = c("custom", "table1.4"),
  person_display = c("distribution", "labels"),
  item_display = c("distribution", "labels"),
  distribution_style = c("winsteps", "hashdot", "x"),
  person_title = "PERSON",
  item_title = "ITEM",
  measure_header = "MEASURE",
  bottom_header = paste0("BOTTOM P=50", intToUtf8(37L)),
  center_header = "MEASURE",
  top_header = paste0("TOP P=50", intToUtf8(37L)),
  more_label = "<more>",
  rare_label = "<rare>",
  less_label = "<less>",
  freq_label = "<freq>",
  measure_range = NULL,
  lines_per_logit = 2L,
  person_width = 18L,
  item_width = 14L,
  person_hash_size = NULL,
  item_hash_size = NULL,
  name_trunc = NULL,
  label_abbrev = c("truncate", "smart"),
  label_overrides = NULL,
  digits = 0L,
  right_measure = TRUE,
  line_length = NULL,
  max_page = NULL,
  page_break = "\f"
)
}
\arguments{
  \item{persons}{Numeric vector or data frame of person measures.}
  \item{items}{Numeric vector or data frame of item measures.}
  \item{steps}{Step thresholds for each item. This can be a shared numeric vector, a matrix/data frame with one row per item, or a list with one numeric vector per item.}
  \item{table_style}{Optional Winsteps-inspired preset. \code{"table1.4"} applies the standard bottom/measure/top layout defaults. \code{"custom"} leaves the explicit arguments untouched.}
  \item{person_display}{How to draw the person side: \code{"labels"} or \code{"distribution"}.}
  \item{item_display}{How to draw the three item range columns: \code{"labels"} or \code{"distribution"}.}
  \item{distribution_style}{How distribution counts are compacted.}
  \item{person_title}{Heading shown for the person side.}
  \item{item_title}{Heading shown for the item side.}
  \item{measure_header}{Heading shown above the left and optional right measure columns.}
  \item{bottom_header}{Heading shown above the bottom-range item column.}
  \item{center_header}{Heading shown above the item difficulty column.}
  \item{top_header}{Heading shown above the top-range item column.}
  \item{more_label}{Top-left direction label.}
  \item{rare_label}{Top-right direction label.}
  \item{less_label}{Bottom-left direction label.}
  \item{freq_label}{Bottom-right direction label.}
  \item{measure_range}{Optional numeric vector of length 2 giving c(min, max).}
  \item{lines_per_logit}{Number of text rows used for one logit interval.}
  \item{person_width}{Character width reserved for the person column.}
  \item{item_width}{Character width reserved for each item column.}
  \item{person_hash_size}{Optional override for the number of persons represented by one \code{\#}.}
  \item{item_hash_size}{Optional override for the number of items represented by one \code{\#}.}
  \item{name_trunc}{Maximum width of labels when label displays are used.}
  \item{label_abbrev}{Label abbreviation strategy. \code{"truncate"} clips labels at the available width; \code{"smart"} removes low-information words and compresses longer words before truncating.}
  \item{label_overrides}{Optional user-supplied label abbreviations. This can be a named character vector of exact replacements or a function. Functions receive \code{label}, \code{width}, and \code{side}, where \code{side} can be \code{"person"}, \code{"item_bottom"}, \code{"item_center"}, or \code{"item_top"}.}
  \item{digits}{Digits for printed measure labels.}
  \item{right_measure}{Show a mirrored measure column on the far right.}
  \item{line_length}{Optional maximum output width, similar to Winsteps \code{LINELENGTH=}.}
  \item{max_page}{Optional maximum number of output lines per page, similar to Winsteps \code{MAXPAGE=}.}
  \item{page_break}{Marker inserted between rendered pages when \code{max_page} creates multiple pages.}
}
\value{
An object of class \code{ascii_wright_map}.
}
\description{
Creates a Table 1.4-like ASCII range map for polytomous items with three item
columns showing bottom-range, item difficulty, and top-range locations. The
renderer can also constrain total line width and paginate long maps.
}
\examples{
pdat <- example_polytomous_data()
map <- polytomous_range_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  table_style = "table1.4",
  line_length = 80,
  max_page = 12,
  label_abbrev = "smart",
  label_overrides = c(`Read books on animals` = "Read books animals")
)
print(map)
}
