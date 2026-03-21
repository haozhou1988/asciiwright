\name{make_label_overrides}
\alias{make_label_overrides}
\title{Build a label override vector from previewed or edited labels}
\usage{
make_label_overrides(
  x,
  replacements = NULL,
  original_col = "label",
  replacement_col = "final",
  drop_identity = TRUE,
  drop_empty = TRUE
)
}
\arguments{
  \item{x}{A preview data frame created by \code{preview_label_abbrev()}, a data frame containing label columns, or a character vector / named vector of original labels.}
  \item{replacements}{Optional replacement labels. When \code{x} is a data frame and \code{replacements} is \code{NULL}, \code{replacement_col} is used. When \code{x} is a vector, \code{replacements} must be a character vector of the same length.}
  \item{original_col}{Name of the column containing the original labels when \code{x} is a data frame.}
  \item{replacement_col}{Name of the column containing the edited labels when \code{x} is a data frame.}
  \item{drop_identity}{Whether to omit entries whose replacement is identical to the original label.}
  \item{drop_empty}{Whether to omit missing or empty replacements.}
}
\value{
A named character vector suitable for the \code{label_overrides} argument used
by \code{wright_map_ascii()} and related functions.
}
\description{
Turns a preview table or a pair of label vectors into a compact
\code{label_overrides} object. A common workflow is to preview abbreviations,
edit the \code{final} column by hand, then feed the result back into the map
renderer.
}
\examples{
preview <- preview_label_abbrev(
  c("Read books on animals", "Find where animal lives"),
  width = 18,
  style = "smart"
)
preview$final[preview$label == "Find where animal lives"] <- "Animal habitat"

overrides <- make_label_overrides(preview)
overrides
}
