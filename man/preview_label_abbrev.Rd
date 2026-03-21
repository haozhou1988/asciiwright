\name{preview_label_abbrev}
\alias{preview_label_abbrev}
\title{Preview label abbreviations before rendering a map}
\usage{
preview_label_abbrev(
  labels,
  width,
  style = c("truncate", "smart"),
  side = "item",
  label_overrides = NULL,
  unique = TRUE
)
}
\arguments{
  \item{labels}{Character vector of labels, a named vector, or a data frame with a \code{label} column.}
  \item{width}{Target width for the abbreviated labels.}
  \item{style}{Abbreviation strategy. \code{"truncate"} clips labels at the available width; \code{"smart"} removes low-information words and compresses longer words before truncating.}
  \item{side}{Side identifier passed through to \code{label_overrides}.}
  \item{label_overrides}{Optional user-supplied label abbreviations. This can be a named character vector of exact replacements or a function. Functions receive \code{label}, \code{width}, and \code{side}.}
  \item{unique}{Whether to preview each unique label only once.}
}
\value{
A data frame with the original labels, suggested abbreviations, final
abbreviations after overrides, and change flags.
}
\description{
Lets you inspect how the package will abbreviate labels before drawing an ASCII
Wright map. This is useful for spotting awkward shortenings, testing custom
override functions, and preparing hand-edited labels for
\code{make_label_overrides()}.
}
\examples{
preview <- preview_label_abbrev(
  c("Read books on animals", "Find where animal lives"),
  width = 18,
  style = "smart"
)
preview
}
