\name{write_wright_map}
\alias{write_wright_map}
\title{Write an ASCII Wright map or score table to a text or Markdown file}
\usage{
write_wright_map(
  x,
  file,
  format = c("auto", "txt", "md"),
  append = FALSE,
  code_fence = TRUE
)
}
\arguments{
  \item{x}{An object created by \code{wright_map_ascii()} or \code{score_table_ascii()}.}
  \item{file}{Output path.}
  \item{format}{Output format. \code{"auto"} infers from the file extension.}
  \item{append}{Whether to append to an existing file.}
  \item{code_fence}{When writing Markdown, wrap the map in a fenced code block.}
}
\value{
Invisibly returns the output path.
}
\description{
Writes a rendered ASCII Wright map to a plain-text or Markdown file while
preserving fixed-width alignment.
}
\examples{
dat <- example_wright_data()
map <- wright_map_ascii(dat$persons, dat$items)
tmp <- tempfile(fileext = ".txt")
write_wright_map(map, tmp)
}
