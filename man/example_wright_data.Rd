\name{example_wright_data}
\alias{example_wright_data}
\title{Example person and item measures}
\usage{
example_wright_data()
}
\value{
A list with named numeric vectors \code{persons}, \code{person_scores}, and
\code{items}.
}
\description{
Returns a small set of person and item measures that produce a Winsteps-like
ASCII Wright map and can be used for demos or tests.
}
\examples{
dat <- example_wright_data()
str(dat)
}
