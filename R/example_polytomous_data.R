#' Example data for polytomous threshold Wright maps
#'
#' @return A list with person measures, item measures, shared step thresholds,
#'   and category codes.
#' @export
example_polytomous_data <- function() {
  list(
    persons = c(
      A = 3.2,
      B = 2.7,
      C = 2.1,
      D = 1.4,
      E = 1.0,
      F = 0.6,
      G = 0.2,
      H = -0.3,
      I = -0.8,
      J = -1.5,
      K = -2.1,
      L = -3.0
    ),
    items = c(
      `Watch birds` = -0.47,
      `Read books on animals` = -0.80,
      `Find where animal lives` = -1.20,
      `Go to museum` = -3.00,
      `Go to zoo` = -3.90,
      `Go on picnic` = -4.80
    ),
    steps = c(-1.03, 1.03),
    category_codes = c("0", "1", "2"),
    category_labels = c("Dislike", "Neutral", "Like")
  )
}
