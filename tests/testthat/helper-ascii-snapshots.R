expect_ascii_snapshot <- function(x, snapshot_name, ...) {
  actual <- if (inherits(x, c("ascii_wright_map", "ascii_score_table"))) {
    format(x, ...)
  } else {
    as.character(x)
  }
  actual <- gsub("\f", "<PAGE BREAK>", actual, fixed = TRUE)
  actual <- sub("[ ]+$", "", actual)

  snapshot_path <- test_path("_ascii_snaps", snapshot_name)
  expected <- readLines(snapshot_path, warn = FALSE)
  expected <- sub("[ ]+$", "", expected)

  expect_identical(actual, expected)
}
