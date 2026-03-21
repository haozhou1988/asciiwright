estimate_total_score_se <- function(theta, item_measures) {
  probs <- stats::plogis(theta - item_measures)
  info <- sum(probs * (1 - probs))
  1 / sqrt(info)
}

format_score_table_measure <- function(value, digits = 2L, extreme = FALSE) {
  paste0(formatC(value, format = "f", digits = digits), if (extreme) "E" else " ")
}

format_score_table_se <- function(value, digits = 2L) {
  formatC(value, format = "f", digits = digits)
}

build_score_table_cell <- function(score, measure, se, widths, extreme = FALSE) {
  paste0(
    sprintf("%*s", widths$score, score),
    " ",
    sprintf("%*s", widths$measure, format_score_table_measure(measure, digits = widths$measure_digits, extreme = extreme)),
    " ",
    sprintf("%*s", widths$se, format_score_table_se(se, digits = widths$se_digits))
  )
}

pad_score_table_cell <- function(width) {
  strrep(" ", width)
}

#' Create a Winsteps-style raw score-to-measure ASCII table
#'
#' @param items Numeric vector or data frame of item difficulties.
#' @param include_extremes Whether to include minimum and maximum possible scores.
#' @param columns Number of side-by-side table blocks.
#' @param digits Digits shown for measures.
#' @param se_digits Digits shown for standard errors.
#' @param score_digits Digits shown for score labels.
#' @param extreme_adjust Extreme score adjustment used for minimum and maximum
#'   scores under the dichotomous Rasch model.
#' @param title Optional title override.
#' @return An object of class `ascii_score_table`.
#' @export
score_table_ascii <- function(
    items,
    include_extremes = TRUE,
    columns = 3L,
    digits = 2L,
    se_digits = 2L,
    score_digits = 0L,
    extreme_adjust = 0.3,
    title = NULL
) {
  item_df <- normalize_wright_input(items, side = "item")

  if (!is.numeric(columns) || length(columns) != 1L || columns < 1) {
    stop("'columns' must be a positive integer.", call. = FALSE)
  }

  columns <- as.integer(columns)
  max_score <- nrow(item_df)
  score_values <- if (isTRUE(include_extremes)) 0:max_score else seq.int(1L, max_score - 1L)

  if (!length(score_values)) {
    stop("No score points are available for the requested table.", call. = FALSE)
  }

  measures <- estimate_score_measures(
    score_values = score_values,
    item_measures = item_df$measure,
    extreme_adjust = extreme_adjust
  )
  ses <- vapply(measures, estimate_total_score_se, numeric(1), item_measures = item_df$measure)
  extreme <- score_values %in% c(0, max_score)

  score_labels <- format_score_values(score_values, digits = score_digits)
  measure_labels <- vapply(
    seq_along(measures),
    function(i) format_score_table_measure(measures[i], digits = digits, extreme = extreme[i]),
    character(1)
  )
  se_labels <- vapply(ses, format_score_table_se, character(1), digits = se_digits)

  widths <- list(
    score = max(nchar("SCORE", type = "width"), nchar(score_labels, type = "width")),
    measure = max(nchar("MEASURE", type = "width"), nchar(measure_labels, type = "width")),
    se = max(nchar("S.E.", type = "width"), nchar(se_labels, type = "width")),
    measure_digits = as.integer(digits),
    se_digits = as.integer(se_digits)
  )

  block_cell_width <- widths$score + widths$measure + widths$se + 2L
  block_header <- paste0(
    "| ",
    sprintf("%*s", widths$score, "SCORE"),
    " ",
    sprintf("%*s", widths$measure, "MEASURE"),
    " ",
    sprintf("%*s", widths$se, "S.E."),
    " |"
  )
  separator_block <- paste0("|", strrep("-", block_cell_width + 2L), "|")

  n_scores <- length(score_values)
  block_height <- ceiling(n_scores / columns)
  row_lines <- character(block_height)

  for (row in seq_len(block_height)) {
    chunks <- character(columns)
    for (col in seq_len(columns)) {
      idx <- (col - 1L) * block_height + row
      if (idx <= n_scores) {
        chunks[col] <- paste0(
          "| ",
          build_score_table_cell(
            score = score_labels[idx],
            measure = measures[idx],
            se = ses[idx],
            widths = widths,
            extreme = extreme[idx]
          ),
          " |"
        )
      } else {
        chunks[col] <- paste0("| ", pad_score_table_cell(block_cell_width), " |")
      }
    }
    row_lines[row] <- paste(chunks, collapse = "")
  }

  header_row <- paste(rep(block_header, columns), collapse = "")
  separator_row <- paste(rep(separator_block, columns), collapse = "")
  title_text <- if (is.null(title)) {
    paste("TABLE OF MEASURES ON TEST OF", max_score, if (max_score == 1L) "ITEM" else "ITEMS")
  } else {
    title
  }

  lines <- c(
    title_text,
    separator_row,
    header_row,
    separator_row,
    row_lines,
    separator_row
  )

  structure(
    list(
      lines = lines,
      scores = data.frame(
        score = score_values,
        measure = measures,
        se = ses,
        extreme = extreme
      ),
      settings = list(
        include_extremes = isTRUE(include_extremes),
        columns = columns,
        digits = digits,
        se_digits = se_digits,
        score_digits = score_digits,
        extreme_adjust = extreme_adjust,
        title = title_text
      )
    ),
    class = "ascii_score_table"
  )
}

#' @export
format.ascii_score_table <- function(x, ...) {
  x$lines
}

#' @export
as.character.ascii_score_table <- function(x, ...) {
  paste(format(x, ...), collapse = "\n")
}

#' @export
print.ascii_score_table <- function(x, ...) {
  cat(as.character(x, ...), sep = "\n")
  invisible(x)
}
