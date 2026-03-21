normalize_wright_input <- function(
    x,
    labels = NULL,
    scores = NULL,
    side = c("person", "item")
) {
  side <- match.arg(side)
  default_measure <- if (side == "person") "ability" else "difficulty"

  if (is.data.frame(x)) {
    has_measure <- "measure" %in% names(x)
    has_default <- default_measure %in% names(x)
    measure_col <- if (has_measure) "measure" else if (has_default) default_measure else NULL

    if (is.null(measure_col)) {
      stop(
        "Data frame inputs must contain a 'measure' column or a side-specific column.",
        call. = FALSE
      )
    }

    label_col <- if ("label" %in% names(x)) "label" else NULL
    inferred_scores <- if ("score" %in% names(x)) x[["score"]] else NULL
    values <- x[[measure_col]]
    inferred_labels <- if (!is.null(label_col)) x[[label_col]] else rownames(x)
  } else if (is.numeric(x)) {
    values <- unname(x)
    inferred_labels <- names(x)
    inferred_scores <- NULL
  } else {
    stop("Inputs must be numeric vectors or data frames.", call. = FALSE)
  }

  if (!is.null(labels)) {
    inferred_labels <- labels
  }
  if (!is.null(scores)) {
    inferred_scores <- scores
  }

  if (length(values) == 0L) {
    stop("At least one measure is required on each side.", call. = FALSE)
  }

  if (is.null(inferred_labels)) {
    inferred_labels <- paste0(substr(side, 1L, 1L), seq_along(values))
  }

  if (length(inferred_labels) != length(values)) {
    stop("Labels must have the same length as measures.", call. = FALSE)
  }

  if (!is.null(inferred_scores) && length(inferred_scores) != length(values)) {
    stop("Scores must have the same length as person measures.", call. = FALSE)
  }

  keep <- is.finite(values)
  if (!all(keep)) {
    values <- values[keep]
    inferred_labels <- inferred_labels[keep]
    if (!is.null(inferred_scores)) {
      inferred_scores <- inferred_scores[keep]
    }
  }

  if (!length(values)) {
    stop("All measures were missing or non-finite.", call. = FALSE)
  }

  if (is.null(inferred_scores)) {
    inferred_scores <- rep(NA_real_, length(values))
  }

  data.frame(
    label = as.character(inferred_labels),
    measure = as.numeric(values),
    score = inferred_scores,
    stringsAsFactors = FALSE
  )
}

truncate_tokens <- function(tokens, width) {
  if (!length(tokens)) {
    return("")
  }

  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens) || width <= 0L) {
    return("")
  }

  out <- ""
  for (token in tokens) {
    candidate <- if (nzchar(out)) paste(out, token) else token
    if (nchar(candidate, type = "width") <= width) {
      out <- candidate
      next
    }

    if (!nzchar(out)) {
      clipped <- substr(token, 1L, max(1L, width - 1L))
      if (nchar(token, type = "width") > width) {
        clipped <- paste0(substr(clipped, 1L, max(1L, width - 1L)), ".")
      }
      return(substr(clipped, 1L, width))
    }

    if (width >= 3L && nchar(out, type = "width") <= width - 3L) {
      out <- paste0(substr(out, 1L, width - 3L), "...")
    }
    return(substr(out, 1L, width))
  }

  substr(out, 1L, width)
}

truncate_text <- function(text, width) {
  text <- trimws(text)
  if (!nzchar(text) || width <= 0L) {
    return("")
  }
  if (nchar(text, type = "width") <= width) {
    return(text)
  }
  if (width == 1L) {
    return(substr(text, 1L, 1L))
  }
  paste0(substr(text, 1L, width - 1L), ".")
}

normalize_label_text <- function(text) {
  trimws(gsub("\\s+", " ", as.character(text)))
}

drop_label_stopwords <- function(tokens) {
  stopwords <- c("a", "an", "and", "at", "for", "from", "in", "of", "on", "or", "the", "to", "with")
  keep <- !(tolower(tokens) %in% stopwords)
  if (!any(keep)) {
    tokens
  } else {
    tokens[keep]
  }
}

compact_label_word <- function(word, level = 0L) {
  if (!nzchar(word) || grepl("^[[:digit:].-]+$", word)) {
    return(word)
  }

  if (level <= 0L) {
    return(word)
  }

  if (nchar(word, type = "width") <= 3L) {
    return(if (level >= 3L) substr(word, 1L, 1L) else word)
  }

  chars <- strsplit(word, "", fixed = TRUE)[[1]]
  if (length(chars) <= 2L) {
    squeezed <- word
  } else {
    middle <- chars[2:(length(chars) - 1L)]
    keep_middle <- middle[!(tolower(middle) %in% c("a", "e", "i", "o", "u"))]
    if (!length(keep_middle)) {
      keep_middle <- middle[1L]
    }
    squeezed <- paste0(chars[1L], paste0(keep_middle, collapse = ""), chars[length(chars)])
  }

  if (level == 1L) {
    return(squeezed)
  }
  if (level == 2L) {
    return(substr(squeezed, 1L, max(3L, min(4L, nchar(squeezed, type = "width")))))
  }

  substr(word, 1L, 1L)
}

compress_label_tokens <- function(tokens, width) {
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens) || width <= 0L) {
    return("")
  }

  current_tokens <- tokens
  levels <- integer(length(current_tokens))

  repeat {
    rendered_tokens <- mapply(compact_label_word, current_tokens, levels, USE.NAMES = FALSE)
    candidate <- paste(rendered_tokens, collapse = " ")
    if (nchar(candidate, type = "width") <= width) {
      return(candidate)
    }

    compressible <- which(levels < 3L & nchar(rendered_tokens, type = "width") > 1L)
    if (length(compressible)) {
      idx <- compressible[which.max(nchar(rendered_tokens[compressible], type = "width"))]
      levels[idx] <- levels[idx] + 1L
      next
    }

    if (length(current_tokens) > 1L) {
      removable <- if (length(current_tokens) > 2L) 2:(length(current_tokens) - 1L) else seq_along(current_tokens)
      idx <- removable[which.min(nchar(current_tokens[removable], type = "width"))]
      current_tokens <- current_tokens[-idx]
      levels <- levels[-idx]
      next
    }

    break
  }

  truncate_text(paste(tokens, collapse = " "), width)
}

apply_label_override <- function(label, width, side, label_overrides = NULL) {
  if (is.null(label_overrides)) {
    return(label)
  }

  if (is.function(label_overrides)) {
    replacement <- tryCatch(
      label_overrides(label = label, width = width, side = side),
      error = function(e) label_overrides(label)
    )
    if (length(replacement) == 1L && !is.na(replacement) && nzchar(as.character(replacement))) {
      return(as.character(replacement))
    }
    return(label)
  }

  if (is.character(label_overrides) && !is.null(names(label_overrides)) && label %in% names(label_overrides)) {
    return(as.character(unname(label_overrides[[label]])))
  }

  label
}

abbreviate_label_text <- function(
    label,
    width,
    style = c("truncate", "smart"),
    label_overrides = NULL,
    side = "item"
) {
  style <- match.arg(style)
  label <- normalize_label_text(label)
  label <- normalize_label_text(apply_label_override(label, width = width, side = side, label_overrides = label_overrides))

  if (nchar(label, type = "width") <= width) {
    return(label)
  }

  if (style == "truncate") {
    return(truncate_text(label, width))
  }

  tokens <- strsplit(label, "\\s+")[[1]]
  reduced <- drop_label_stopwords(tokens)

  raw_candidates <- list(tokens)
  if (!identical(reduced, tokens)) {
    raw_candidates[[length(raw_candidates) + 1L]] <- reduced
  }

  for (candidate_tokens in raw_candidates) {
    candidate <- paste(candidate_tokens, collapse = " ")
    if (nchar(candidate, type = "width") <= width) {
      return(candidate)
    }
  }

  compressed_candidates <- if (!identical(reduced, tokens)) {
    list(reduced, tokens)
  } else {
    list(tokens)
  }

  for (candidate_tokens in compressed_candidates) {
    candidate <- compress_label_tokens(candidate_tokens, width = width)
    if (nchar(candidate, type = "width") <= width) {
      return(candidate)
    }
  }

  truncate_text(label, width)
}

format_measure_label <- function(value, digits = 0L, width = 7L) {
  formatC(value, format = "f", digits = digits, width = width)
}

measure_to_row <- function(values, top, step, n_rows) {
  index <- round((top - values) / step) + 1L
  pmin.int(pmax.int(index, 1L), n_rows)
}

stat_rows <- function(values, top, step, n_rows) {
  mean_value <- mean(values)
  sd_value <- stats::sd(values)

  stats <- c(
    T = mean_value + 2 * sd_value,
    S = mean_value + 1 * sd_value,
    M = mean_value,
    S = mean_value - 1 * sd_value,
    T = mean_value - 2 * sd_value
  )

  rows <- measure_to_row(stats, top = top, step = step, n_rows = n_rows)
  names(rows) <- c("T_top", "S_top", "M", "S_bottom", "T_bottom")
  rows
}

build_axis_cell <- function(axis_value, person_marks, item_marks, axis_style = c("single", "double")) {
  axis_style <- match.arg(axis_style)
  axis_body <- if (abs(axis_value - round(axis_value)) < 1e-9) {
    if (axis_style == "double") "++" else "+"
  } else {
    if (axis_style == "double") "||" else "|"
  }
  cell <- c(" ", axis_body, " ")

  if (length(person_marks)) {
    cell[1L] <- person_marks[1L]
  }
  if (length(item_marks)) {
    cell[3L] <- item_marks[1L]
  }

  paste(cell, collapse = "")
}

format_score_values <- function(x, digits = 0L) {
  if (!length(x)) {
    return(character())
  }

  if (is.numeric(x)) {
    out <- if (digits == 0L && all(abs(x[!is.na(x)] - round(x[!is.na(x)])) < 1e-9)) {
      as.character(as.integer(round(x)))
    } else {
      formatC(x, format = "f", digits = digits)
    }
  } else {
    out <- as.character(x)
  }

  out[is.na(x)] <- NA_character_
  out
}

sort_tokens <- function(tokens) {
  if (!length(tokens)) {
    return(tokens)
  }

  tokens <- unique(tokens)
  numeric_tokens <- suppressWarnings(as.numeric(tokens))
  if (all(!is.na(numeric_tokens))) {
    tokens[order(numeric_tokens, decreasing = TRUE)]
  } else {
    sort(tokens, decreasing = TRUE)
  }
}

estimate_total_score_measure <- function(
    raw_score,
    item_measures,
    extreme_adjust = 0.3
) {
  max_score <- length(item_measures)
  target <- max(min(raw_score, max_score), 0)

  if (target <= 0) {
    target <- extreme_adjust
  } else if (target >= max_score) {
    target <- max_score - extreme_adjust
  }

  score_function <- function(theta) {
    sum(stats::plogis(theta - item_measures)) - target
  }

  lower <- min(item_measures) - 10
  upper <- max(item_measures) + 10

  while (score_function(lower) > 0) {
    lower <- lower - 5
  }
  while (score_function(upper) < 0) {
    upper <- upper + 5
  }

  stats::uniroot(score_function, lower = lower, upper = upper)$root
}

estimate_score_measures <- function(
    score_values,
    item_measures,
    extreme_adjust = 0.3
) {
  vapply(
    score_values,
    estimate_total_score_measure,
    numeric(1),
    item_measures = item_measures,
    extreme_adjust = extreme_adjust
  )
}

build_score_rows <- function(
    data,
    top,
    step,
    n_rows,
    width,
    digits = 0L,
    method = c("rasch", "observed"),
    item_measures = NULL,
    extreme_adjust = 0.3
) {
  method <- match.arg(method)

  if (width <= 0L || all(is.na(data$score))) {
    return(list(
      has_scores = FALSE,
      width = 0L,
      render = function(i) "",
      method = NA_character_
    ))
  }

  score_labels <- format_score_values(data$score, digits = digits)
  keep <- !is.na(score_labels)
  grouped_measures <- split(data$measure[keep], score_labels[keep])
  score_positions <- NULL
  method_used <- method

  if (
    method == "rasch" &&
    is.numeric(data$score) &&
    !is.null(item_measures) &&
    length(item_measures) > 0L
  ) {
    score_values <- vapply(split(data$score[keep], score_labels[keep]), mean, numeric(1))
    score_positions <- tryCatch(
      estimate_score_measures(
        score_values = score_values,
        item_measures = item_measures,
        extreme_adjust = extreme_adjust
      ),
      error = function(e) NULL
    )
  }

  if (is.null(score_positions)) {
    score_positions <- vapply(grouped_measures, mean, numeric(1))
    method_used <- "observed"
  }

  score_rows <- split(
    names(score_positions),
    measure_to_row(score_positions, top = top, step = step, n_rows = n_rows)
  )
  score_rows <- lapply(score_rows, sort_tokens)

  list(
    has_scores = TRUE,
    width = width,
    render = function(i) {
      tokens <- score_rows[[as.character(i)]]
      if (is.null(tokens)) {
        return(sprintf("%*s", width, ""))
      }
      sprintf("%*s", width, truncate_tokens(tokens, width))
    },
    method = method_used
  )
}

infer_hash_size <- function(counts, width, style, hash_size = NULL) {
  if (!is.null(hash_size)) {
    if (!is.numeric(hash_size) || length(hash_size) != 1L || hash_size < 1) {
      stop("Hash sizes must be positive integers.", call. = FALSE)
    }
    return(as.integer(hash_size))
  }

  if (style == "x") {
    return(1L)
  }
  if (style == "hashdot") {
    return(2L)
  }

  max_count <- max(counts, 0L)
  if (max_count <= width) {
    1L
  } else {
    max(2L, ceiling(max_count / width))
  }
}

format_distribution_tokens <- function(
    count,
    width,
    style = c("winsteps", "hashdot", "x"),
    hash_size = 1L,
    pair_char = "#",
    single_char = ".",
    repeat_char = "X",
    overflow_char = "+"
) {
  style <- match.arg(style)

  if (!count || width <= 0L) {
    return("")
  }

  raw <- switch(
    style,
    x = strrep(repeat_char, count),
    hashdot = {
      full <- count %/% hash_size
      remainder <- count %% hash_size
      paste0(if (remainder > 0L) single_char else "", strrep(pair_char, full))
    },
    winsteps = {
      if (hash_size <= 1L) {
        strrep(repeat_char, count)
      } else {
        full <- count %/% hash_size
        remainder <- count %% hash_size
        paste0(if (remainder > 0L) single_char else "", strrep(pair_char, full))
      }
    }
  )

  if (!nzchar(raw)) {
    raw <- single_char
  }

  if (nchar(raw, type = "width") <= width) {
    return(raw)
  }

  if (width == 1L) {
    return(overflow_char)
  }

  paste0(substr(raw, 1L, width - 1L), overflow_char)
}

build_side_rows <- function(
    data,
    n_rows,
    width,
    mode = c("labels", "distribution"),
    distribution_style = c("winsteps", "hashdot", "x"),
    name_trunc = 8L,
    hash_size = NULL,
    label_abbrev = c("truncate", "smart"),
    label_overrides = NULL,
    side = "item"
) {
  mode <- match.arg(mode)
  distribution_style <- match.arg(distribution_style)
  label_abbrev <- match.arg(label_abbrev)

  if (mode == "labels") {
    rendered_labels <- vapply(
      data$label,
      abbreviate_label_text,
      character(1),
      width = name_trunc,
      style = label_abbrev,
      label_overrides = label_overrides,
      side = side
    )
    rows <- split(rendered_labels, data$row)
    rows <- lapply(rows, sort)

    return(list(
      render = function(i, align = c("right", "left")) {
        align <- match.arg(align)
        tokens <- rows[[as.character(i)]]
        if (is.null(tokens)) {
          tokens <- character()
        }
        text <- truncate_tokens(tokens, width)
        if (align == "right") {
          sprintf("%*s", width, text)
        } else {
          sprintf("%-*s", width, text)
        }
      },
      hash_size = NA_integer_,
      compressed = FALSE
    ))
  }

  counts <- tabulate(data$row, nbins = n_rows)
  resolved_hash_size <- infer_hash_size(
    counts = counts,
    width = width,
    style = distribution_style,
    hash_size = hash_size
  )

  list(
    render = function(i, align = c("right", "left")) {
      align <- match.arg(align)
      text <- format_distribution_tokens(
        count = counts[i],
        width = width,
        style = distribution_style,
        hash_size = resolved_hash_size
      )
      if (align == "right") {
        sprintf("%*s", width, text)
      } else {
        sprintf("%-*s", width, text)
      }
    },
    hash_size = resolved_hash_size,
    compressed = distribution_style == "winsteps" && resolved_hash_size > 1L
  )
}

build_axis_aligned_line <- function(
    left_text,
    right_text,
    header_width,
    spacer,
    side_width,
    axis_text = "|",
    trailing_width = 0L
) {
  paste0(
    strrep(" ", header_width),
    spacer,
    sprintf("%*s", side_width + 2L, left_text),
    axis_text,
    sprintf("%-*s", side_width + 2L + trailing_width, right_text)
  )
}

build_distribution_note <- function(side_title, hash_size) {
  if (is.na(hash_size) || hash_size <= 1L) {
    return(NULL)
  }

  dot_meaning <- if (hash_size == 2L) {
    "1"
  } else {
    paste0("1 TO ", hash_size - 1L)
  }

  paste0(
    side_title,
    ": EACH \"#\" IS ",
    hash_size,
    "; EACH \".\" IS ",
    dot_meaning
  )
}

#' Create a Winsteps-inspired ASCII Wright map
#'
#' @param persons Numeric vector or data frame of person measures.
#' @param items Numeric vector or data frame of item measures.
#' @param person_labels Optional labels for the persons.
#' @param item_labels Optional labels for the items.
#' @param person_scores Optional raw person scores. When `persons` is a data
#'   frame, a `score` column is used automatically unless overridden here. Each
#'   unique score is shown once at the representative location of that score.
#' @param score_method How to place raw score labels on the latent scale.
#'   `"rasch"` estimates the score location from the item difficulties under a
#'   dichotomous Rasch model; `"observed"` uses the average observed person
#'   measure for each score group.
#' @param score_extreme_adjust Adjustment used for extreme raw scores when
#'   `score_method = "rasch"`.
#' @param lines_per_logit Number of text rows used for one logit interval.
#' @param measure_range Optional numeric vector of length 2 giving c(min, max).
#' @param person_title Heading shown for the person side.
#' @param item_title Heading shown for the item side.
#' @param person_display How to draw the person side: `"labels"` or `"distribution"`.
#' @param item_display How to draw the item side: `"labels"` or `"distribution"`.
#' @param distribution_style How distribution counts are compacted:
#'   `"winsteps"` uses `X` when counts fit and automatically switches to `#`/`.`
#'   when they do not, `"hashdot"` always uses grouped `#`/`.`, and `"x"` uses
#'   one `X` per case.
#' @param person_hash_size Optional override for the number of persons
#'   represented by one `#` in grouped distribution displays.
#' @param item_hash_size Optional override for the number of items represented
#'   by one `#` in grouped distribution displays.
#' @param axis_style Whether to render the center as a single axis (`"+"` / `"|"`)
#'   or a double axis (`"++"` / `"||"`), closer to Winsteps mirrored tables.
#' @param right_measure Show a mirrored measure column on the far right.
#' @param measure_header Heading shown above the logit column.
#' @param score_header Heading shown above the optional score column.
#' @param more_label Top-left direction label.
#' @param rare_label Top-right direction label.
#' @param less_label Bottom-left direction label.
#' @param freq_label Bottom-right direction label.
#' @param label_width Total width reserved for each side.
#' @param name_trunc Maximum width of each individual label.
#' @param label_abbrev Label abbreviation strategy. `"truncate"` clips labels at
#'   the available width; `"smart"` removes low-information words and compresses
#'   longer words before truncating.
#' @param label_overrides Optional user-supplied label abbreviations. This can
#'   be a named character vector of exact replacements or a function. Functions
#'   receive `label`, `width`, and `side`.
#' @param digits Digits for printed measures.
#' @param score_digits Digits for printed score labels when scores are numeric.
#' @return An object of class `ascii_wright_map`.
#' @export
wright_map_ascii <- function(
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
) {
  person_df <- normalize_wright_input(
    persons,
    labels = person_labels,
    scores = person_scores,
    side = "person"
  )
  item_df <- normalize_wright_input(items, labels = item_labels, side = "item")

  person_display <- match.arg(person_display)
  item_display <- match.arg(item_display)
  distribution_style <- match.arg(distribution_style)
  axis_style <- match.arg(axis_style)
  score_method <- match.arg(score_method)
  label_abbrev <- match.arg(label_abbrev)

  if (!is.numeric(lines_per_logit) || length(lines_per_logit) != 1L || lines_per_logit < 1) {
    stop("'lines_per_logit' must be a positive integer.", call. = FALSE)
  }

  lines_per_logit <- as.integer(lines_per_logit)
  label_width <- as.integer(label_width)
  name_trunc <- as.integer(name_trunc)
  digits <- as.integer(digits)
  score_digits <- as.integer(score_digits)

  all_values <- c(person_df$measure, item_df$measure)

  if (is.null(measure_range)) {
    lower <- floor(min(all_values))
    upper <- ceiling(max(all_values))
  } else {
    if (!is.numeric(measure_range) || length(measure_range) != 2L) {
      stop("'measure_range' must be NULL or a numeric vector of length 2.", call. = FALSE)
    }
    lower <- min(measure_range)
    upper <- max(measure_range)
  }

  step <- 1 / lines_per_logit
  axis_values <- seq(from = upper, to = lower, by = -step)
  n_rows <- length(axis_values)

  person_df$row <- measure_to_row(person_df$measure, top = upper, step = step, n_rows = n_rows)
  item_df$row <- measure_to_row(item_df$measure, top = upper, step = step, n_rows = n_rows)

  person_side <- build_side_rows(
    data = person_df,
    n_rows = n_rows,
    width = label_width,
    mode = person_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = person_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "person"
  )
  item_side <- build_side_rows(
    data = item_df,
    n_rows = n_rows,
    width = label_width,
    mode = item_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = item_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "item"
  )

  person_stats <- stat_rows(person_df$measure, top = upper, step = step, n_rows = n_rows)
  item_stats <- stat_rows(item_df$measure, top = upper, step = step, n_rows = n_rows)

  measure_col_width <- max(
    7L,
    nchar(measure_header, type = "width"),
    max(nchar(formatC(axis_values, format = "f", digits = digits), type = "width"))
  )

  score_text <- format_score_values(person_df$score, digits = score_digits)
  has_scores <- any(!is.na(score_text))
  score_col_width <- if (has_scores) {
    max(
      5L,
      nchar(score_header, type = "width"),
      max(nchar(score_text[!is.na(score_text)], type = "width"))
    )
  } else {
    0L
  }
  score_rows <- build_score_rows(
    person_df,
    top = upper,
    step = step,
    n_rows = n_rows,
    width = score_col_width,
    digits = score_digits,
    method = score_method,
    item_measures = item_df$measure,
    extreme_adjust = score_extreme_adjust
  )

  header_width <- measure_col_width + if (score_rows$has_scores) score_col_width + 1L else 0L
  spacer <- "  "
  axis_col_width <- if (axis_style == "double") 4L else 3L
  right_measure_width <- if (isTRUE(right_measure)) measure_col_width + nchar(spacer) else 0L
  total_width <- header_width + nchar(spacer) + label_width + 1L + axis_col_width + 1L + label_width + right_measure_width

  notes <- c(
    if (person_side$compressed) build_distribution_note(person_title, person_side$hash_size),
    if (item_side$compressed) build_distribution_note(item_title, item_side$hash_size)
  )

  lines <- character(length = n_rows + 3L + length(notes))
  title_right <- paste(person_title, "-", "MAP", "-", item_title)
  header_text <- if (score_rows$has_scores) {
    paste0(
      sprintf("%*s", score_col_width, score_header),
      " ",
      sprintf("%*s", measure_col_width, measure_header)
    )
  } else {
    sprintf("%*s", measure_col_width, measure_header)
  }

  lines[1L] <- paste0(
    header_text,
    spacer,
    sprintf("%-*s", total_width - header_width - nchar(spacer) - right_measure_width, title_right),
    if (isTRUE(right_measure)) paste0(spacer, sprintf("%*s", measure_col_width, measure_header)) else ""
  )

  lines[2L] <- build_axis_aligned_line(
    left_text = more_label,
    right_text = rare_label,
    header_width = header_width,
    spacer = spacer,
    side_width = label_width,
    axis_text = if (axis_style == "double") "-++-" else "|",
    trailing_width = right_measure_width
  )

  for (i in seq_len(n_rows)) {
    person_marks <- c(
      if (person_stats["T_top"] == i || person_stats["T_bottom"] == i) "T",
      if (person_stats["S_top"] == i || person_stats["S_bottom"] == i) "S",
      if (person_stats["M"] == i) "M"
    )
    item_marks <- c(
      if (item_stats["T_top"] == i || item_stats["T_bottom"] == i) "T",
      if (item_stats["S_top"] == i || item_stats["S_bottom"] == i) "S",
      if (item_stats["M"] == i) "M"
    )

    axis_text <- build_axis_cell(
      axis_value = axis_values[i],
      person_marks = person_marks,
      item_marks = item_marks,
      axis_style = axis_style
    )

    measure_text <- if (abs(axis_values[i] - round(axis_values[i])) < 1e-9) {
      format_measure_label(axis_values[i], digits = digits, width = measure_col_width)
    } else {
      strrep(" ", measure_col_width)
    }

    score_prefix <- if (score_rows$has_scores) {
      paste0(score_rows$render(i), " ")
    } else {
      ""
    }
    right_measure_text <- if (isTRUE(right_measure)) {
      paste0(
        spacer,
        if (abs(axis_values[i] - round(axis_values[i])) < 1e-9) {
          format_measure_label(axis_values[i], digits = digits, width = measure_col_width)
        } else {
          strrep(" ", measure_col_width)
        }
      )
    } else {
      ""
    }

    lines[i + 2L] <- paste0(
      score_prefix,
      measure_text,
      spacer,
      person_side$render(i, align = "right"),
      " ",
      axis_text,
      " ",
      item_side$render(i, align = "left"),
      right_measure_text
    )
  }

  bottom_index <- n_rows + 3L
  lines[bottom_index] <- build_axis_aligned_line(
    left_text = less_label,
    right_text = freq_label,
    header_width = header_width,
    spacer = spacer,
    side_width = label_width,
    axis_text = if (axis_style == "double") "-++-" else "|",
    trailing_width = right_measure_width
  )

  if (length(notes)) {
    for (j in seq_along(notes)) {
      lines[bottom_index + j] <- paste0(strrep(" ", header_width + nchar(spacer)), notes[j])
    }
  }

  structure(
    list(
      lines = lines,
      axis_values = axis_values,
      persons = person_df,
      items = item_df,
      settings = list(
        lines_per_logit = lines_per_logit,
        measure_range = c(lower, upper),
        label_width = label_width,
        name_trunc = name_trunc,
        label_abbrev = label_abbrev,
        digits = digits,
        score_digits = score_digits,
        score_method = score_method,
        score_method_used = score_rows$method,
        score_extreme_adjust = score_extreme_adjust,
        person_display = person_display,
        item_display = item_display,
        distribution_style = distribution_style,
        person_hash_size = person_side$hash_size,
        item_hash_size = item_side$hash_size,
        has_scores = score_rows$has_scores,
        axis_style = axis_style,
        right_measure = isTRUE(right_measure)
      )
    ),
    class = "ascii_wright_map"
  )
}

#' @export
format.ascii_wright_map <- function(x, ...) {
  x$lines
}

#' @export
as.character.ascii_wright_map <- function(x, ...) {
  paste(format(x, ...), collapse = "\n")
}

#' @export
print.ascii_wright_map <- function(x, ...) {
  cat(as.character(x, ...), sep = "\n")
  invisible(x)
}
