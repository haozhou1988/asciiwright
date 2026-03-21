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

build_axis_cell <- function(axis_value, left_marks, right_marks, axis_style = c("single", "double")) {
  axis_style <- match.arg(axis_style)
  axis_body <- if (abs(axis_value - round(axis_value)) < 1e-9) {
    if (axis_style == "double") "++" else "+"
  } else {
    if (axis_style == "double") "||" else "|"
  }
  cell <- c(" ", axis_body, " ")

  if (length(left_marks)) {
    cell[1L] <- left_marks[1L]
  }
  if (length(right_marks)) {
    cell[3L] <- right_marks[1L]
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
    min_score = 0,
    max_score = length(item_measures),
    extreme_adjust = 0.3
) {
  target <- max(min(raw_score, max_score), min_score)

  if (target <= min_score) {
    target <- min_score + extreme_adjust
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
    min_score = 0,
    max_score = length(item_measures),
    extreme_adjust = 0.3
) {
  vapply(
    score_values,
    estimate_total_score_measure,
    numeric(1),
    item_measures = item_measures,
    min_score = min_score,
    max_score = max_score,
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
    extreme_adjust = 0.3,
    min_score = NULL,
    max_score = NULL
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
    resolved_min_score <- if (is.null(min_score)) 0 else min_score
    resolved_max_score <- if (is.null(max_score)) length(item_measures) else max_score
    score_positions <- tryCatch(
      estimate_score_measures(
        score_values = score_values,
        item_measures = item_measures,
        min_score = resolved_min_score,
        max_score = resolved_max_score,
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

  if (width == 1L) {
    return(switch(
      style,
      x = repeat_char,
      hashdot = if (count >= hash_size) pair_char else single_char,
      winsteps = if (hash_size <= 1L) repeat_char else if (count >= hash_size) pair_char else single_char
    ))
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
    left_width,
    right_width,
    axis_text = "|",
    trailing_width = 0L
) {
  paste0(
    strrep(" ", header_width),
    spacer,
    sprintf("%*s", left_width + 2L, left_text),
    axis_text,
    sprintf("%-*s", right_width + 2L + trailing_width, right_text)
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
    "EACH \"#\" IN THE ",
    side_title,
    " COLUMN IS ",
    hash_size,
    ". EACH \".\" IS ",
    dot_meaning,
    "."
  )
}

resolve_table_style <- function(table_style) {
  switch(
    table_style,
    custom = list(
      layout = "persons-left",
      item_scale = "difficulty",
      person_display = "labels",
      item_display = "labels",
      axis_style = "single",
      right_measure = FALSE,
      preset_label_width = 32L,
      preset_line_length = NULL
    ),
    `table1.0` = list(
      layout = "persons-left",
      item_scale = "difficulty",
      person_display = "labels",
      item_display = "labels",
      axis_style = "single",
      right_measure = FALSE,
      preset_label_width = 32L,
      preset_line_length = NULL
    ),
    `table1.1` = list(
      layout = "persons-left",
      item_scale = "difficulty",
      person_display = "distribution",
      item_display = "distribution",
      axis_style = "single",
      right_measure = FALSE,
      preset_label_width = 10L,
      preset_line_length = NULL
    ),
    `table1.2` = list(
      layout = "persons-left",
      item_scale = "difficulty",
      person_display = "distribution",
      item_display = "labels",
      axis_style = "single",
      right_measure = FALSE,
      preset_label_width = 18L,
      preset_line_length = 80L
    ),
    `table1.3` = list(
      layout = "items-left",
      item_scale = "difficulty",
      person_display = "labels",
      item_display = "distribution",
      axis_style = "single",
      right_measure = FALSE,
      preset_label_width = 18L,
      preset_line_length = 80L
    ),
    `table1.10` = list(
      layout = "persons-left",
      item_scale = "easiness",
      person_display = "labels",
      item_display = "labels",
      axis_style = "double",
      right_measure = FALSE,
      preset_label_width = 20L,
      preset_line_length = NULL
    ),
    `table1.11` = list(
      layout = "persons-left",
      item_scale = "easiness",
      person_display = "distribution",
      item_display = "distribution",
      axis_style = "double",
      right_measure = TRUE,
      preset_label_width = 8L,
      preset_line_length = NULL
    ),
    `table1.12` = list(
      layout = "persons-left",
      item_scale = "easiness",
      person_display = "distribution",
      item_display = "labels",
      axis_style = "double",
      right_measure = FALSE,
      preset_label_width = 14L,
      preset_line_length = 80L
    ),
    `table1.13` = list(
      layout = "items-left",
      item_scale = "easiness",
      person_display = "labels",
      item_display = "distribution",
      axis_style = "double",
      right_measure = FALSE,
      preset_label_width = 14L,
      preset_line_length = 80L
    )
  )
}

transform_item_display_measures <- function(item_measures, person_measures, item_scale = c("difficulty", "easiness")) {
  item_scale <- match.arg(item_scale)

  if (item_scale == "difficulty") {
    return(as.numeric(item_measures))
  }

  person_anchor <- mean(person_measures)
  item_anchor <- mean(item_measures)
  person_anchor + item_anchor - item_measures
}

direction_labels_for_side <- function(side_kind = c("person", "item"), item_scale = c("difficulty", "easiness")) {
  side_kind <- match.arg(side_kind)
  item_scale <- match.arg(item_scale)

  if (side_kind == "person") {
    return(list(top = "<more>", bottom = "<less>"))
  }

  if (item_scale == "difficulty") {
    list(top = "<rare>", bottom = "<freq>")
  } else {
    list(top = "<freq>", bottom = "<rare>")
  }
}

resolve_side_widths <- function(
    desired_left_width,
    desired_right_width,
    left_mode,
    right_mode,
    name_trunc,
    fixed_width,
    line_length = NULL
) {
  left_width <- as.integer(desired_left_width)
  right_width <- as.integer(desired_right_width)

  if (is.null(line_length)) {
    return(list(left_width = left_width, right_width = right_width))
  }

  if (!is.numeric(line_length) || length(line_length) != 1L || !is.finite(line_length) || line_length < 20) {
    stop("'line_length' must be NULL or a single number of at least 20.", call. = FALSE)
  }

  available_width <- as.integer(line_length) - fixed_width
  min_left <- if (left_mode == "distribution") 1L else max(4L, min(as.integer(name_trunc), 4L))
  min_right <- if (right_mode == "distribution") 1L else max(4L, min(as.integer(name_trunc), 4L))

  if (available_width < (min_left + min_right)) {
    stop("'line_length' is too small for the requested map layout.", call. = FALSE)
  }

  while ((left_width + right_width) > available_width) {
    left_excess <- left_width - min_left
    right_excess <- right_width - min_right

    if (left_excess <= 0L && right_excess <= 0L) {
      break
    }

    if (left_excess >= right_excess && left_excess > 0L) {
      left_width <- left_width - 1L
    } else if (right_excess > 0L) {
      right_width <- right_width - 1L
    } else {
      left_width <- left_width - 1L
    }
  }

  list(left_width = left_width, right_width = right_width)
}

paginate_wright_lines <- function(header_lines, body_lines, footer_lines = character(), max_page = NULL) {
  if (is.null(max_page) || !is.finite(max_page) || max_page <= 0L) {
    return(list(c(header_lines, body_lines, footer_lines)))
  }

  if (!is.numeric(max_page) || length(max_page) != 1L || max_page < (length(header_lines) + 2L)) {
    stop("'max_page' must allow repeated headers plus at least one body row.", call. = FALSE)
  }

  max_page <- as.integer(max_page)
  final_capacity <- max_page - length(header_lines) - length(footer_lines)
  interim_capacity <- max_page - length(header_lines)

  if (final_capacity < 1L || interim_capacity < 1L) {
    stop("'max_page' is too small for the requested page footer.", call. = FALSE)
  }

  if (length(header_lines) + length(body_lines) + length(footer_lines) <= max_page) {
    return(list(c(header_lines, body_lines, footer_lines)))
  }

  pages <- list()
  remaining <- body_lines

  while (length(remaining) > final_capacity) {
    take <- min(interim_capacity, length(remaining) - final_capacity)
    pages[[length(pages) + 1L]] <- c(header_lines, remaining[seq_len(take)])
    remaining <- remaining[-seq_len(take)]
  }

  pages[[length(pages) + 1L]] <- c(header_lines, remaining, footer_lines)
  pages
}

flatten_wright_pages <- function(pages, page_break = "\f") {
  if (!length(pages)) {
    return(character())
  }

  out <- pages[[1L]]
  if (length(pages) == 1L) {
    return(out)
  }

  for (i in 2:length(pages)) {
    out <- c(out, page_break, pages[[i]])
  }

  out
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
#' @param table_style Optional Winsteps-inspired preset. `"table1.0"` to
#'   `"table1.3"` cover the standard difficulty-oriented layouts and
#'   `"table1.10"` to `"table1.13"` cover mirrored easiness-oriented layouts.
#'   `"custom"` leaves the individual display options untouched.
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
#' @param layout Which side appears on the left: persons or items.
#' @param item_scale Whether item locations are rendered as difficulties or as
#'   mirrored easiness locations.
#' @param line_length Optional maximum output width, similar to Winsteps
#'   `LINELENGTH=`.
#' @param max_page Optional maximum number of output lines per page, similar to
#'   Winsteps `MAXPAGE=`.
#' @param page_break Marker inserted between rendered pages when `max_page`
#'   creates multiple pages.
#' @param measure_header Heading shown above the logit column.
#' @param score_header Heading shown above the optional score column.
#' @param more_label Top-left direction label.
#' @param rare_label Top-right direction label.
#' @param less_label Bottom-left direction label.
#' @param freq_label Bottom-right direction label.
#' @param label_width Total width reserved for each side.
#' @param person_width Optional width override for the person side.
#' @param item_width Optional width override for the item side.
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
    table_style = c(
      "custom",
      "table1.0",
      "table1.1",
      "table1.2",
      "table1.3",
      "table1.10",
      "table1.11",
      "table1.12",
      "table1.13"
    ),
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
    layout = c("persons-left", "items-left"),
    item_scale = c("difficulty", "easiness"),
    line_length = NULL,
    max_page = NULL,
    page_break = "\f",
    measure_header = "MEASURE",
    score_header = "SCORE",
    more_label = "<more>",
    rare_label = "<rare>",
    less_label = "<less>",
    freq_label = "<freq>",
    label_width = 32L,
    person_width = NULL,
    item_width = NULL,
    name_trunc = 8L,
    label_abbrev = c("truncate", "smart"),
    label_overrides = NULL,
    digits = 0L,
    score_digits = 0L
) {
  table_style_missing <- missing(table_style)
  person_display_missing <- missing(person_display)
  item_display_missing <- missing(item_display)
  axis_style_missing <- missing(axis_style)
  right_measure_missing <- missing(right_measure)
  layout_missing <- missing(layout)
  item_scale_missing <- missing(item_scale)
  label_width_missing <- missing(label_width)
  line_length_missing <- missing(line_length)

  person_df <- normalize_wright_input(
    persons,
    labels = person_labels,
    scores = person_scores,
    side = "person"
  )
  item_df <- normalize_wright_input(items, labels = item_labels, side = "item")

  table_style <- match.arg(table_style)
  preset <- resolve_table_style(table_style)
  if (table_style != "custom" || !table_style_missing) {
    if (person_display_missing) {
      person_display <- preset$person_display
    }
    if (item_display_missing) {
      item_display <- preset$item_display
    }
    if (axis_style_missing) {
      axis_style <- preset$axis_style
    }
    if (right_measure_missing) {
      right_measure <- preset$right_measure
    }
    if (layout_missing) {
      layout <- preset$layout
    }
    if (item_scale_missing) {
      item_scale <- preset$item_scale
    }
    if (label_width_missing) {
      label_width <- preset$preset_label_width
    }
    if (line_length_missing) {
      line_length <- preset$preset_line_length
    }
  }

  person_display <- match.arg(person_display)
  item_display <- match.arg(item_display)
  distribution_style <- match.arg(distribution_style)
  axis_style <- match.arg(axis_style)
  score_method <- match.arg(score_method)
  label_abbrev <- match.arg(label_abbrev)
  layout <- match.arg(layout)
  item_scale <- match.arg(item_scale)

  if (!is.numeric(lines_per_logit) || length(lines_per_logit) != 1L || lines_per_logit < 1) {
    stop("'lines_per_logit' must be a positive integer.", call. = FALSE)
  }

  lines_per_logit <- as.integer(lines_per_logit)
  label_width <- as.integer(label_width)
  if (!is.null(person_width)) {
    person_width <- as.integer(person_width)
  }
  if (!is.null(item_width)) {
    item_width <- as.integer(item_width)
  }
  name_trunc <- as.integer(name_trunc)
  digits <- as.integer(digits)
  score_digits <- as.integer(score_digits)

  item_display_df <- item_df
  item_display_df$measure <- transform_item_display_measures(
    item_measures = item_df$measure,
    person_measures = person_df$measure,
    item_scale = item_scale
  )

  all_values <- c(person_df$measure, item_display_df$measure)

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
  item_display_df$row <- measure_to_row(item_display_df$measure, top = upper, step = step, n_rows = n_rows)

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
    extreme_adjust = score_extreme_adjust,
    min_score = 0,
    max_score = length(item_df$measure)
  )

  header_width <- measure_col_width + if (score_rows$has_scores) score_col_width + 1L else 0L
  spacer <- "  "
  axis_col_width <- if (axis_style == "double") 4L else 3L
  right_measure_width <- if (isTRUE(right_measure)) measure_col_width + nchar(spacer) else 0L
  fixed_width <- header_width + nchar(spacer) + 1L + axis_col_width + 1L + right_measure_width

  resolved_person_width <- if (is.null(person_width)) label_width else person_width
  resolved_item_width <- if (is.null(item_width)) label_width else item_width

  left_kind <- if (layout == "persons-left") "person" else "item"
  right_kind <- if (layout == "persons-left") "item" else "person"
  left_title <- if (layout == "persons-left") person_title else item_title
  right_title <- if (layout == "persons-left") item_title else person_title
  left_mode <- if (layout == "persons-left") person_display else item_display
  right_mode <- if (layout == "persons-left") item_display else person_display
  left_hash_override <- if (layout == "persons-left") person_hash_size else item_hash_size
  right_hash_override <- if (layout == "persons-left") item_hash_size else person_hash_size
  desired_left_width <- if (layout == "persons-left") resolved_person_width else resolved_item_width
  desired_right_width <- if (layout == "persons-left") resolved_item_width else resolved_person_width

  widths <- resolve_side_widths(
    desired_left_width = desired_left_width,
    desired_right_width = desired_right_width,
    left_mode = left_mode,
    right_mode = right_mode,
    name_trunc = name_trunc,
    fixed_width = fixed_width,
    line_length = line_length
  )

  left_df <- if (layout == "persons-left") person_df else item_display_df
  right_df <- if (layout == "persons-left") item_display_df else person_df
  left_side <- build_side_rows(
    data = left_df,
    n_rows = n_rows,
    width = widths$left_width,
    mode = left_mode,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = left_hash_override,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = left_kind
  )
  right_side <- build_side_rows(
    data = right_df,
    n_rows = n_rows,
    width = widths$right_width,
    mode = right_mode,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = right_hash_override,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = right_kind
  )

  left_stats <- stat_rows(left_df$measure, top = upper, step = step, n_rows = n_rows)
  right_stats <- stat_rows(right_df$measure, top = upper, step = step, n_rows = n_rows)

  person_direction <- list(top = more_label, bottom = less_label)
  item_direction <- if (item_scale == "difficulty") {
    list(top = rare_label, bottom = freq_label)
  } else {
    list(top = freq_label, bottom = rare_label)
  }
  left_direction <- if (left_kind == "person") person_direction else item_direction
  right_direction <- if (right_kind == "person") person_direction else item_direction

  notes <- c(
    if (left_side$compressed) build_distribution_note(left_title, left_side$hash_size),
    if (right_side$compressed) build_distribution_note(right_title, right_side$hash_size)
  )

  map_width <- widths$left_width + 1L + axis_col_width + 1L + widths$right_width
  title_right <- truncate_text(paste(left_title, "-", "MAP", "-", right_title), map_width)
  header_text <- if (score_rows$has_scores) {
    paste0(
      sprintf("%*s", score_col_width, score_header),
      " ",
      sprintf("%*s", measure_col_width, measure_header)
    )
  } else {
    sprintf("%*s", measure_col_width, measure_header)
  }

  header_lines <- character(2L)
  header_lines[1L] <- paste0(
    header_text,
    spacer,
    sprintf("%-*s", map_width, title_right),
    if (isTRUE(right_measure)) paste0(spacer, sprintf("%*s", measure_col_width, measure_header)) else ""
  )

  header_lines[2L] <- build_axis_aligned_line(
    left_text = left_direction$top,
    right_text = right_direction$top,
    header_width = header_width,
    spacer = spacer,
    left_width = widths$left_width,
    right_width = widths$right_width,
    axis_text = if (axis_style == "double") "-++-" else "|",
    trailing_width = right_measure_width
  )

  body_lines <- character(n_rows)
  for (i in seq_len(n_rows)) {
    left_marks <- c(
      if (left_stats["T_top"] == i || left_stats["T_bottom"] == i) "T",
      if (left_stats["S_top"] == i || left_stats["S_bottom"] == i) "S",
      if (left_stats["M"] == i) "M"
    )
    right_marks <- c(
      if (right_stats["T_top"] == i || right_stats["T_bottom"] == i) "T",
      if (right_stats["S_top"] == i || right_stats["S_bottom"] == i) "S",
      if (right_stats["M"] == i) "M"
    )

    axis_text <- build_axis_cell(
      axis_value = axis_values[i],
      left_marks = left_marks,
      right_marks = right_marks,
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

    body_lines[i] <- paste0(
      score_prefix,
      measure_text,
      spacer,
      left_side$render(i, align = "right"),
      " ",
      axis_text,
      " ",
      right_side$render(i, align = "left"),
      right_measure_text
    )
  }

  footer_lines <- build_axis_aligned_line(
    left_text = left_direction$bottom,
    right_text = right_direction$bottom,
    header_width = header_width,
    spacer = spacer,
    left_width = widths$left_width,
    right_width = widths$right_width,
    axis_text = if (axis_style == "double") "-++-" else "|",
    trailing_width = right_measure_width
  )

  if (length(notes)) {
    footer_lines <- c(
      footer_lines,
      vapply(
        notes,
        function(note) paste0(strrep(" ", header_width + nchar(spacer)), note),
        character(1)
      )
    )
  }

  pages <- paginate_wright_lines(
    header_lines = header_lines,
    body_lines = body_lines,
    footer_lines = footer_lines,
    max_page = max_page
  )
  lines <- flatten_wright_pages(pages = pages, page_break = page_break)

  structure(
    list(
      lines = lines,
      pages = pages,
      axis_values = axis_values,
      persons = person_df,
      items = item_display_df,
      items_original = item_df,
      settings = list(
        table_style = table_style,
        lines_per_logit = lines_per_logit,
        measure_range = c(lower, upper),
        label_width = label_width,
        person_width = resolved_person_width,
        item_width = resolved_item_width,
        left_width = widths$left_width,
        right_width = widths$right_width,
        measure_col_width = measure_col_width,
        score_col_width = score_col_width,
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
        person_hash_size = if (layout == "persons-left") left_side$hash_size else right_side$hash_size,
        item_hash_size = if (layout == "persons-left") right_side$hash_size else left_side$hash_size,
        has_scores = score_rows$has_scores,
        axis_style = axis_style,
        right_measure = isTRUE(right_measure),
        layout = layout,
        item_scale = item_scale,
        line_length = line_length,
        max_page = max_page,
        page_break = page_break,
        page_count = length(pages)
      )
    ),
    class = "ascii_wright_map"
  )
}

#' @export
format.ascii_wright_map <- function(x, ..., page = NULL, page_break = NULL) {
  if (is.null(x$pages)) {
    return(x$lines)
  }

  if (!is.null(page)) {
    if (!is.numeric(page) || length(page) != 1L || page < 1L || page > length(x$pages)) {
      stop("'page' must select one of the available pages.", call. = FALSE)
    }
    return(x$pages[[as.integer(page)]])
  }

  resolved_page_break <- if (is.null(page_break)) x$settings$page_break else page_break
  flatten_wright_pages(x$pages, page_break = resolved_page_break)
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
