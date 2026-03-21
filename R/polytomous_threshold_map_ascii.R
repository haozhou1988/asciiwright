normalize_polytomous_steps <- function(steps, n_items, item_labels = NULL) {
  if (is.null(steps)) {
    stop("'steps' is required for polytomous threshold maps.", call. = FALSE)
  }

  out <- NULL

  if (is.numeric(steps) && is.null(dim(steps))) {
    out <- rep(list(as.numeric(steps)), n_items)
  } else if (is.matrix(steps) || is.data.frame(steps)) {
    if (nrow(steps) != n_items) {
      stop("Step matrices/data frames must have one row per item.", call. = FALSE)
    }
    out <- lapply(seq_len(nrow(steps)), function(i) {
      vals <- as.numeric(steps[i, ])
      vals[is.finite(vals)]
    })
  } else if (is.list(steps)) {
    if (!is.null(item_labels) && !is.null(names(steps)) && all(item_labels %in% names(steps))) {
      steps <- steps[item_labels]
    } else if (length(steps) == 1L && n_items > 1L) {
      steps <- rep(steps, n_items)
    }
    if (length(steps) != n_items) {
      stop("Step lists must have one element per item, or a single shared element.", call. = FALSE)
    }
    out <- lapply(steps, function(x) as.numeric(x))
  } else {
    stop("'steps' must be a numeric vector, matrix, data frame, or list.", call. = FALSE)
  }

  if (any(vapply(out, function(x) length(x) < 2L, logical(1)))) {
    stop("Each polytomous item must have at least two step thresholds.", call. = FALSE)
  }

  out
}

partial_credit_probabilities <- function(theta, item_measure, step_thresholds) {
  m <- length(step_thresholds)
  eta <- numeric(m + 1L)

  if (m > 0L) {
    for (k in seq_len(m)) {
      eta[k + 1L] <- eta[k] + theta - item_measure - step_thresholds[k]
    }
  }

  eta <- eta - max(eta)
  exp_eta <- exp(eta)
  exp_eta / sum(exp_eta)
}

partial_credit_expected_score <- function(theta, item_measure, step_thresholds) {
  probs <- partial_credit_probabilities(theta, item_measure, step_thresholds)
  sum((seq_along(probs) - 1L) * probs)
}

partial_credit_cumulative_above <- function(theta, item_measure, step_thresholds, category) {
  probs <- partial_credit_probabilities(theta, item_measure, step_thresholds)
  sum(probs[(category + 1L):length(probs)])
}

solve_polytomous_theta <- function(fun, target, item_measure, step_thresholds) {
  anchor <- c(item_measure, item_measure + step_thresholds)
  lower <- min(anchor) - 10
  upper <- max(anchor) + 10

  f_lower <- fun(lower) - target
  f_upper <- fun(upper) - target

  while (f_lower > 0) {
    lower <- lower - 5
    f_lower <- fun(lower) - target
  }
  while (f_upper < 0) {
    upper <- upper + 5
    f_upper <- fun(upper) - target
  }

  stats::uniroot(function(theta) fun(theta) - target, lower = lower, upper = upper)$root
}

compute_polytomous_positions <- function(
    item_measure,
    step_thresholds,
    mode = c("halfpoint", "thurstonian", "andrich", "center"),
    low_adj = 0.25,
    high_adj = 0.25
) {
  mode <- match.arg(mode)
  m <- length(step_thresholds)

  if (mode == "andrich") {
    return(item_measure + step_thresholds)
  }

  if (mode == "halfpoint") {
    return(vapply(
      seq_len(m),
      function(k) solve_polytomous_theta(
        fun = function(theta) partial_credit_expected_score(theta, item_measure, step_thresholds),
        target = k - 0.5,
        item_measure = item_measure,
        step_thresholds = step_thresholds
      ),
      numeric(1)
    ))
  }

  if (mode == "thurstonian") {
    return(vapply(
      seq_len(m),
      function(k) solve_polytomous_theta(
        fun = function(theta) partial_credit_cumulative_above(theta, item_measure, step_thresholds, category = k),
        target = 0.5,
        item_measure = item_measure,
        step_thresholds = step_thresholds
      ),
      numeric(1)
    ))
  }

  targets <- c(low_adj, if (m > 1L) seq_len(m - 1L) else numeric(), m + 1L - high_adj - 1L)
  vapply(
    targets,
    function(target) solve_polytomous_theta(
      fun = function(theta) partial_credit_expected_score(theta, item_measure, step_thresholds),
      target = target,
      item_measure = item_measure,
      step_thresholds = step_thresholds
    ),
    numeric(1)
  )
}

polytomous_suffixes <- function(
    step_thresholds,
    mode = c("halfpoint", "thurstonian", "andrich", "center"),
    category_codes = NULL
) {
  mode <- match.arg(mode)
  m <- length(step_thresholds)

  if (is.null(category_codes)) {
    category_codes <- as.character(0:m)
  } else {
    category_codes <- as.character(category_codes)
  }

  if (length(category_codes) != m + 1L) {
    stop("Category codes must have length equal to number of categories.", call. = FALSE)
  }

  switch(
    mode,
    halfpoint = paste0(".", category_codes[seq_len(m)], "5"),
    thurstonian = paste0(".", category_codes[seq_len(m) + 1L]),
    andrich = paste0(".", category_codes[seq_len(m) + 1L]),
    center = paste0(".", category_codes)
  )
}

expand_polytomous_items <- function(
    items,
    steps,
    mode = c("halfpoint", "thurstonian", "andrich", "center"),
    low_adj = 0.25,
    high_adj = 0.25,
    category_codes = NULL
) {
  mode <- match.arg(mode)
  item_df <- normalize_wright_input(items, side = "item")
  step_list <- normalize_polytomous_steps(steps, n_items = nrow(item_df), item_labels = item_df$label)

  expanded <- vector("list", length = nrow(item_df))

  for (i in seq_len(nrow(item_df))) {
    item_measure <- item_df$measure[i]
    item_label <- item_df$label[i]
    item_steps <- step_list[[i]]
    suffixes <- polytomous_suffixes(
      step_thresholds = item_steps,
      mode = mode,
      category_codes = category_codes
    )
    positions <- compute_polytomous_positions(
      item_measure = item_measure,
      step_thresholds = item_steps,
      mode = mode,
      low_adj = low_adj,
      high_adj = high_adj
    )

    expanded[[i]] <- data.frame(
      label = paste(item_label, suffixes),
      measure = positions,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, expanded)
}

threshold_mode_title <- function(mode = c("halfpoint", "thurstonian", "andrich", "center"), item_title = "ITEM") {
  mode <- match.arg(mode)
  suffix <- switch(
    mode,
    halfpoint = "Rasch-half-point thresholds",
    thurstonian = "50% cumulative probabilities",
    andrich = "Andrich thresholds",
    center = "category centers"
  )
  paste(item_title, "-", suffix)
}

resolve_threshold_table_style <- function(table_style) {
  switch(
    table_style,
    custom = list(
      mode = NULL,
      defaults = list()
    ),
    `table1.5` = list(
      mode = "halfpoint",
      defaults = list(
        person_display = "distribution",
        item_display = "labels",
        label_width = 24L
      )
    ),
    `table1.6` = list(
      mode = "thurstonian",
      defaults = list(
        person_display = "distribution",
        item_display = "labels",
        label_width = 24L
      )
    ),
    `table1.7` = list(
      mode = "andrich",
      defaults = list(
        person_display = "distribution",
        item_display = "labels",
        label_width = 24L
      )
    ),
    `table1.8` = list(
      mode = "center",
      defaults = list(
        person_display = "distribution",
        item_display = "labels",
        label_width = 24L
      )
    )
  )
}

#' Create a Winsteps-style polytomous threshold Wright map
#'
#' @param persons Numeric vector or data frame of person measures.
#' @param items Numeric vector or data frame of item measures.
#' @param steps Step thresholds for each item. This can be a shared numeric
#'   vector, a matrix/data frame with one row per item, or a list with one
#'   numeric vector per item. Thresholds are Rasch-Andrich thresholds relative
#'   to item difficulty.
#' @param table_style Optional Winsteps-inspired preset. `"table1.5"` to
#'   `"table1.8"` map directly to the main threshold variants. `"custom"`
#'   leaves the mode and display settings unchanged.
#' @param mode Which polytomous map to produce: `"halfpoint"` (Table 1.5-like),
#'   `"thurstonian"` (Table 1.6-like), `"andrich"` (Table 1.7-like), or
#'   `"center"` (Table 1.8-like).
#' @param category_codes Labels used in the suffixes appended to each item.
#' @param low_adj Adjustment for the bottom extreme category in `"center"` mode.
#' @param high_adj Adjustment for the top extreme category in `"center"` mode.
#' @param person_title Heading shown for the person side.
#' @param item_title Base heading shown for the item side.
#' @param ... Additional arguments passed to [wright_map_ascii()], such as
#'   `line_length`, `max_page`, `label_abbrev`, or `right_measure`.
#' @return An object of class `ascii_wright_map`.
#' @export
polytomous_threshold_map_ascii <- function(
    persons,
    items,
    steps,
    table_style = c("custom", "table1.5", "table1.6", "table1.7", "table1.8"),
    mode = c("halfpoint", "thurstonian", "andrich", "center"),
    category_codes = NULL,
    low_adj = 0.25,
    high_adj = 0.25,
    person_title = "PERSON",
    item_title = "ITEM",
    ...
) {
  table_style_missing <- missing(table_style)
  mode_missing <- missing(mode)
  table_style <- match.arg(table_style)
  mode <- match.arg(mode)
  extra_args <- list(...)
  preset <- resolve_threshold_table_style(table_style)

  if (table_style != "custom" || !table_style_missing) {
    if (mode_missing) {
      mode <- preset$mode
    }
    for (nm in names(preset$defaults)) {
      if (is.null(extra_args[[nm]])) {
        extra_args[[nm]] <- preset$defaults[[nm]]
      }
    }
  }

  expanded_items <- expand_polytomous_items(
    items = items,
    steps = steps,
    mode = mode,
    low_adj = low_adj,
    high_adj = high_adj,
    category_codes = category_codes
  )

  if (is.null(extra_args$name_trunc)) {
    extra_args$name_trunc <- max(nchar(expanded_items$label, type = "width"))
  }

  map_args <- c(
    list(
      persons = persons,
      items = expanded_items,
      person_title = person_title,
      item_title = threshold_mode_title(mode = mode, item_title = item_title)
    ),
    extra_args
  )

  map <- do.call(wright_map_ascii, map_args)

  map$polytomous <- list(
    table_style = table_style,
    mode = mode,
    low_adj = low_adj,
    high_adj = high_adj,
    category_codes = if (is.null(category_codes)) NULL else as.character(category_codes),
    step_spec = steps
  )
  map$settings$polytomous_table_style <- table_style
  map
}
