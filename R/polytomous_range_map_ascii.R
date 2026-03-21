compute_polytomous_range_points <- function(items, steps) {
  item_df <- normalize_wright_input(items, side = "item")
  step_list <- normalize_polytomous_steps(steps, n_items = nrow(item_df), item_labels = item_df$label)

  bottom <- vector("list", length = nrow(item_df))
  center <- vector("list", length = nrow(item_df))
  top <- vector("list", length = nrow(item_df))

  for (i in seq_len(nrow(item_df))) {
    item_measure <- item_df$measure[i]
    item_label <- item_df$label[i]
    item_steps <- step_list[[i]]
    thurstonian <- compute_polytomous_positions(
      item_measure = item_measure,
      step_thresholds = item_steps,
      mode = "thurstonian"
    )

    bottom[[i]] <- data.frame(label = item_label, measure = thurstonian[1L], stringsAsFactors = FALSE)
    center[[i]] <- data.frame(label = item_label, measure = item_measure, stringsAsFactors = FALSE)
    top[[i]] <- data.frame(label = item_label, measure = thurstonian[length(thurstonian)], stringsAsFactors = FALSE)
  }

  list(
    bottom = do.call(rbind, bottom),
    center = do.call(rbind, center),
    top = do.call(rbind, top)
  )
}

build_range_direction_line <- function(
    measure_col_width,
    spacer,
    person_width,
    item_width,
    left_label,
    right_label,
    person_title,
    item_title
) {
  person_strip <- {
    dash_width <- max(0L, person_width - nchar(left_label, type = "width") - nchar(person_title, type = "width") - 2L)
    strip <- paste0(left_label, " ", strrep("-", dash_width), if (dash_width > 0L) " " else "", person_title)
    sprintf("%-*s", person_width, substr(strip, 1L, person_width))
  }

  paste0(
    strrep(" ", measure_col_width),
    spacer,
    person_strip,
    " -+- ",
    sprintf("%-*s", item_width, truncate_text(item_title, item_width)),
    " -+- ",
    sprintf("%-*s", item_width, truncate_text(item_title, item_width)),
    " -+- ",
    sprintf("%-*s", item_width, truncate_text(paste(item_title, right_label), item_width))
  )
}

build_range_header_line <- function(
    measure_col_width,
    spacer,
    person_width,
    item_width,
    measure_header,
    bottom_header,
    center_header,
    top_header,
    right_measure = TRUE
) {
  paste0(
    sprintf("%*s", measure_col_width, measure_header),
    spacer,
    sprintf("%*s", person_width + 3L, ""),
    "| ",
    sprintf("%-*s", item_width, truncate_text(bottom_header, item_width)),
    " | ",
    sprintf("%-*s", item_width, truncate_text(center_header, item_width)),
    " | ",
    sprintf("%-*s", item_width, truncate_text(top_header, item_width)),
    " |",
    if (isTRUE(right_measure)) sprintf("%*s", measure_col_width + 1L, measure_header) else ""
  )
}

resolve_range_table_style <- function(table_style) {
  switch(
    table_style,
    custom = list(
      person_display = "distribution",
      item_display = "labels",
      person_width = 18L,
      item_width = 14L,
      right_measure = TRUE,
      preset_line_length = NULL
    ),
    `table1.4` = list(
      person_display = "distribution",
      item_display = "labels",
      person_width = 18L,
      item_width = 14L,
      right_measure = TRUE,
      preset_line_length = NULL
    )
  )
}

resolve_range_widths <- function(
    desired_person_width,
    desired_item_width,
    person_mode,
    item_mode,
    name_trunc,
    fixed_width,
    line_length = NULL
) {
  person_width <- as.integer(desired_person_width)
  item_width <- as.integer(desired_item_width)

  if (is.null(line_length)) {
    return(list(person_width = person_width, item_width = item_width))
  }

  if (!is.numeric(line_length) || length(line_length) != 1L || !is.finite(line_length) || line_length < 30) {
    stop("'line_length' must be NULL or a single number of at least 30.", call. = FALSE)
  }

  min_person <- if (person_mode == "distribution") 1L else max(4L, min(as.integer(name_trunc), 4L))
  min_item <- if (item_mode == "distribution") 1L else max(4L, min(as.integer(name_trunc), 4L))
  available_width <- as.integer(line_length) - fixed_width

  if (available_width < (min_person + 3L * min_item)) {
    stop("'line_length' is too small for the requested range-map layout.", call. = FALSE)
  }

  while ((person_width + 3L * item_width) > available_width) {
    person_excess <- person_width - min_person
    item_excess <- item_width - min_item

    if (person_excess <= 0L && item_excess <= 0L) {
      break
    }

    if (item_excess > 0L && (3L * item_excess >= person_excess || person_excess <= 0L)) {
      item_width <- item_width - 1L
    } else if (person_excess > 0L) {
      person_width <- person_width - 1L
    } else {
      item_width <- item_width - 1L
    }
  }

  list(person_width = person_width, item_width = item_width)
}

range_map_ascii_from_points <- function(
    persons,
    range_points,
    table_style = c("custom", "table1.4"),
    person_display = c("distribution", "labels"),
    item_display = c("distribution", "labels"),
    distribution_style = c("winsteps", "hashdot", "x"),
    person_title = "PERSON",
    item_title = "ITEM",
    measure_header = "MEASURE",
    bottom_header = paste0("BOTTOM P=50", intToUtf8(37L)),
    center_header = "MEASURE",
    top_header = paste0("TOP P=50", intToUtf8(37L)),
    more_label = "<more>",
    rare_label = "<rare>",
    less_label = "<less>",
    freq_label = "<freq>",
    measure_range = NULL,
    lines_per_logit = 2L,
    person_width = 18L,
    item_width = 14L,
    person_hash_size = NULL,
    item_hash_size = NULL,
    name_trunc = NULL,
    label_abbrev = c("truncate", "smart"),
    label_overrides = NULL,
    digits = 0L,
    right_measure = TRUE,
    line_length = NULL,
    max_page = NULL,
    page_break = "\f"
) {
  table_style_missing <- missing(table_style)
  person_display_missing <- missing(person_display)
  item_display_missing <- missing(item_display)
  person_width_missing <- missing(person_width)
  item_width_missing <- missing(item_width)
  right_measure_missing <- missing(right_measure)
  line_length_missing <- missing(line_length)

  table_style <- match.arg(table_style)
  preset <- resolve_range_table_style(table_style)
  if (table_style != "custom" || !table_style_missing) {
    if (person_display_missing) {
      person_display <- preset$person_display
    }
    if (item_display_missing) {
      item_display <- preset$item_display
    }
    if (person_width_missing) {
      person_width <- preset$person_width
    }
    if (item_width_missing) {
      item_width <- preset$item_width
    }
    if (right_measure_missing) {
      right_measure <- preset$right_measure
    }
    if (line_length_missing) {
      line_length <- preset$preset_line_length
    }
  }

  person_display <- match.arg(person_display)
  item_display <- match.arg(item_display)
  distribution_style <- match.arg(distribution_style)
  label_abbrev <- match.arg(label_abbrev)
  person_width <- as.integer(person_width)
  item_width <- as.integer(item_width)

  if (!is.list(range_points) || !all(c("bottom", "center", "top") %in% names(range_points))) {
    stop("'range_points' must be a list with 'bottom', 'center', and 'top' data frames.", call. = FALSE)
  }

  if (is.null(name_trunc)) {
    name_trunc <- max(person_width, item_width)
  }
  name_trunc <- as.integer(name_trunc)

  person_df <- normalize_wright_input(persons, side = "person")
  range_points <- list(
    bottom = normalize_wright_input(range_points$bottom, side = "item"),
    center = normalize_wright_input(range_points$center, side = "item"),
    top = normalize_wright_input(range_points$top, side = "item")
  )

  all_values <- c(
    person_df$measure,
    range_points$bottom$measure,
    range_points$center$measure,
    range_points$top$measure
  )

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

  step <- 1 / as.integer(lines_per_logit)
  axis_values <- seq(from = upper, to = lower, by = -step)
  n_rows <- length(axis_values)

  person_df$row <- measure_to_row(person_df$measure, top = upper, step = step, n_rows = n_rows)
  range_points$bottom$row <- measure_to_row(range_points$bottom$measure, top = upper, step = step, n_rows = n_rows)
  range_points$center$row <- measure_to_row(range_points$center$measure, top = upper, step = step, n_rows = n_rows)
  range_points$top$row <- measure_to_row(range_points$top$measure, top = upper, step = step, n_rows = n_rows)

  measure_col_width <- max(
    7L,
    nchar(measure_header, type = "width"),
    max(nchar(formatC(axis_values, format = "f", digits = digits), type = "width"))
  )
  spacer <- "  "
  range_axis <- function(value) if (abs(value - round(value)) < 1e-9) "+" else "|"
  right_measure_width <- if (isTRUE(right_measure)) measure_col_width + 1L else 0L
  fixed_width <- max(
    measure_col_width + nchar(spacer) + 13L + right_measure_width,
    measure_col_width + nchar(spacer) + 15L
  )
  widths <- resolve_range_widths(
    desired_person_width = person_width,
    desired_item_width = item_width,
    person_mode = person_display,
    item_mode = item_display,
    name_trunc = name_trunc,
    fixed_width = fixed_width,
    line_length = line_length
  )
  person_width <- widths$person_width
  item_width <- widths$item_width

  person_side <- build_side_rows(
    data = person_df,
    n_rows = n_rows,
    width = person_width,
    mode = person_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = person_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "person"
  )
  bottom_side <- build_side_rows(
    data = range_points$bottom,
    n_rows = n_rows,
    width = item_width,
    mode = item_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = item_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "item_bottom"
  )
  center_side <- build_side_rows(
    data = range_points$center,
    n_rows = n_rows,
    width = item_width,
    mode = item_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = item_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "item_center"
  )
  top_side <- build_side_rows(
    data = range_points$top,
    n_rows = n_rows,
    width = item_width,
    mode = item_display,
    distribution_style = distribution_style,
    name_trunc = name_trunc,
    hash_size = item_hash_size,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    side = "item_top"
  )

  notes <- c(
    if (person_side$compressed) build_distribution_note(person_title, person_side$hash_size),
    if (bottom_side$compressed) build_distribution_note(paste(item_title, "BOTTOM"), bottom_side$hash_size),
    if (center_side$compressed) build_distribution_note(paste(item_title, "MEASURE"), center_side$hash_size),
    if (top_side$compressed) build_distribution_note(paste(item_title, "TOP"), top_side$hash_size)
  )

  header_lines <- character(2L)
  header_lines[1L] <- build_range_header_line(
    measure_col_width = measure_col_width,
    spacer = spacer,
    person_width = person_width,
    item_width = item_width,
    measure_header = measure_header,
    bottom_header = bottom_header,
    center_header = center_header,
    top_header = top_header,
    right_measure = right_measure
  )

  header_lines[2L] <- build_range_direction_line(
    measure_col_width = measure_col_width,
    spacer = spacer,
    person_width = person_width,
    item_width = item_width,
    left_label = more_label,
    right_label = rare_label,
    person_title = person_title,
    item_title = item_title
  )

  body_lines <- character(n_rows)
  for (i in seq_len(n_rows)) {
    measure_text <- if (abs(axis_values[i] - round(axis_values[i])) < 1e-9) {
      format_measure_label(axis_values[i], digits = digits, width = measure_col_width)
    } else {
      strrep(" ", measure_col_width)
    }
    right_measure_text <- if (isTRUE(right_measure)) {
      if (abs(axis_values[i] - round(axis_values[i])) < 1e-9) {
        paste0(" ", format_measure_label(axis_values[i], digits = digits, width = measure_col_width))
      } else {
        strrep(" ", right_measure_width)
      }
    } else {
      ""
    }

    body_lines[i] <- paste0(
      measure_text,
      spacer,
      person_side$render(i, align = "right"),
      " ",
      range_axis(axis_values[i]),
      " ",
      bottom_side$render(i, align = "left"),
      " ",
      range_axis(axis_values[i]),
      " ",
      center_side$render(i, align = "left"),
      " ",
      range_axis(axis_values[i]),
      " ",
      top_side$render(i, align = "left"),
      right_measure_text
    )
  }

  footer_lines <- build_range_direction_line(
    measure_col_width = measure_col_width,
    spacer = spacer,
    person_width = person_width,
    item_width = item_width,
    left_label = less_label,
    right_label = freq_label,
    person_title = person_title,
    item_title = item_title
  )

  if (length(notes)) {
    footer_lines <- c(
      footer_lines,
      vapply(
        notes,
        function(note) paste0(strrep(" ", measure_col_width + nchar(spacer)), note),
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
      items = range_points$center,
      range_points = range_points,
      settings = list(
        table_style = table_style,
        measure_range = c(lower, upper),
        person_display = person_display,
        item_display = item_display,
        distribution_style = distribution_style,
        person_width = person_width,
        item_width = item_width,
        name_trunc = name_trunc,
        label_abbrev = label_abbrev,
        right_measure = isTRUE(right_measure),
        person_hash_size = person_side$hash_size,
        item_hash_size = bottom_side$hash_size,
        line_length = line_length,
        max_page = max_page,
        page_break = page_break,
        page_count = length(pages)
      )
    ),
    class = "ascii_wright_map"
  )
}

#' Create a Winsteps-style polytomous range map (Table 1.4-like)
#'
#' @param persons Numeric vector or data frame of person measures.
#' @param items Numeric vector or data frame of item measures.
#' @param steps Step thresholds for each item. This can be a shared numeric
#'   vector, a matrix/data frame with one row per item, or a list with one
#'   numeric vector per item.
#' @param table_style Optional Winsteps-inspired preset. `"table1.4"` applies
#'   the standard bottom/measure/top layout defaults. `"custom"` leaves the
#'   explicit arguments untouched.
#' @param person_display How to draw the person side: `"labels"` or `"distribution"`.
#' @param item_display How to draw the three item range columns: `"labels"` or
#'   `"distribution"`.
#' @param distribution_style How distribution counts are compacted.
#' @param person_title Heading shown for the person side.
#' @param item_title Heading shown for the item side.
#' @param measure_header Heading shown above the left and optional right measure columns.
#' @param bottom_header Heading shown above the bottom-range item column.
#' @param center_header Heading shown above the item difficulty column.
#' @param top_header Heading shown above the top-range item column.
#' @param more_label Top-left direction label.
#' @param rare_label Top-right direction label.
#' @param less_label Bottom-left direction label.
#' @param freq_label Bottom-right direction label.
#' @param measure_range Optional numeric vector of length 2 giving c(min, max).
#' @param lines_per_logit Number of text rows used for one logit interval.
#' @param person_width Character width reserved for the person column.
#' @param item_width Character width reserved for each item column.
#' @param person_hash_size Optional override for the number of persons represented by one `#`.
#' @param item_hash_size Optional override for the number of items represented by one `#`.
#' @param name_trunc Maximum width of labels when label displays are used.
#' @param label_abbrev Label abbreviation strategy. `"truncate"` clips labels at
#'   the available width; `"smart"` removes low-information words and compresses
#'   longer words before truncating.
#' @param label_overrides Optional user-supplied label abbreviations. This can
#'   be a named character vector of exact replacements or a function. Functions
#'   receive `label`, `width`, and `side`, where `side` can be `"person"`,
#'   `"item_bottom"`, `"item_center"`, or `"item_top"`.
#' @param digits Digits for printed measure labels.
#' @param right_measure Show a mirrored measure column on the far right.
#' @param line_length Optional maximum output width, similar to Winsteps
#'   `LINELENGTH=`.
#' @param max_page Optional maximum number of output lines per page, similar to
#'   Winsteps `MAXPAGE=`.
#' @param page_break Marker inserted between rendered pages when `max_page`
#'   creates multiple pages.
#' @return An object of class `ascii_wright_map`.
#' @export
polytomous_range_map_ascii <- function(
    persons,
    items,
    steps,
    table_style = c("custom", "table1.4"),
    person_display = c("distribution", "labels"),
    item_display = c("distribution", "labels"),
    distribution_style = c("winsteps", "hashdot", "x"),
    person_title = "PERSON",
    item_title = "ITEM",
    measure_header = "MEASURE",
    bottom_header = paste0("BOTTOM P=50", intToUtf8(37L)),
    center_header = "MEASURE",
    top_header = paste0("TOP P=50", intToUtf8(37L)),
    more_label = "<more>",
    rare_label = "<rare>",
    less_label = "<less>",
    freq_label = "<freq>",
    measure_range = NULL,
    lines_per_logit = 2L,
    person_width = 18L,
    item_width = 14L,
    person_hash_size = NULL,
    item_hash_size = NULL,
    name_trunc = NULL,
    label_abbrev = c("truncate", "smart"),
    label_overrides = NULL,
    digits = 0L,
    right_measure = TRUE,
    line_length = NULL,
    max_page = NULL,
    page_break = "\f"
) {
  table_style <- match.arg(table_style)
  range_points <- compute_polytomous_range_points(items = items, steps = steps)
  map <- range_map_ascii_from_points(
    persons = persons,
    range_points = range_points,
    table_style = table_style,
    person_display = person_display,
    item_display = item_display,
    distribution_style = distribution_style,
    person_title = person_title,
    item_title = item_title,
    measure_header = measure_header,
    bottom_header = bottom_header,
    center_header = center_header,
    top_header = top_header,
    more_label = more_label,
    rare_label = rare_label,
    less_label = less_label,
    freq_label = freq_label,
    measure_range = measure_range,
    lines_per_logit = lines_per_logit,
    person_width = person_width,
    item_width = item_width,
    person_hash_size = person_hash_size,
    item_hash_size = item_hash_size,
    name_trunc = name_trunc,
    label_abbrev = label_abbrev,
    label_overrides = label_overrides,
    digits = digits,
    right_measure = right_measure,
    line_length = line_length,
    max_page = max_page,
    page_break = page_break
  )
  map$polytomous <- list(
    table_style = table_style,
    step_spec = steps
  )
  map$settings$polytomous_table_style <- table_style
  map
}
