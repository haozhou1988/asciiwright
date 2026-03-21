normalize_label_preview_input <- function(x, arg = "labels") {
  if (is.data.frame(x)) {
    if ("label" %in% names(x)) {
      labels <- x[["label"]]
    } else if (!is.null(rownames(x))) {
      labels <- rownames(x)
    } else {
      stop(
        sprintf("'%s' data frames must contain a 'label' column or row names.", arg),
        call. = FALSE
      )
    }
  } else if (is.character(x)) {
    labels <- x
  } else if (!is.null(names(x))) {
    labels <- names(x)
  } else {
    stop(
      sprintf("'%s' must be a character vector, a named vector, or a data frame with labels.", arg),
      call. = FALSE
    )
  }

  labels <- normalize_label_text(as.character(labels))
  keep <- !is.na(labels) & nzchar(labels)
  labels[keep]
}

#' Preview label abbreviations before rendering a map
#'
#' @param labels Character vector of labels, a named vector, or a data frame
#'   with a `label` column.
#' @param width Target width for the abbreviated labels.
#' @param style Abbreviation strategy. `"truncate"` clips labels at the
#'   available width; `"smart"` removes low-information words and compresses
#'   longer words before truncating.
#' @param side Side identifier passed through to `label_overrides`.
#' @param label_overrides Optional user-supplied label abbreviations. This can
#'   be a named character vector of exact replacements or a function. Functions
#'   receive `label`, `width`, and `side`.
#' @param unique Whether to preview each unique label only once.
#' @return A data frame with the original labels, suggested abbreviations, final
#'   abbreviations after overrides, and change flags.
#' @export
preview_label_abbrev <- function(
    labels,
    width,
    style = c("truncate", "smart"),
    side = "item",
    label_overrides = NULL,
    unique = TRUE
) {
  style <- match.arg(style)
  width <- as.integer(width)

  if (!is.numeric(width) || length(width) != 1L || is.na(width) || width < 1L) {
    stop("'width' must be a positive integer.", call. = FALSE)
  }

  labels <- normalize_label_preview_input(labels)
  if (isTRUE(unique)) {
    labels <- unique(labels)
  }

  suggested <- vapply(
    labels,
    abbreviate_label_text,
    character(1),
    width = width,
    style = style,
    label_overrides = NULL,
    side = side
  )
  final <- vapply(
    labels,
    abbreviate_label_text,
    character(1),
    width = width,
    style = style,
    label_overrides = label_overrides,
    side = side
  )

  data.frame(
    label = labels,
    suggested = suggested,
    final = final,
    changed = labels != final,
    used_override = suggested != final,
    width = rep.int(width, length(labels)),
    style = rep.int(style, length(labels)),
    side = rep.int(side, length(labels)),
    stringsAsFactors = FALSE
  )
}

#' Build a `label_overrides` vector from previewed or edited labels
#'
#' @param x A preview data frame created by [preview_label_abbrev()], a data
#'   frame containing label columns, or a character vector / named vector of
#'   original labels.
#' @param replacements Optional replacement labels. When `x` is a data frame and
#'   `replacements` is `NULL`, `replacement_col` is used. When `x` is a vector,
#'   `replacements` must be a character vector of the same length.
#' @param original_col Name of the column containing the original labels when
#'   `x` is a data frame.
#' @param replacement_col Name of the column containing the edited labels when
#'   `x` is a data frame.
#' @param drop_identity Whether to omit entries whose replacement is identical
#'   to the original label.
#' @param drop_empty Whether to omit missing or empty replacements.
#' @return A named character vector suitable for the `label_overrides` argument
#'   used by [wright_map_ascii()] and related functions.
#' @export
make_label_overrides <- function(
    x,
    replacements = NULL,
    original_col = "label",
    replacement_col = "final",
    drop_identity = TRUE,
    drop_empty = TRUE
) {
  if (is.data.frame(x)) {
    if (!original_col %in% names(x)) {
      stop(sprintf("Column '%s' was not found in 'x'.", original_col), call. = FALSE)
    }

    original <- x[[original_col]]
    replacement <- if (is.null(replacements)) {
      if (!replacement_col %in% names(x)) {
        stop(sprintf("Column '%s' was not found in 'x'.", replacement_col), call. = FALSE)
      }
      x[[replacement_col]]
    } else {
      replacements
    }
  } else {
    original <- normalize_label_preview_input(x, arg = "x")
    if (is.null(replacements)) {
      stop("'replacements' must be supplied when 'x' is not a data frame.", call. = FALSE)
    }
    replacement <- replacements
  }

  if (length(original) != length(replacement)) {
    stop("Original labels and replacements must have the same length.", call. = FALSE)
  }

  original <- normalize_label_text(as.character(original))
  replacement <- normalize_label_text(as.character(replacement))

  keep <- !is.na(original) & nzchar(original)
  if (isTRUE(drop_empty)) {
    keep <- keep & !is.na(replacement) & nzchar(replacement)
  }

  original <- original[keep]
  replacement <- replacement[keep]

  if (isTRUE(drop_identity)) {
    keep <- original != replacement
    original <- original[keep]
    replacement <- replacement[keep]
  }

  if (!length(original)) {
    return(structure(character(), names = character()))
  }

  keep_last <- !duplicated(original, fromLast = TRUE)
  structure(replacement[keep_last], names = original[keep_last])
}
