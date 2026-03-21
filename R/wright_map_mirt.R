check_mirt_available <- function() {
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop(
      "Package 'mirt' is required for wright_map_mirt(). Install it with install.packages(\"mirt\").",
      call. = FALSE
    )
  }
}

coerce_numeric_matrix <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.data.frame(x)) {
    out <- lapply(x, function(col) {
      if (is.factor(col)) {
        as.numeric(as.character(col))
      } else if (is.logical(col)) {
        as.numeric(col)
      } else {
        as.numeric(col)
      }
    })
    out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- names(x)
    out <- as.matrix(out)
    rownames(out) <- rownames(x)
    return(out)
  }

  if (is.matrix(x) || is.array(x)) {
    return(matrix(as.numeric(x), nrow = nrow(x), dimnames = dimnames(x)))
  }

  stop("'response_data' must be a matrix, array, or data frame.", call. = FALSE)
}

detect_frequency_column <- function(item_names, data_colnames = NULL, freq_column = "auto") {
  if (is.null(freq_column)) {
    return(NULL)
  }

  if (!identical(freq_column, "auto")) {
    return(as.character(freq_column)[1L])
  }

  aliases <- c("freq", "frequency", "weight", "weights")
  from_items <- item_names[tolower(item_names) %in% aliases]
  from_data <- if (!is.null(data_colnames)) data_colnames[tolower(data_colnames) %in% aliases] else character()
  candidates <- c(from_items, from_data)

  if (!length(candidates)) {
    return(NULL)
  }

  candidates[1L]
}

normalize_frequency_weights <- function(weights, n_rows) {
  if (is.null(weights)) {
    return(rep.int(1L, n_rows))
  }

  weights <- as.numeric(weights)
  if (length(weights) != n_rows || any(!is.finite(weights)) || any(weights < 0)) {
    stop("Frequency weights must be non-negative, finite values with one weight per response row.", call. = FALSE)
  }
  if (any(abs(weights - round(weights)) > 1e-8)) {
    stop("Frequency weights must be whole numbers.", call. = FALSE)
  }

  as.integer(round(weights))
}

compute_shifted_raw_scores <- function(response_data) {
  if (is.null(response_data) || !nrow(response_data) || !ncol(response_data)) {
    return(NULL)
  }

  col_mins <- vapply(
    seq_len(ncol(response_data)),
    function(j) {
      vals <- response_data[, j]
      vals <- vals[is.finite(vals)]
      if (!length(vals)) {
        0
      } else {
        min(vals)
      }
    },
    numeric(1)
  )

  adjusted <- sweep(response_data, 2L, col_mins, FUN = "-")
  rowSums(adjusted, na.rm = TRUE)
}

resolve_mirt_response_source <- function(model, response_data = NULL, data_env = parent.frame()) {
  if (!is.null(response_data)) {
    return(response_data)
  }

  from_call <- tryCatch(
    eval(model@Call$data, envir = data_env),
    error = function(e) NULL
  )
  if (!is.null(from_call)) {
    return(from_call)
  }

  mirt::extract.mirt(model, "data")
}

prepare_mirt_response_data <- function(model, response_data = NULL, freq_column = "auto") {
  item_names <- as.character(mirt::extract.mirt(model, "itemnames"))
  item_types <- as.character(mirt::extract.mirt(model, "itemtype"))
  K <- as.integer(mirt::extract.mirt(model, "K"))

  data_matrix <- coerce_numeric_matrix(response_data)
  if (is.null(colnames(data_matrix)) && ncol(data_matrix) == length(item_names)) {
    colnames(data_matrix) <- item_names
  }

  freq_name <- detect_frequency_column(item_names, colnames(data_matrix), freq_column = freq_column)
  measurement_mask <- rep(TRUE, length(item_names))
  if (!is.null(freq_name) && freq_name %in% item_names) {
    measurement_mask[item_names == freq_name] <- FALSE
  }

  measurement_names <- item_names[measurement_mask]
  measurement_types <- item_types[measurement_mask]
  measurement_K <- K[measurement_mask]

  if (!is.null(colnames(data_matrix))) {
    if (!all(measurement_names %in% colnames(data_matrix))) {
      stop("Could not match the scored item names from the mirt model to 'response_data'.", call. = FALSE)
    }

    response_matrix <- data_matrix[, measurement_names, drop = FALSE]
    weights <- if (!is.null(freq_name) && freq_name %in% colnames(data_matrix)) data_matrix[, freq_name] else NULL
  } else if (ncol(data_matrix) == length(item_names)) {
    response_matrix <- data_matrix[, measurement_mask, drop = FALSE]
    weights <- if (!is.null(freq_name) && any(!measurement_mask)) data_matrix[, which(!measurement_mask)[1L]] else NULL
    colnames(response_matrix) <- measurement_names
  } else if (ncol(data_matrix) == length(measurement_names)) {
    response_matrix <- data_matrix
    colnames(response_matrix) <- measurement_names
    weights <- NULL
  } else {
    stop("Could not align 'response_data' with the item names stored in the mirt model.", call. = FALSE)
  }

  list(
    response_data = response_matrix,
    weights = normalize_frequency_weights(weights, nrow(response_matrix)),
    freq_column = if (!is.null(freq_name) && freq_name %in% item_names) freq_name else NULL,
    item_names = measurement_names,
    item_types = measurement_types,
    K = measurement_K
  )
}

prepare_mirt_person_data <- function(
    model,
    response_info,
    theta = NULL,
    theta_method = "EAP",
    include_scores = TRUE
) {
  theta_values <- if (is.null(theta)) {
    scores <- mirt::fscores(
      model,
      method = theta_method,
      full.scores = TRUE,
      full.scores.SE = FALSE
    )
    if (is.null(dim(scores))) {
      as.numeric(scores)
    } else {
      if (ncol(scores) != 1L) {
        stop("wright_map_mirt() currently supports unidimensional mirt models only.", call. = FALSE)
      }
      as.numeric(scores[, 1L])
    }
  } else {
    theta <- if (is.data.frame(theta)) as.matrix(theta) else theta
    if (is.null(dim(theta))) {
      as.numeric(theta)
    } else {
      if (ncol(theta) != 1L) {
        stop("'theta' must be a numeric vector or a one-column matrix/data frame.", call. = FALSE)
      }
      as.numeric(theta[, 1L])
    }
  }

  if (length(theta_values) != nrow(response_info$response_data)) {
    stop("The number of theta values must match the number of response rows before frequency expansion.", call. = FALSE)
  }

  raw_scores <- if (isTRUE(include_scores)) compute_shifted_raw_scores(response_info$response_data) else NULL
  base_labels <- rownames(response_info$response_data)
  if (is.null(base_labels)) {
    base_labels <- as.character(seq_along(theta_values))
  }

  weights <- response_info$weights
  if (all(weights == 1L)) {
    labels <- base_labels
    measures <- theta_values
    scores <- raw_scores
  } else {
    expand_index <- rep.int(seq_along(theta_values), times = weights)
    labels <- paste0("P", seq_along(expand_index))
    measures <- theta_values[expand_index]
    scores <- if (is.null(raw_scores)) NULL else raw_scores[expand_index]
  }

  data.frame(
    label = labels,
    measure = measures,
    score = if (is.null(scores)) rep(NA_real_, length(measures)) else scores,
    stringsAsFactors = FALSE
  )
}

extract_mirt_item_spec <- function(model, response_info) {
  coef_list <- mirt::coef(model, IRTpars = TRUE)
  item_names <- response_info$item_names
  item_types <- response_info$item_types
  K <- response_info$K

  items <- vector("list", length(item_names))
  threshold_points <- vector("list", length(item_names))
  range_bottom <- vector("list", length(item_names))
  range_center <- vector("list", length(item_names))
  range_top <- vector("list", length(item_names))
  is_polytomous <- logical(length(item_names))

  for (i in seq_along(item_names)) {
    item_name <- item_names[i]
    pars <- coef_list[[item_name]]
    if (is.null(pars)) {
      stop(sprintf("Could not extract coefficients for item '%s' from the mirt model.", item_name), call. = FALSE)
    }

    par_names <- colnames(pars)
    threshold_cols <- grep("^b[0-9]+$", par_names, value = TRUE)
    if (length(threshold_cols)) {
      threshold_cols <- threshold_cols[order(as.integer(sub("^b", "", threshold_cols)))]
    }

    if (length(threshold_cols)) {
      thresholds <- as.numeric(pars[1L, threshold_cols])
      thresholds <- thresholds[is.finite(thresholds)]
      if (!length(thresholds)) {
        stop(sprintf("Item '%s' did not yield any finite threshold locations.", item_name), call. = FALSE)
      }

      item_measure <- mean(thresholds)
      suffixes <- paste0(".", sub("^b", "", threshold_cols))[seq_along(thresholds)]
      threshold_points[[i]] <- data.frame(
        label = paste(item_name, suffixes),
        measure = thresholds,
        stringsAsFactors = FALSE
      )
      range_bottom[[i]] <- data.frame(label = item_name, measure = min(thresholds), stringsAsFactors = FALSE)
      range_center[[i]] <- data.frame(label = item_name, measure = item_measure, stringsAsFactors = FALSE)
      range_top[[i]] <- data.frame(label = item_name, measure = max(thresholds), stringsAsFactors = FALSE)
      is_polytomous[i] <- TRUE
    } else if ("b" %in% par_names) {
      item_measure <- as.numeric(pars[1L, "b"])
      threshold_points[[i]] <- NULL
      range_bottom[[i]] <- NULL
      range_center[[i]] <- NULL
      range_top[[i]] <- NULL
      is_polytomous[i] <- FALSE
    } else {
      stop(
        sprintf(
          "Item '%s' does not expose 'b' or 'b1', 'b2', ... coefficients in coef(model, IRTpars = TRUE).",
          item_name
        ),
        call. = FALSE
      )
    }

    items[[i]] <- data.frame(
      label = item_name,
      measure = item_measure,
      itemtype = item_types[i],
      categories = K[i],
      stringsAsFactors = FALSE
    )
  }

  poly_index <- which(is_polytomous)

  list(
    items = do.call(rbind, items),
    threshold_points = if (length(poly_index)) do.call(rbind, threshold_points[poly_index]) else NULL,
    range_points = if (length(poly_index)) {
      list(
        bottom = do.call(rbind, range_bottom[poly_index]),
        center = do.call(rbind, range_center[poly_index]),
        top = do.call(rbind, range_top[poly_index])
      )
    } else {
      NULL
    },
    is_polytomous = is_polytomous,
    item_types = item_types,
    K = K
  )
}

validate_mirt_map_args <- function(extra_args) {
  reserved <- c("persons", "items", "steps", "range_points", "person_scores")
  used <- intersect(names(extra_args), reserved)
  if (length(used)) {
    stop(
      sprintf("Do not pass %s through '...'; wright_map_mirt() constructs those inputs automatically.", paste(sprintf("'%s'", used), collapse = ", ")),
      call. = FALSE
    )
  }
}

#' Create an ASCII Wright map directly from a mirt model
#'
#' @param model A fitted unidimensional `mirt` model object.
#' @param response_data Optional response matrix or data frame used to compute
#'   raw scores and detect frequency columns. When omitted, the data stored in
#'   the model is used.
#' @param theta Optional numeric vector or one-column matrix/data frame of
#'   person measures. When omitted, [mirt::fscores()] is used.
#' @param theta_method Method passed to [mirt::fscores()] when `theta` is not
#'   supplied.
#' @param map_type Which map to create. `"items"` shows one location per item,
#'   `"thresholds"` plots the model's `b1`, `b2`, ... category boundaries, and
#'   `"range"` plots bottom/center/top summaries from those boundaries.
#'   `"auto"` chooses `"thresholds"` for all-polytomous models and `"items"`
#'   otherwise.
#' @param include_scores When `TRUE`, raw scores are computed from
#'   `response_data` and passed through to the rendered map.
#' @param freq_column Name of a frequency column for aggregated response data.
#'   `"auto"` detects common names such as `"freq"` and `"weight"`.
#' @param ... Additional arguments passed to [wright_map_ascii()] for
#'   `"items"`/`"thresholds"` maps or to `range_map_ascii_from_points()` for
#'   `"range"` maps.
#' @return An object of class `ascii_wright_map`.
#' @export
wright_map_mirt <- function(
    model,
    response_data = NULL,
    theta = NULL,
    theta_method = "EAP",
    map_type = c("auto", "items", "thresholds", "range"),
    include_scores = TRUE,
    freq_column = "auto",
    ...
) {
  check_mirt_available()

  if (!inherits(model, "SingleGroupClass")) {
    stop("wright_map_mirt() currently supports single-group mirt models only.", call. = FALSE)
  }

  map_type <- match.arg(map_type)
  extra_args <- list(...)
  validate_mirt_map_args(extra_args)
  response_source <- resolve_mirt_response_source(
    model,
    response_data = response_data,
    data_env = parent.frame()
  )

  response_info <- prepare_mirt_response_data(
    model,
    response_data = response_source,
    freq_column = freq_column
  )
  person_df <- prepare_mirt_person_data(
    model,
    response_info = response_info,
    theta = theta,
    theta_method = theta_method,
    include_scores = include_scores
  )
  item_spec <- extract_mirt_item_spec(model, response_info = response_info)

  has_polytomous <- any(item_spec$is_polytomous)
  all_polytomous <- all(item_spec$is_polytomous)
  map_type_used <- if (map_type == "auto") {
    if (all_polytomous) "thresholds" else "items"
  } else {
    map_type
  }

  map <- switch(
    map_type_used,
    items = {
      if (isTRUE(include_scores) && has_polytomous && is.null(extra_args$score_method)) {
        extra_args$score_method <- "observed"
      }
      map_args <- c(
        list(
          persons = person_df,
          items = item_spec$items[, c("label", "measure"), drop = FALSE]
        ),
        extra_args
      )
      do.call(wright_map_ascii, map_args)
    },
    thresholds = {
      if (!all_polytomous) {
        stop("Threshold maps currently require all scored items to be polytomous.", call. = FALSE)
      }
      if (isTRUE(include_scores) && is.null(extra_args$score_method)) {
        extra_args$score_method <- "observed"
      }
      map_args <- c(
        list(
          persons = person_df,
          items = item_spec$threshold_points
        ),
        extra_args
      )
      do.call(wright_map_ascii, map_args)
    },
    range = {
      if (!all_polytomous) {
        stop("Range maps currently require all scored items to be polytomous.", call. = FALSE)
      }
      if ("score_method" %in% names(extra_args)) {
        stop("'score_method' is not used for range maps created by wright_map_mirt().", call. = FALSE)
      }
      map_args <- c(
        list(
          persons = person_df,
          range_points = item_spec$range_points
        ),
        extra_args
      )
      do.call(range_map_ascii_from_points, map_args)
    }
  )

  map$mirt <- list(
    map_type = map_type,
    map_type_used = map_type_used,
    theta_method = theta_method,
    include_scores = isTRUE(include_scores),
    freq_column = response_info$freq_column,
    item_types = unname(item_spec$item_types),
    categories = unname(item_spec$K)
  )
  map$settings$mirt <- map$mirt
  map
}
