test_that("wright_map_ascii returns printable text output", {
  dat <- example_wright_data()
  map <- wright_map_ascii(dat$persons, dat$items, name_trunc = 4)
  rendered <- format(map)
  axis_pos <- regexpr("\\+", rendered[3L])[1L]

  expect_s3_class(map, "ascii_wright_map")
  expect_true(length(rendered) > 5)
  expect_true(any(grepl("MAP", rendered, fixed = TRUE)))
  expect_true(any(grepl("M", rendered, fixed = TRUE)))
  expect_true(any(grepl("\\+", rendered)))
  expect_equal(regexpr("\\|", rendered[2L])[1L], axis_pos)
  expect_equal(regexpr("\\|", rendered[length(rendered)])[1L], axis_pos)
  expect_false(any(rendered == ""))
})

test_that("wright_map_ascii accepts data frame inputs", {
  person_df <- data.frame(
    label = c("Ana", "Ben", "Cid"),
    measure = c(1.2, 0.1, -0.8),
    stringsAsFactors = FALSE
  )
  item_df <- data.frame(
    label = c("I1", "I2"),
    difficulty = c(0.5, -1.0),
    stringsAsFactors = FALSE
  )

  map <- wright_map_ascii(person_df, item_df)

  txt <- as.character(map)
  expect_match(txt, "Ana")
  expect_match(txt, "I1")
})

test_that("wright_map_ascii supports distribution views", {
  dat <- example_wright_data()

  mixed <- wright_map_ascii(
    dat$persons,
    dat$items,
    person_display = "distribution",
    distribution_style = "winsteps"
  )
  dense <- wright_map_ascii(
    dat$persons,
    dat$items,
    person_display = "distribution",
    item_display = "distribution",
    distribution_style = "winsteps",
    label_width = 3
  )

  expect_match(as.character(mixed), "X", fixed = TRUE)
  expect_match(as.character(dense), "#", fixed = TRUE)
  expect_match(as.character(dense), "EACH \"#\" IS", fixed = TRUE)
  expect_equal(mixed$settings$person_display, "distribution")
  expect_equal(dense$settings$item_display, "distribution")
})

test_that("wright_map_ascii supports score columns", {
  dat <- example_wright_data()

  map <- wright_map_ascii(
    dat$persons,
    dat$items,
    person_scores = dat$person_scores,
    person_display = "distribution",
    item_display = "distribution",
    distribution_style = "winsteps",
    label_width = 10
  )

  rendered <- format(map)
  expect_match(rendered[1], "SCORE", fixed = TRUE)
  expect_true(any(grepl("\\b15\\b", rendered)))
  expect_true(map$settings$has_scores)
  expect_equal(map$settings$score_method_used, "rasch")
  expect_equal(map$settings$score_extreme_adjust, 0.3)
})

test_that("wright_map_ascii groups duplicated scores to one representative row", {
  persons <- c(a = 1.3, b = 1.1, c = -0.2, d = -0.4)
  items <- c(i1 = 0.5, i2 = -0.5)
  scores <- c(30, 30, 20, 20)

  map <- wright_map_ascii(
    persons,
    items,
    person_scores = scores,
    person_display = "distribution",
    item_display = "distribution",
    distribution_style = "winsteps",
    measure_range = c(-1, 2),
    label_width = 5
  )

  rendered <- format(map)
  expect_equal(sum(grepl("\\b30\\b", rendered)), 1)
  expect_equal(sum(grepl("\\b20\\b", rendered)), 1)
})

test_that("wright_map_ascii supports observed score placement fallback", {
  persons <- c(a = 1.3, b = 1.1, c = -0.2, d = -0.4)
  items <- c(i1 = 0.5, i2 = -0.5)
  scores <- c("H", "H", "L", "L")

  map <- wright_map_ascii(
    persons,
    items,
    person_scores = scores,
    score_method = "observed",
    person_display = "distribution",
    item_display = "distribution"
  )

  expect_equal(map$settings$score_method_used, "observed")
  expect_true(any(grepl("\\bH\\b", format(map))))
  expect_true(any(grepl("\\bL\\b", format(map))))
})

test_that("wright_map_ascii supports double-axis mirrored measure layout", {
  dat <- example_wright_data()

  map <- wright_map_ascii(
    dat$persons,
    dat$items,
    person_scores = dat$person_scores,
    person_display = "distribution",
    item_display = "distribution",
    distribution_style = "winsteps",
    label_width = 6,
    axis_style = "double",
    right_measure = TRUE
  )

  rendered <- format(map)
  expect_match(rendered[1], "MEASURE", fixed = TRUE)
  expect_true(grepl("MEASURE\\s*$", rendered[1]))
  expect_match(rendered[2], "-\\+\\+-")
  expect_true(any(grepl("\\+\\+", rendered)))
  expect_true(any(grepl("\\|\\|", rendered)))
  expect_true(map$settings$right_measure)
  expect_equal(map$settings$axis_style, "double")
})

test_that("wright_map_ascii supports smart abbreviations and label overrides", {
  persons <- c(A = 1.2, B = 0.2, C = -1.1)
  items <- c(
    `Read books on animals` = -0.5,
    `Find where animal lives` = -1.5
  )

  smart_map <- wright_map_ascii(
    persons,
    items,
    person_display = "distribution",
    label_width = 18,
    name_trunc = 18,
    label_abbrev = "smart"
  )
  override_map <- wright_map_ascii(
    persons,
    items,
    person_display = "distribution",
    label_width = 18,
    name_trunc = 18,
    label_abbrev = "smart",
    label_overrides = c(`Find where animal lives` = "Animal habitat")
  )

  expect_true(any(grepl("Read books animals", format(smart_map), fixed = TRUE)))
  expect_true(any(grepl("Animal habitat", format(override_map), fixed = TRUE)))
})

test_that("preview_label_abbrev previews suggested and overridden labels", {
  preview <- preview_label_abbrev(
    c("Read books on animals", "Find where animal lives"),
    width = 18,
    style = "smart",
    label_overrides = c(`Find where animal lives` = "Animal habitat")
  )

  expect_s3_class(preview, "data.frame")
  expect_equal(preview$suggested[[1]], "Read books animals")
  expect_equal(preview$final[[2]], "Animal habitat")
  expect_true(preview$changed[[1]])
  expect_true(preview$used_override[[2]])
})

test_that("make_label_overrides builds named vectors from edited previews", {
  preview <- preview_label_abbrev(
    c("Read books on animals", "Find where animal lives"),
    width = 18,
    style = "smart"
  )
  preview$final[preview$label == "Find where animal lives"] <- "Animal habitat"

  overrides <- make_label_overrides(preview)

  expect_equal(
    unname(overrides[c("Read books on animals", "Find where animal lives")]),
    c("Read books animals", "Animal habitat")
  )
  expect_identical(names(overrides), c("Read books on animals", "Find where animal lives"))
})

test_that("wright_map_ascii validates label lengths", {
  expect_error(
    wright_map_ascii(c(a = 1, b = 2), c(i = 0), person_labels = "only-one"),
    "Labels must have the same length as measures"
  )
})

test_that("wright_map_ascii validates score lengths", {
  expect_error(
    wright_map_ascii(c(a = 1, b = 2), c(i = 0), person_scores = 1),
    "Scores must have the same length as person measures"
  )
})

test_that("write_wright_map writes txt and markdown outputs", {
  dat <- example_wright_data()
  map <- wright_map_ascii(dat$persons, dat$items, person_scores = dat$person_scores)

  txt_file <- tempfile(fileext = ".txt")
  md_file <- tempfile(fileext = ".md")

  write_wright_map(map, txt_file)
  write_wright_map(map, md_file)

  txt_lines <- readLines(txt_file, warn = FALSE)
  md_lines <- readLines(md_file, warn = FALSE)

  expect_identical(txt_lines, format(map))
  expect_identical(md_lines[1], "```text")
  expect_identical(md_lines[length(md_lines)], "```")
  expect_identical(md_lines[2:(length(md_lines) - 1)], format(map))
})

test_that("score_table_ascii returns a printable Winsteps-style table", {
  dat <- example_wright_data()
  tbl <- score_table_ascii(dat$items, columns = 3)
  txt <- format(tbl)

  expect_s3_class(tbl, "ascii_score_table")
  expect_match(txt[1], "TABLE OF MEASURES ON TEST", fixed = TRUE)
  expect_true(any(grepl("SCORE", txt, fixed = TRUE)))
  expect_true(any(grepl("MEASURE", txt, fixed = TRUE)))
  expect_true(any(grepl("E", txt, fixed = TRUE)))
})

test_that("score_table_ascii can omit extremes", {
  items <- c(i1 = -1, i2 = 0, i3 = 1)
  tbl <- score_table_ascii(items, include_extremes = FALSE, columns = 2)
  scores <- tbl$scores$score

  expect_false(any(scores %in% c(0, length(items))))
  expect_equal(scores, c(1, 2))
})

test_that("polytomous_threshold_map_ascii expands half-point thresholds", {
  dat <- example_polytomous_data()
  map <- polytomous_threshold_map_ascii(
    persons = dat$persons,
    items = dat$items[1],
    steps = dat$steps,
    mode = "halfpoint",
    person_display = "distribution",
    label_width = 18
  )

  expect_s3_class(map, "ascii_wright_map")
  expect_equal(map$items$measure, c(-1.76, 0.81), tolerance = 0.03)
  expect_true(all(grepl("\\.05$|\\.15$", map$items$label)))
  expect_true(any(grepl("\\.05", format(map))))
})

test_that("polytomous_threshold_map_ascii matches Winsteps-style threshold locations", {
  item <- c(`Watch birds` = -0.47)
  steps <- c(-1.03, 1.03)
  persons <- c(a = 1, b = 0, c = -1)

  thurstonian <- polytomous_threshold_map_ascii(
    persons = persons,
    items = item,
    steps = steps,
    mode = "thurstonian",
    person_display = "distribution",
    label_width = 18
  )
  andrich <- polytomous_threshold_map_ascii(
    persons = persons,
    items = item,
    steps = steps,
    mode = "andrich",
    person_display = "distribution",
    label_width = 18
  )
  center <- polytomous_threshold_map_ascii(
    persons = persons,
    items = item,
    steps = steps,
    mode = "center",
    person_display = "distribution",
    label_width = 18
  )

  expect_equal(thurstonian$items$measure, c(-1.61, 0.66), tolerance = 0.03)
  expect_equal(andrich$items$measure, c(-1.50, 0.56), tolerance = 0.03)
  expect_equal(center$items$measure, c(-2.68, -0.47, 1.74), tolerance = 0.03)
})

test_that("polytomous_range_map_ascii produces bottom-measure-top columns", {
  pdat <- example_polytomous_data()
  map <- polytomous_range_map_ascii(
    persons = pdat$persons,
    items = pdat$items[1],
    steps = pdat$steps,
    person_display = "distribution",
    item_display = "labels",
    item_width = 18
  )

  expect_s3_class(map, "ascii_wright_map")
  expect_equal(map$range_points$bottom$measure, -1.61, tolerance = 0.03)
  expect_equal(map$range_points$center$measure, -0.47, tolerance = 0.03)
  expect_equal(map$range_points$top$measure, 0.66, tolerance = 0.03)
  expect_true(any(grepl("BOTTOM P=50%", format(map), fixed = TRUE)))
  expect_true(any(grepl("TOP P=50%", format(map), fixed = TRUE)))
  expect_true(grepl("\\| BOTTOM P=50%", format(map)[1]))
  expect_true(grepl("-\\+-", format(map)[2]))
})

test_that("polytomous_range_map_ascii supports side-aware label overrides", {
  pdat <- example_polytomous_data()
  map <- polytomous_range_map_ascii(
    persons = pdat$persons,
    items = pdat$items[2],
    steps = pdat$steps,
    person_display = "distribution",
    item_display = "labels",
    item_width = 18,
    name_trunc = 18,
    label_abbrev = "smart",
    label_overrides = function(label, width, side) {
      if (side == "item_bottom") {
        return("Bottom animals")
      }
      if (side == "item_center") {
        return("Center animals")
      }
      if (side == "item_top") {
        return("Top animals")
      }
      label
    }
  )

  rendered <- format(map)
  expect_true(any(grepl("Bottom animals", rendered, fixed = TRUE)))
  expect_true(any(grepl("Center animals", rendered, fixed = TRUE)))
  expect_true(any(grepl("Top animals", rendered, fixed = TRUE)))
  expect_equal(map$settings$label_abbrev, "smart")
})

test_that("write_wright_map also writes score tables", {
  dat <- example_wright_data()
  tbl <- score_table_ascii(dat$items)
  txt_file <- tempfile(fileext = ".txt")

  write_wright_map(tbl, txt_file)
  txt_lines <- readLines(txt_file, warn = FALSE)

  expect_identical(txt_lines, format(tbl))
})

test_that("wright_map_mirt builds item maps from dichotomous mirt models", {
  skip_if_not_installed("mirt")

  data("LSAT7", package = "mirt")
  mod <- mirt::mirt(LSAT7, 1, itemtype = "Rasch", verbose = FALSE)
  map <- wright_map_mirt(mod, map_type = "items", label_width = 8, name_trunc = 8)

  expect_s3_class(map, "ascii_wright_map")
  expect_equal(map$mirt$map_type_used, "items")
  expect_equal(nrow(map$persons), sum(LSAT7[, "freq"]))
  expect_false(any(map$items$label == "freq"))
  expect_true(any(grepl("Item.1", format(map), fixed = TRUE)))
})

test_that("wright_map_mirt auto-detects polytomous threshold maps", {
  skip_if_not_installed("mirt")

  data("Science", package = "mirt")
  mod <- mirt::mirt(Science, 1, itemtype = "graded", verbose = FALSE)
  map <- wright_map_mirt(
    mod,
    map_type = "auto",
    person_display = "distribution",
    label_width = 20,
    name_trunc = 20
  )

  expect_s3_class(map, "ascii_wright_map")
  expect_equal(map$mirt$map_type_used, "thresholds")
  expect_true(any(grepl("\\.1|\\.2|\\.3", map$items$label)))
  expect_equal(map$settings$score_method_used, "observed")
})

test_that("wright_map_mirt can build range maps from polytomous models", {
  skip_if_not_installed("mirt")

  data("Science", package = "mirt")
  mod <- mirt::mirt(Science, 1, itemtype = "graded", verbose = FALSE)
  map <- wright_map_mirt(
    mod,
    map_type = "range",
    person_display = "distribution",
    item_display = "labels",
    item_width = 18,
    name_trunc = 18
  )

  expect_s3_class(map, "ascii_wright_map")
  expect_equal(map$mirt$map_type_used, "range")
  expect_true(any(grepl("BOTTOM P=50%", format(map), fixed = TRUE)))
  expect_true(any(grepl("TOP P=50%", format(map), fixed = TRUE)))
})
