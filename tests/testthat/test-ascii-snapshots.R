test_that("basic Wright map output stays aligned", {
  dat <- example_wright_data()
  map <- wright_map_ascii(
    dat$persons,
    dat$items,
    name_trunc = 4
  )

  expect_ascii_snapshot(map, "wright-map-basic.txt")
})

test_that("mirrored easiness map paging stays stable", {
  dat <- example_wright_data()
  map <- wright_map_ascii(
    dat$persons,
    dat$items,
    person_scores = dat$person_scores,
    table_style = "table1.11",
    max_page = 12
  )

  expect_ascii_snapshot(map, "wright-map-mirrored-paged.txt")
})

test_that("polytomous threshold preset output stays stable", {
  pdat <- example_polytomous_data()
  map <- polytomous_threshold_map_ascii(
    persons = pdat$persons,
    items = pdat$items,
    steps = pdat$steps,
    table_style = "table1.6",
    line_length = 60,
    max_page = 10
  )

  expect_ascii_snapshot(map, "polytomous-threshold-paged.txt")
})

test_that("polytomous range preset output stays stable", {
  pdat <- example_polytomous_data()
  map <- polytomous_range_map_ascii(
    persons = pdat$persons,
    items = pdat$items,
    steps = pdat$steps,
    table_style = "table1.4",
    line_length = 72,
    max_page = 10
  )

  expect_ascii_snapshot(map, "polytomous-range-paged.txt")
})
