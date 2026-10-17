test_that("versions do not overlap in time within a closure", {
  cl <- sf::st_drop_geometry(closures)
  for (id in unique(cl$closure_id)) {
    x <- cl[cl$closure_id == id, ]; x <- x[order(x$valid_from), ]
    if (nrow(x) > 1) expect_true(all(x$valid_from[-1] > x$valid_to[-nrow(x)]), info = id)
  }
  expect_true(all(is.na(cl$valid_to) | cl$valid_to >= cl$valid_from))
})

test_that("every version has a gear and a source reference", {
  expect_false(anyNA(closures$gear_closed))
  expect_true(all(grepl("^F20", closures$source_ref)))
  expect_false(any(sf::st_is_empty(closures)))
})

test_that("closures_on returns what was in force", {
  x <- closures_on("2010-07-01")
  expect_s3_class(x, "sf")
  expect_true(all(x$valid_from <= as.Date("2010-07-01")))
  expect_gt(nrow(closures_on("2024-01-01")), 50)
  expect_equal(nrow(closures_on("2007-03-01")), 0)        # gap Jan-Jul 2007
})

test_that("gear matching follows the directions' wording", {
  expect_equal(gear_applies(c("trawl", "demersal otter trawl", "gillnet", "all methods", "hook", "automatic longline"),
                            "Danish seine"), c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(gear_applies("hook", "automatic longline"), TRUE)
  expect_equal(gear_applies("automatic longline", "dropline"), FALSE)
  expect_equal(gear_applies("none (orange roughy management area)", "TW"), FALSE)
  expect_equal(gear_class(c("TW", "DS", "GN", "AL", "DL")),
               c("otter trawl", "Danish seine", "gillnet", "automatic longline", "hook"))
})

test_that("in_closure finds a point inside the ECDW exclusion zone", {
  s <- data.frame(lon = 155.8, lat = -28, date = as.Date("2020-01-01"), gear = "TW")
  r <- in_closure(s, gear = "gear")
  expect_true(any(grepl("East Coast Deepwater", r$area)))
  expect_true(all(r$applies[grepl("East Coast Deepwater", r$area)]))
  expect_equal(nrow(in_closure(data.frame(lon = 120, lat = -20, date = as.Date("2020-01-01")))), 0)
})

test_that("bundled files are installed", {
  expect_true(file.exists(closures_gpkg()))
  p <- closure_source(closures$version_id[1])$pdf
  expect_true(file.exists(p))
})
