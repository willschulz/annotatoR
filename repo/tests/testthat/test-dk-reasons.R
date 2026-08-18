test_that("five-point rhetoric format defines canonical nested DK reasons", {
  path <- testthat::test_path(
    "..", "..", "inst", "label_formats", "rhetoric_placement_5point.json"
  )
  format <- jsonlite::fromJSON(path)
  reasons <- format$button_layout$dk_reasons

  expect_equal(format$button_layout$ui_type, "relative_5point")
  expect_equal(
    reasons$id,
    c("ideol_nodiff", "nonideol", "topics_different")
  )
  expect_equal(reasons$key, c("5", "6", "7"))
  expect_equal(
    reasons$response,
    c(
      "Don't Know: ideological, no difference",
      "Don't Know: both non-ideological",
      "Don't Know: these topics are too different"
    )
  )
  expect_true(all(startsWith(reasons$response, "Don't Know: ")))
})

load_dk_helpers <- function() {
  app_path <- testthat::test_path("..", "..", "inst", "app", "app.R")
  expressions <- parse(app_path)
  helper_names <- c("default_dk_reasons", "get_dk_reasons")
  env <- new.env(parent = baseenv())
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
        is.symbol(expression[[2]]) &&
        as.character(expression[[2]]) %in% helper_names) {
      eval(expression, envir = env)
    }
  }
  env
}

test_that("DK helper uses config and safely supports historical layouts", {
  helpers <- load_dk_helpers()
  configured <- jsonlite::fromJSON(
    testthat::test_path(
      "..", "..", "inst", "label_formats", "rhetoric_placement_5point.json"
    )
  )$button_layout

  expect_equal(
    helpers$get_dk_reasons(configured)$response,
    configured$dk_reasons$response
  )
  expect_equal(
    helpers$get_dk_reasons(list())$response,
    configured$dk_reasons$response
  )
  expect_equal(
    helpers$get_dk_reasons(list(dk_reasons = data.frame(id = "bad")))$response,
    configured$dk_reasons$response
  )
})

