classic_simulation_book <- function() {
  data.frame(ID = c(4L, 2L, 3L, 1L), LOCATION = c("B", "B", "A", "A"),
             PLOT = c(2L, 1L, 2L, 1L), TREATMENT = c("x", "y", "x", "y"))
}

test_that("classic simulation records replayable inputs and preserves the caller's RNG for explicit seeds", {
  set.seed(52)
  before <- .Random.seed
  for (seed in list(19, 19.75, -8)) {
    result <- simulate_classic_field_book(classic_simulation_book(), 0, 100, "YIELD", seed)
    expect_identical(.Random.seed, before)
    expect_identical(result$input_field_book, classic_simulation_book())
    expect_identical(result$metadata$model, "truncated_normal")
    expect_type(result$metadata$seed, "integer")
    expect_identical(result$metadata$rng_kind, RNGkind())
    expect_identical(result$metadata$package_version, as.character(utils::packageVersion("FielDHub")))
    replay <- do.call(simulate_classic_field_book,
                      c(list(field_book = result$input_field_book), result$metadata$parameters))
    expect_identical(replay, result)
    expect_identical(.Random.seed, before)
  }
})

test_that("a seedless classic simulation consumes exactly one draw from the caller's stream", {
  set.seed(52)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(52)
  result <- simulate_classic_field_book(classic_simulation_book(), 0, 100, "YIELD", NULL)
  expect_identical(.Random.seed, after_one_draw)
  expect_identical(result$input_field_book, classic_simulation_book())
  expect_identical(result$metadata$model, "truncated_normal")
  expect_identical(result$metadata$seed, expected)
  expect_identical(result$metadata$rng_kind, RNGkind())
  expect_identical(result$metadata$package_version, as.character(utils::packageVersion("FielDHub")))
  replay <- do.call(simulate_classic_field_book,
                    c(list(field_book = result$input_field_book), result$metadata$parameters))
  expect_identical(replay, result)
  expect_identical(.Random.seed, after_one_draw)
})

test_that("classic simulation validates response names and optional ID sorting", {
  book <- classic_simulation_book()
  ordered <- simulate_classic_field_book(book, 0, 100, "Dry yield (kg)", 7, order_by_id = TRUE)
  expect_identical(ordered$field_book$ID, 1:4)
  expect_true("Dry yield (kg)" %in% names(ordered$field_book))
  expect_false("RESP" %in% names(ordered$field_book))
  for (name in list(NULL, "", "  ", NA_character_, c("a", "b"), "PLOT", "text")) {
    expect_error(simulate_classic_field_book(book, 0, 100, name, 7), class = "fieldhub_input_error")
  }
  for (value in list(NULL, 1, NA, c(TRUE, FALSE))) {
    expect_error(simulate_classic_field_book(book, 0, 100, "YIELD", 7, value), class = "fieldhub_input_error")
  }
  expect_error(simulate_classic_field_book(book[names(book) != "ID"], 0, 100, "YIELD", 7, TRUE),
               class = "fieldhub_input_error")
})

test_that("every classic module delegates response generation to the shared service", {
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    code <- design_server_body(module)
    symbols <- all.names(code)
    expect_identical(sum(symbols == "app_classic_workflow"), 1L)
    expect_false("norm_trunc" %in% symbols)
  }
  expect_identical(sum(all.names(body(app_classic_workflow)) == "classic_workflow_book"), 1L)
  expect_identical(sum(all.names(body(classic_workflow_book)) == "simulate_classic_field_book"), 1L)
})
