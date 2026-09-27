test_that("blank app seeds select the recorded automatic-seed path", {
  set.seed(419)
  before <- .Random.seed
  for (value in list(NULL, numeric(), character(), NA_real_, NA_integer_, "", "  ")) {
    expect_null(read_app_seed(value))
  }
  expect_identical(.Random.seed, before)
})

test_that("app seed parsing preserves valid numeric conversion", {
  for (value in list(0, 0L, -17, 123L, 7.9, "123", " -17 ", "1e3", c(seed = 42))) {
    expect_identical(read_app_seed(value), as.numeric(value))
  }
  for (value in list(TRUE, FALSE, NaN, Inf, -Inf, "NA", "NaN", "bad", "1,2", NA_character_,
                     list(12), matrix(12), c(1, 2), .Machine$integer.max + 1)) {
    expect_error(read_app_seed(value), class = "fieldhub_input_error")
  }
})

test_that("automatic simulation seeds come from the accepted design", {
  design <- CRD(t = 4, reps = 2, seed = NULL)
  expect_identical(workflow_seed(NULL, design), design$metadata$seed)
  expect_identical(workflow_seed(numeric(), design), design$metadata$seed)
  expect_identical(workflow_seed(19, stop("Do not evaluate the design")), 19)
  expect_error(workflow_seed(NULL, list()), class = "fieldhub_internal_error")
  settings <- list(min_value = 10, max_value = 30, response_name = "Yield")
  expect_identical(
    classic_workflow_book(design$fieldBook, settings, workflow_seed(NULL, design)),
    classic_workflow_book(design$fieldBook, settings, design$metadata$seed))
})

test_that("a run resolves an automatic seed once for allocation and field construction", {
  set.seed(419)
  before <- .Random.seed
  seed <- resolve_seed(read_app_seed(NULL))
  allocation <- do_optim(design = "sparse", lines = 120, l = 4,
    copies_per_entry = 3, add_checks = TRUE, checks = 4, seed = seed)
  design <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3,
    checks = 4, sparse_list = allocation, seed = seed, year = 2026)
  expect_identical(allocation$metadata$seed, seed)
  expect_identical(design$metadata$seed, seed)
  expect_identical(reproduce_design(design), design)
  expect_identical(.Random.seed, before)
})

test_that("one seed widget shares labels and API bounds while retaining supplied defaults", {
  skip_if_not_installed("shiny")
  html <- as.character(app_seed_input("trial-seed", value = 123))
  expect_match(html, "trial-seed", fixed = TRUE)
  expect_match(html, "blank = automatic", fixed = TRUE)
  expect_match(html, 'value="123"', fixed = TRUE)
  expect_match(html, 'min="-2147483647"', fixed = TRUE)
  expect_match(html, 'max="2147483647"', fixed = TRUE)
})

test_that("every design module uses shared seed controls without requiring a value", {
  namespace <- asNamespace("FielDHub")
  seed_resolvers <- function(expr) {
    if (missing(expr) || (!is.call(expr) && !is.pairlist(expr))) return(0L)
    resolves_input <- is.call(expr) && identical(expr[[1]], as.name("resolve_seed")) &&
      length(expr) == 2L && is.call(expr[[2]]) && identical(expr[[2]][[1]], as.name("read_app_seed"))
    as.integer(resolves_input) + sum(vapply(as.list(expr), seed_resolvers, integer(1)))
  }
  seed_requirements <- function(expr) {
    if (!is.call(expr) && !is.pairlist(expr)) return(FALSE)
    if (is.call(expr) && identical(expr[[1]], quote(shiny::req)) &&
        any(grepl("seed", vapply(as.list(expr)[-1], function(arg)
          paste(deparse(arg), collapse = ""), character(1)), ignore.case = TRUE))) return(TRUE)
    any(vapply(as.list(expr), seed_requirements, logical(1)))
  }
  for (entry in fieldhub_app_registry()) {
    server <- body(get(entry$server, namespace))
    ui <- body(get(entry$ui, namespace))
    expect_true("read_app_seed" %in% all.names(server), info = entry$id)
    expect_true("app_seed_input" %in% all.names(ui), info = entry$id)
    expect_false(seed_requirements(server), info = entry$id)
    expect_identical(seed_resolvers(server), 1L, info = entry$id)
  }
  for (workflow in c("app_classic_workflow", "app_spatial_workflow")) {
    expect_true("workflow_seed" %in% all.names(body(get(workflow, namespace))), info = workflow)
  }
})
