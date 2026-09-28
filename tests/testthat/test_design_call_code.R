test_that("standalone calls rebuild every catalogue result", {
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    code <- design_call_code(x)
    expect_type(code, "character")
    expect_true(length(code) > 0L, info = name)
    rebuilt <- suppressWarnings(eval(parse(text = code), envir = new.env(parent = baseenv())))
    expect_identical(rebuilt, x, info = name)
  }
})

test_that("standalone calls omit constant defaults and always retain seed and year", {
  x <- RCBD(t = 5, reps = 3, seed = 11)
  code <- paste(design_call_code(x), collapse = "\n")
  expect_match(code, "FielDHub::RCBD(", fixed = TRUE)
  expect_match(code, "seed = 11", fixed = TRUE)
  expect_false(grepl("planter =", code, fixed = TRUE))
  expect_false(grepl("continuous =", code, fixed = TRUE))
  x <- catalogue_design("diagonal_single")
  expect_match(paste(design_call_code(x), collapse = "\n"), 'year = "2026"', fixed = TRUE)
  for (width in c(20L, 60L, 120L, 500L)) {
    expect_identical(eval(parse(text = design_call_code(x, width)), new.env(parent = baseenv())), x)
  }
  for (bad in list(NULL, NA_real_, "80", 1, 501, c(80, 90), matrix(80))) {
    expect_error(design_call_code(x, bad), class = "fieldhub_input_error")
  }
})

test_that("calls quote labels and serialize small uploaded tables as data", {
  labels <- c("No nitrogen", 'A "; stop("not code"); #', "line\nfeed", "back\\slash", "caf\u00e9")
  x <- CRD(data = data.frame(TREATMENT = labels, REPS = rep(2, length(labels))), seed = 21)
  expect_identical(eval(parse(text = design_call_code(x)), new.env(parent = baseenv())), x)
  x <- full_factorial(data = data.frame(FACTOR = c("Water supply", "Water supply", "Dose", "Dose"),
                                        LEVEL = c("Rain fed", "Irrigated", "No nitrogen", "High N")),
                       reps = 2, seed = 21)
  expect_identical(eval(parse(text = design_call_code(x)), new.env(parent = baseenv())), x)
})

test_that("standalone calls retain presence-sensitive explicit defaults", {
  x <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                          exptName = NULL, seed = 35, year = 2026)
  code <- design_call_code(x)
  expect_match(paste(code, collapse = "\n"), "exptName = NULL", fixed = TRUE)
  expect_identical(eval(parse(text = code), new.env(parent = baseenv())), x)
})

test_that("standalone code retains RNG settings without leaking state", {
  local_rng_state()
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE, after = FALSE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  x <- RCBD(t = 5, reps = 3, seed = 11)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(82)
  before <- .Random.seed
  kind <- RNGkind()
  code <- design_call_code(x)
  expect_identical(.Random.seed, before)
  expect_identical(eval(parse(text = code), new.env(parent = baseenv())), x)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  rm(".Random.seed", envir = globalenv())
  expect_identical(eval(parse(text = code), new.env(parent = baseenv())), x)
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
  expect_identical(RNGkind(), kind)
})

test_that("allocation inputs produce a two-step standalone call", {
  for (design in c("sparse", "prep")) {
    allocation <- catalogue_design(paste0("do_optim_", design))
    x <- if (design == "sparse") {
      sparse_allocation(lines = 120, l = 4, copies_per_entry = 3,
                          sparse_list = allocation, checks = 4, seed = 31, year = 2026)
    } else {
      multi_location_prep(lines = 80, l = 4, copies_per_entry = 5,
                           optim_list = allocation, checks = 2, rep_checks = c(4, 4),
                           allow_fillers = TRUE, seed = 31, year = 2026)
    }
    code <- design_call_code(x)
    expect_match(paste(code, collapse = "\n"), "FielDHub::do_optim(", fixed = TRUE)
    expect_match(paste(code, collapse = "\n"), "alloc <-", fixed = TRUE)
    expect_identical(suppressWarnings(eval(parse(text = code), new.env(parent = baseenv()))), x)
    parameter <- if (design == "sparse") "sparse_list" else "optim_list"
    x$metadata$parameters[[parameter]]$metadata <- NULL
    fallback <- design_call_code(x)
    expect_length(fallback, 0L)
    expect_match(attr(fallback, "reason"), "allocation", ignore.case = TRUE)
  }
})

test_that("unsupported records and large uploads explain the RDS fallback", {
  x <- CRD(data = data.frame(TREATMENT = paste0("T", seq_len(201)), REPS = 2), seed = 12)
  code <- design_call_code(x)
  expect_length(code, 0L)
  expect_match(attr(code, "reason"), "200")
  x <- RCBD(t = 5, reps = 3, seed = 11)
  x$metadata$parameters$data <- quote(stop("must not execute"))
  code <- design_call_code(x)
  expect_length(code, 0L)
  expect_match(attr(code, "reason"), "data")
  x$metadata$parameters <- NULL
  code <- design_call_code(x)
  expect_length(code, 0L)
  expect_match(attr(code, "reason"), "recorded parameters")
})

test_that("legacy recordings emit canonical argument names", {
  x <- catalogue_design("optimized_arrangement")
  old <- x
  old$metadata$parameters$amountChecks <- old$metadata$parameters$rep_checks
  old$metadata$parameters$rep_checks <- NULL
  code <- design_call_code(old)
  expect_false(any(grepl("amountChecks", code, fixed = TRUE)))
  expect_identical(eval(parse(text = code), new.env(parent = baseenv())), x)
  legacy <- readRDS(test_path("fixtures", "v1.5.0-designs.rds"))$designs
  for (saved in legacy) {
    code <- design_call_code(with_design_class(saved))
    expect_true(length(code) > 0L || nzchar(attr(code, "reason")))
  }
})

test_that("both reproduction downloads show the standalone call before the RDS path", {
  x <- RCBD(t = 5, reps = 3, seed = 11)
  code <- design_export_handlers(function() x)$code()
  expect_match(code, "FielDHub::RCBD(", fixed = TRUE)
  expect_lt(regexpr("FielDHub::RCBD(", code, fixed = TRUE)[[1]], regexpr("readRDS(", code, fixed = TRUE)[[1]])
  archive <- paste(workflow_reproduction_code(x), collapse = "\n")
  expect_match(archive, "FielDHub::RCBD(", fixed = TRUE)
  expect_match(archive, 'readRDS("workflow.rds")', fixed = TRUE)
  x <- CRD(data = data.frame(TREATMENT = paste0("T", seq_len(201)), REPS = 2), seed = 12)
  expect_match(design_export_handlers(function() x)$code(), "200")
  expect_match(paste(workflow_reproduction_code(x), collapse = "\n"), "200")
})
