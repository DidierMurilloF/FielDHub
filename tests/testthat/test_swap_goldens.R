# Bit-identity gate for the pair-swap optimizer (spec-m15 section 4.4): every
# result recorded from master 720449e must be reproduced with identical(). The
# fixture is platform-specific (sum() accumulates in long double on x86_64) and
# is not shipped to CRAN (.Rbuildignore).
test_that("the pair-swap optimizer reproduces its pre-change goldens", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("FIELDHUB_GOLDEN"), "true"), "set FIELDHUB_GOLDEN=true")
  path <- test_path("fixtures", "swap-goldens.rds")
  skip_if_not(file.exists(path), "fixture not in this build")
  g <- readRDS(path)
  skip_if_not(identical(g$platform, R.version$platform), "goldens are platform-specific")
  new_parameters <- c("rowcol_penalty", "rowcol_scope")
  normalize <- function(v) {
    if (is.list(v$metadata)) {
      # The version changes with every release, not with the design
      v$metadata$package_version <- NULL
      p <- v$metadata$parameters
      if (all(new_parameters %in% names(p))) {
        # Gate B1: the parameters gain exactly the two new names, at their defaults, at the end
        expect_identical(utils::tail(names(p), 2L), new_parameters)
        expect_identical(p$rowcol_penalty, 0)
        expect_identical(p$rowcol_scope, "pairs")
        v$metadata$parameters <- p[setdiff(names(p), new_parameters)]
      }
    }
    v
  }
  old_kind <- RNGkind()
  on.exit(suppressWarnings(do.call(RNGkind, as.list(old_kind))), add = TRUE)
  for (r in g$results) {
    suppressWarnings(do.call(RNGkind, as.list(r$rng_kind)))
    value <- suppressWarnings(do.call(r$fun, r$args))
    expect_identical(normalize(value), normalize(r$value), label = r$label)
  }
})

test_that("stored pre-M1.5 results validate and replay to the same layout", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("FIELDHUB_GOLDEN"), "true"), "set FIELDHUB_GOLDEN=true")
  path <- test_path("fixtures", "swap-goldens.rds")
  skip_if_not(file.exists(path), "fixture not in this build")
  g <- readRDS(path)
  skip_if_not(identical(g$platform, R.version$platform), "goldens are platform-specific")
  default_kind <- c("Mersenne-Twister", "Inversion", "Rejection")
  pick <- function(fun) {
    i <- which(vapply(g$results, function(r) identical(r$fun, fun) && identical(r$rng_kind, default_kind),
                      logical(1)))[1]
    g$results[[i]]$value
  }
  old_swap <- pick("swap_pairs")
  expect_false("rowcol_penalty" %in% names(old_swap$metadata$parameters))
  expect_silent(validate_fieldhub_optimization(old_swap))
  expect_identical(suppressWarnings(reproduce_design(old_swap))$optim_design, old_swap$optim_design)
  old_prep <- pick("partially_replicated")
  expect_false("rowcol_penalty" %in% names(old_prep$metadata$parameters))
  expect_identical(suppressWarnings(reproduce_design(old_prep))$fieldBook, old_prep$fieldBook)
})
