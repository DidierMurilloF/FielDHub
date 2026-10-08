library(FielDHub)

# Regression tests: these inputs failed with errors that did not say what
# was wrong ("wrong sign in 'by' argument", "object 'nt' not found",
# "object 'WholePlots' not found", "argument of length 0",
# "non-character argument").

test_that("latin_square() explains that it needs more than one treatment", {
  expect_error(latin_square(t = 1, seed = 1), "more than one treatment")
})

test_that("CRD() accepts treatments given as a factor", {
  crd <- CRD(t = factor(c("A", "B", "C")), reps = 2, seed = 1)
  expect_equal(as.vector(table(crd$fieldBook$TREATMENT)), c(2L, 2L, 2L))
  expect_setequal(unique(crd$fieldBook$TREATMENT), c("A", "B", "C"))
})

test_that("split plots accept a count of whole plots with sub-plot labels", {
  spd <- split_plot(wp = 3, sp = c("a", "b"), reps = 2, seed = 1)
  expect_setequal(unique(spd$fieldBook$WHOLE_PLOT), 1:3)
  expect_setequal(unique(spd$fieldBook$SUB_PLOT), c("a", "b"))
  expect_equal(nrow(spd$fieldBook), 3 * 2 * 2)
  sspd <- split_split_plot(wp = 3, sp = c("a", "b"), ssp = 2, reps = 2, seed = 1)
  expect_setequal(unique(sspd$fieldBook$WHOLE_PLOT), 1:3)
  expect_setequal(unique(sspd$fieldBook$SUB_PLOT), c("a", "b"))
  expect_equal(nrow(sspd$fieldBook), 3 * 2 * 2 * 2)
})

test_that("split_families() asks for the number of locations", {
  df <- data.frame(ENTRY = 1:20, NAME = paste0("G", 1:20), FAMILY = rep(1:4, 5))
  expect_error(split_families(data = df), "number of locations")
})

test_that("sparse_allocation() explains when no field dimensions fit", {
  expect_error(
    sparse_allocation(lines = 60, l = 3, copies_per_entry = 2, checks = 3, seed = 1),
    "no field dimension options"
  )
})

test_that("degenerate factors and replication give classed input errors", {
  # Regression test: these used to crash with leaked, unclassed errors: base
  # R's "incorrect number of dimensions" for a one-level sub-/strip-plot
  # factor, and blocksdesign's "Too many parameters for the available plots"
  # for under-replicated block designs.
  expect_error(split_plot(wp = 2, sp = 1, reps = 2, seed = 1), class = "fieldhub_input_error")
  expect_error(split_split_plot(wp = 2, sp = 2, ssp = 1, reps = 2, seed = 1),
               class = "fieldhub_input_error")
  expect_error(strip_plot(Hplots = 2, Vplots = 1, reps = 2, seed = 1), class = "fieldhub_input_error")
  expect_error(incomplete_blocks(t = 4, k = 2, reps = 1, seed = 1), class = "fieldhub_input_error")
  # row_column() first warns (fieldhub_design_warning) that the default
  # method = "onestage" is infeasible and falls back to "twostage", which
  # then also fails for this under-replicated design.
  expect_error(suppressWarnings(row_column(t = 4, nrows = 2, reps = 1, seed = 1)),
               class = "fieldhub_input_error")
  for (f in list(alpha_lattice, square_lattice, rectangular_lattice)) {
    expect_error(f(t = 16, k = 4, reps = 1, seed = 1), class = "fieldhub_input_error")
  }
  # Regression test: full_factorial() delegated a single-run design to RCBD(),
  # which raised a "RCBD() requires..." error that named the wrong function.
  e <- expect_error(full_factorial(setfactors = c(1, 1), reps = 2, seed = 1),
                    class = "fieldhub_input_error")
  expect_false(grepl("RCBD", conditionMessage(e)))
})

test_that("alpha_lattice(), square_lattice() and rectangular_lattice() name themselves, not incomplete_blocks()", {
  # Regression test: these engines build their design through
  # incomplete_blocks() and used to let its condition propagate unchanged, so
  # an under-replicated design, or k >= t, raised an error that said
  # "incomplete_blocks() ..." even when the user called, say, alpha_lattice().
  under_replicated <- list(
    alpha_lattice = quote(alpha_lattice(t = 16, k = 4, reps = 1, seed = 1)),
    square_lattice = quote(square_lattice(t = 16, k = 4, reps = 1, seed = 1)),
    rectangular_lattice = quote(rectangular_lattice(t = 12, k = 3, reps = 1, seed = 1))
  )
  for (fn in names(under_replicated)) {
    e <- expect_error(eval(under_replicated[[fn]]), class = "fieldhub_input_error", info = fn)
    expect_match(conditionMessage(e), paste0("^", fn, "\\("), info = fn)
    expect_false(grepl("incomplete_blocks", conditionMessage(e), fixed = TRUE), info = fn)
  }

  # The pre-existing k >= t check in alpha_lattice() had the same defect.
  e <- expect_error(alpha_lattice(t = 16, k = 20, reps = 2, seed = 1),
                    class = "fieldhub_input_error")
  expect_match(conditionMessage(e), "^alpha_lattice\\(")
  expect_false(grepl("incomplete_blocks", conditionMessage(e), fixed = TRUE))

  # incomplete_blocks() itself still names incomplete_blocks() for both checks.
  e <- expect_error(incomplete_blocks(t = 4, k = 2, reps = 1, seed = 1),
                    class = "fieldhub_input_error")
  expect_match(conditionMessage(e), "^incomplete_blocks\\(")
  e <- expect_error(incomplete_blocks(t = 4, k = 20, reps = 2, seed = 1),
                    class = "fieldhub_input_error")
  expect_match(conditionMessage(e), "^incomplete_blocks\\(")

  # The internal `caller` argument is not part of the recorded reproduction
  # parameters, so do.call(incomplete_blocks, x$metadata$parameters) is
  # unaffected.
  ibd <- incomplete_blocks(t = 12, k = 4, reps = 2, seed = 1)
  expect_false("caller" %in% names(ibd$metadata$parameters))
})

test_that("designs stay valid with a single whole plot or a single replicate", {
  # split_plot()/split_split_plot() only crash when the *sub*- or sub-sub-
  # plot factor collapses to one level; a single whole plot, and a single
  # replicate, are both still meaningful designs and must keep working.
  expect_s3_class(split_plot(wp = 1, sp = 2, reps = 2, seed = 1), "FielDHub")
  expect_s3_class(split_split_plot(wp = 1, sp = 2, ssp = 2, reps = 2, seed = 1), "FielDHub")
  expect_s3_class(split_plot(wp = 2, sp = 2, reps = 1, seed = 1), "FielDHub")
})
