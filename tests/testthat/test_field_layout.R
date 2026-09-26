library(FielDHub)

test_that("the planting path goes along the rows, every second row back for a serpentine", {
  expect_identical(
    planting_path(3, 4, "serpentine"),
    cbind(ROW = rep(1:3, each = 4), COLUMN = c(1:4, 4:1, 1:4))
  )
  expect_identical(
    planting_path(3, 4, "cartesian"),
    cbind(ROW = rep(1:3, each = 4), COLUMN = rep(1:4, times = 3))
  )
})

test_that("field_layout() returns the field book that plot() draws", {
  rcbd <- RCBD(t = 6, reps = 3, l = 2, plotNumber = c(101, 1001), seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    for (planter in c("serpentine", "cartesian")) {
      drawn <- suppressWarnings(plot_layout(rcbd, layout = 2, planter = planter,
                                            stacked = stacked))
      expect_identical(
        field_layout(rcbd, layout = 2, planter = planter, stacked = stacked),
        drawn$allSitesFieldbook
      )
    }
  }
})

test_that("field_layout() puts the identifiers and coordinates first", {
  book <- field_layout(alpha_lattice(t = 12, k = 4, r = 2, seed = 1))
  expect_identical(names(book)[1:5], c("ID", "LOCATION", "PLOT", "ROW", "COLUMN"))
})

test_that("field_layout() returns the field book of designs placed when they are built", {
  prep <- partially_replicated(nrows = 8, ncols = 5, repGens = c(10, 20),
                               repUnits = c(2, 1), seed = 1)
  expect_identical(field_layout(prep), prep$fieldBook)
})

test_that("every layout option gives each plot its own coordinates", {
  # Runs on every platform, unlike the golden snapshots of the layouts
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub") || inherits(design, "fieldhub_split_families")) next
    for (stacked in c("vertical", "horizontal", "grid_panel")) {
      for (planter in c("serpentine", "cartesian")) {
        options <- layout_options(design, planter = planter, stacked = stacked)
        for (books in options) {
          for (book in books) {
            expect_false(anyDuplicated(book[c("ROW", "COLUMN")]) > 0,
                         info = paste(name, stacked, planter))
            expect_setequal(book$ID, book$ID[!duplicated(book$ID)])
          }
        }
      }
    }
  }
})

test_that("field_layout() lists the layout options when one is not available", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  err <- tryCatch(field_layout(rcbd, layout = 99), fieldhub_input_error = function(e) e)
  expect_s3_class(err, "fieldhub_input_error")
  expect_match(conditionMessage(err), "Layout option 99 is not available")
  expect_identical(err$options, seq_along(layout_options(rcbd)[[1]]))
})

test_that("field_layout() checks the planter and the stacking", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  expect_error(field_layout(rcbd, planter = "zigzag"), class = "fieldhub_input_error")
  expect_error(field_layout(rcbd, stacked = "diagonal"), class = "fieldhub_input_error")
  expect_error(field_layout(rcbd, stacked = "grid_panel"), "not available",
               class = "fieldhub_input_error")
  expect_error(field_layout(rcbd$fieldBook), class = "fieldhub_input_error")
})

test_that("field_layout() explains that split_families() results have no layout", {
  set.seed(77)
  families <- split_families(l = 2, data = data.frame(
    ENTRY = 1:40, NAME = paste0("SB-", 1:40), FAMILY = rep(1:8, each = 5)
  ))
  expect_error(field_layout(families), "no field layout", class = "fieldhub_input_error")
})

test_that("a CRD with unequal replication has layouts", {
  # Regression test: plot_layout() assumed every treatment had every rep and
  # failed with "`ROW` must be size 10 or 1, not 12".
  crd <- CRD(data = data.frame(Treatment = paste0("T", 1:4), Reps = c(2, 3, 2, 3)),
             seed = 3)
  options <- layout_options(crd)[[1]]
  expect_length(options, 2)
  expect_identical(c(max(options[[1]]$ROW), max(options[[1]]$COLUMN)), c(2L, 5L))
  expect_identical(c(max(options[[2]]$ROW), max(options[[2]]$COLUMN)), c(5L, 2L))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_s3_class(plot(crd)$p, "ggplot")
})

test_that("split-split plots in complete blocks follow the stacking", {
  # Regression test: plot_layout() did not pass `stacked` on, so every
  # stacking gave the vertical layouts.
  sspd <- split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, plotNumber = 101, seed = 13)
  vertical <- field_layout(sspd, stacked = "vertical")
  horizontal <- field_layout(sspd, stacked = "horizontal")
  # Vertically the reps are stacked in rows, horizontally side by side
  expect_true(all(vertical$ROW[vertical$REP == 1] <= 2))
  expect_true(all(horizontal$COLUMN[horizontal$REP == 1] <= 2))
  expect_false(all(horizontal$ROW[horizontal$REP == 1] <= 2))
})

test_that("plot() explains that a stacking is not available", {
  # Regression test: grid_panel failed with "undefined columns selected" for
  # designs with two reps, and RCBD() designs rejected it with a plain error.
  alpha <- alpha_lattice(t = 12, k = 4, r = 2, seed = 1)
  expect_warning(
    expect_error(plot(alpha, stacked = "grid_panel"), class = "fieldhub_error"),
    "Stacking \"grid_panel\" is not available"
  )
  expect_warning(
    expect_error(plot(RCBD(t = 6, reps = 3, seed = 1), stacked = "grid_panel"),
                 class = "fieldhub_error"),
    "not available"
  )
})
