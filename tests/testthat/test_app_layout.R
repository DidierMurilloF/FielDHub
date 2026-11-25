# The layout controls of the app pages: the 'Replicate layout' and 'Field
# dimensions' menus of the classic pages and the field each option draws,
# the location views, and the field sizes the spatial pages search for.

library(FielDHub)

# The select of a panel: its label, its options as c(name = value), and whether
# it is hidden from the page
panel_select <- function(html, id) {
  at <- regexpr(paste0('(?s)<select[^>]*id="', id, '".*?</select>'), html, perl = TRUE)
  select <- regmatches(html, at)
  found <- regmatches(select, gregexpr('<option value="[^"]*"[^>]*>[^<]*', select))[[1L]]
  label <- regmatches(html, regexpr(paste0('<label[^>]*for="', id, '"[^>]*>[^<]*'), html))
  list(label = sub("^.*>", "", label),
       options = stats::setNames(sub('<option value="([^"]*)".*', "\\1", found),
                                 sub("^.*>", "", found)),
       hidden = grepl(paste0('<div hidden>\\s*<div[^>]*>\\s*<label[^>]*for="', id, '"'), html))
}

# The control `id` of a design page spec.
spec_control <- function(spec, id) {
  Filter(function(control) identical(control$id, id), c(spec$controls, spec$steps))[[1L]]
}

# One expectation that every call signals `class`.
expect_calls_classed <- function(calls, class = "fieldhub_input_error") {
  ok <- vapply(calls, function(f) inherits(tryCatch(f(), error = identity), class), logical(1))
  expect(all(ok), sprintf("case %s does not raise %s",
                          c(names(calls), seq_along(calls))[!ok][1L], class))
}

# One expectation that `f(value)` signals `class` for every value.
expect_all_classed <- function(f, values, label, class = "fieldhub_input_error") {
  ok <- vapply(values, function(value) inherits(tryCatch(f(value), error = identity), class),
               logical(1))
  expect(all(ok), sprintf("%s: %s does not raise %s", label, deparse(values[!ok][1L]), class))
}

# The field sizes "r x c" of a choice vector as a two-column matrix.
choice_dims <- function(choices) {
  dims <- do.call(rbind, strsplit(choices, " x ", fixed = TRUE))
  storage.mode(dims) <- "integer"
  dims
}

with_dimension_test_rng <- function(code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  force(code)
}

# ---- which stackings a page offers -------------------------------------------

test_that("a 'Replicate layout' is offered only where it has a layout option", {
  base <- c("Vertical" = "vertical", "Horizontal" = "horizontal")
  panel <- c(base, "Grid panel" = "grid_panel")
  # A grid panel has rows and columns of replicates, more than one of each:
  # two, three, five or seven replicates cannot lie so, and have no grid panel
  cases <- list()
  for (reps in 2:9) {
    offered <- if (reps %in% c(4, 6, 8, 9)) panel else base
    cases[[paste("RCBD", reps)]] <- list(RCBD(t = 4, reps = reps, seed = 1), offered)
    cases[[paste("split plot", reps)]] <- list(split_plot(wp = 2, sp = 2, reps = reps, seed = 1), offered)
    cases[[paste("squares", reps)]] <- list(latin_square(t = 3, reps = reps, seed = 1), offered)
  }
  for (reps in 2:6) {
    cases[[paste("alpha", reps)]] <- list(alpha_lattice(t = 12, k = 3, reps = reps, seed = 1),
                                          if (reps %in% c(4, 6)) panel else base)
  }
  # A row-column design keeps the two stackings of its numbered layouts
  cases[["row-column"]] <- list(row_column(t = 12, nrows = 3, reps = 4, seed = 1, iterations = 10), base)
  # No replicates to arrange: one list of fields, and no stacking to choose
  without <- list(CRD = CRD(t = 6, reps = 4, seed = 1),
                  FD = full_factorial(setfactors = c(2, 3), reps = 4, type = 1, seed = 1),
                  SPD = split_plot(wp = 3, sp = 2, reps = 4, type = 1, seed = 1),
                  SSPD = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 3, type = 1, seed = 1),
                  LSD = latin_square(t = 5, reps = 1, seed = 1),
                  # A design built with dimensions has its one grid
                  stored = RCBD(t = 4, reps = 4, seed = 1, nrows = 4))
  for (name in names(without)) cases[[name]] <- list(without[[name]], base[1L])
  got <- lapply(cases, function(case) layout_stacking_choices(case[[1L]]))
  expect_identical(got, lapply(cases, `[[`, 2L))
  # every offered stacking of a lattice has options
  empty <- unlist(lapply(names(cases)[startsWith(names(cases), "alpha")], function(name) {
    stacked <- layout_stacking_choices(cases[[name]][[1L]])
    sizes <- vapply(stacked, function(s) {
      length(layout_choice_labels(cases[[name]][[1L]], stacked = s, include_grids = TRUE))
    }, integer(1))
    if (any(sizes == 0L)) paste(name, stacked[sizes == 0L])
  }))
  expect_identical(empty, NULL)
})

test_that("a page without a grid panel for its design offers none", {
  design <- RCBD(t = 6, reps = 3, seed = 22)
  spec <- design_app_spec("RCBD")$workflow
  ids <- spec$layout$ids
  shiny::testServer(function(input, output, session) {
    view <- app_classic_layout(input, output, session, design = function() design,
                               planter = function() "serpentine", spec = spec)
  }, {
    html <- as.character(output[[spec$layout$output]]$html)
    stacking <- panel_select(html, session$ns(ids[["stacked"]]))
    expect_identical(unname(stacking$options), c("vertical", "horizontal"))
  })
  expect_length(layout_choice_labels(design, stacked = "grid_panel", include_grids = TRUE), 0L)
})

# ---- the layout panel --------------------------------------------------------

test_that("the layout panel shows its labelled controls and options on every classic page", {
  skip_if_not_installed("shiny")
  set.seed(18)
  before <- .Random.seed
  x <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 1001), seed = 7)
  for (module in names(fieldhub_classic_workflows())) {
    spec <- classic_workflow_spec(module)
    html <- as.character(app_classic_layout_panel(shiny::NS("panel"), spec, x,
                                                  locations = c(2, 4), planter = "cartesian"))
    stacked <- "stacked" %in% names(spec$layout$ids)
    shown <- c(vapply(c("Type of plot", "Entries", "Plots", "Heatmap", "Field dimensions",
                        "Layout options"), grepl, logical(1), html, fixed = TRUE),
               # Four blocks lie in a panel of two by two on every page with a "Replicate layout"
               grid_panel = grepl('<option value="grid_panel">Grid panel</option>', html, fixed = TRUE) == stacked,
               replicate_layout = grepl("Replicate layout", html, fixed = TRUE) == stacked,
               locations = !"location" %in% names(spec$layout$ids) ||
                 grepl('<option value="4">4</option>', html, fixed = TRUE))
    expect(all(shown), sprintf("%s: %s", module, names(shown)[!shown][1L]))
  }
  html <- as.character(app_classic_layout_panel(shiny::NS("panel"), classic_workflow_spec("RCBD"), x))
  expect_true(grepl('<option value="2">2</option>', html, fixed = TRUE))
  expect_error(app_classic_layout_panel(identity, classic_workflow_spec("RCBD"), NULL),
               class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

test_that("each menu holds the numbered layouts and the field grids of its own stacking", {
  design <- split_plot(wp = 3, sp = 2, reps = 4, seed = 1)
  # A grid is named by its size, as a numbered layout is; its value carries
  # the stacking of its menu. The numbered layouts come first, then the grids
  expect_identical(
    lapply(c(vertical = "vertical", horizontal = "horizontal", grid_panel = "grid_panel"),
           function(stacked) layout_choice_labels(design, stacked = stacked, include_grids = TRUE)),
    list(vertical = c("4 \u00d7 6" = "2", "8 \u00d7 3" = "1", "12 \u00d7 2" = "grid:vertical:12:2",
                      "24 \u00d7 1" = "grid:vertical:24:1"),
         horizontal = c("3 \u00d7 8" = "1", "6 \u00d7 4" = "grid:horizontal:6:4",
                        "2 \u00d7 12" = "grid:horizontal:2:12", "1 \u00d7 24" = "grid:horizontal:1:24"),
         # Every option of the grid panel says how its replicates lie
         grid_panel = c("4 \u00d7 6 \u00b7 replicates 2 \u00d7 2" = "1",
                        "6 \u00d7 4 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:6:4",
                        "2 \u00d7 12 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:2:12",
                        "12 \u00d7 2 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:12:2")))
  spec <- design_app_spec("SPD")$workflow
  html <- as.character(app_classic_layout_panel(identity, spec, design))
  expect_identical(panel_select(html, spec$layout$ids[["stacked"]]),
                   list(label = "Replicate layout",
                        options = c("Vertical" = "vertical", "Horizontal" = "horizontal",
                                    "Grid panel" = "grid_panel"),
                        hidden = FALSE))
  # The page opens on the vertical menu, on its first, the most square field
  fields <- panel_select(html, spec$layout$ids[["layout"]])
  expect_identical(fields[c("label", "options")],
                   list(label = "Field dimensions", options = layout_choice_labels(design, include_grids = TRUE)))
  first <- layout_choice_labels(design, include_grids = TRUE)[1L]
  expect_true(grepl(paste0('<option value="', first, '" selected>', names(first), "</option>"),
                    html, fixed = TRUE))
  expect_false(grepl("optgroup|Grid [0-9]", html))
  # Three Latin squares stack as 9 x 3 and 3 x 9, the numbered layouts: they
  # have no other field, and no grid panel
  squares <- latin_square(t = 3, reps = 3, seed = 1)
  expect_identical(panel_select(as.character(app_classic_layout_panel(
    identity, design_app_spec("LSD")$workflow, squares)), "stackedLSD")$options,
    c("Vertical" = "vertical", "Horizontal" = "horizontal"))
})

test_that("the grid panel lists only fields with rows and columns of replicates", {
  rcbd <- RCBD(t = 6, reps = 4, seed = 1)
  expect_identical(layout_choice_labels(rcbd, stacked = "grid_panel", include_grids = TRUE),
                   c("4 \u00d7 6 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:4:6",
                     "6 \u00d7 4 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:6:4",
                     "2 \u00d7 12 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:2:12",
                     "12 \u00d7 2 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:12:2"))
  # The one-row field has its blocks side by side: it is a horizontal layout
  expect_identical(layout_choice_labels(rcbd, stacked = "horizontal",
                                        include_grids = TRUE)[["1 \u00d7 24"]], "grid:horizontal:1:24")
  # Regression test: the grid panel of three blocks of 24 once listed "1 x 72
  # (replicates stacked horizontally)", a field that field_layout() refuses
  # as a grid panel, and the horizontal menu lacked it. Three blocks cannot
  # lie in a panel: they have no grid panel, and the field is a horizontal one.
  three <- RCBD(t = 24, reps = 3, seed = 1)
  expect_length(layout_choice_labels(three, stacked = "grid_panel", include_grids = TRUE), 0L)
  horizontal <- layout_choice_labels(three, stacked = "horizontal", include_grids = TRUE)
  expect_identical(horizontal[["1 \u00d7 72"]], "grid:horizontal:1:72")
  expect_true("72 \u00d7 1" %in% names(layout_choice_labels(three, stacked = "vertical",
                                                             include_grids = TRUE)))
  side_by_side <- app_selected_layout_view(three, horizontal[["1 \u00d7 72"]], "serpentine", 1,
                                           "horizontal")$allSitesFieldbook
  expect_identical(replicates_lie(side_by_side), "horizontal")
  expect_equal(as.vector(tapply(side_by_side$COLUMN, side_by_side$REP, max)), c(24, 48, 72))
  expect_error(field_layout(three, nrows = 1, ncols = 72, stacked = "grid_panel"),
               "more than one of each", class = "fieldhub_input_error")
  # Of the panels of one size, the one field_layout() builds: its rows and
  # columns of replicates name it
  panel <- layout_choice_labels(RCBD(t = 6, reps = 12, seed = 1), stacked = "grid_panel", include_grids = TRUE)
  expect_identical(panel[grepl("^6 \u00d7 12 ", names(panel))],
                   c("6 \u00d7 12 \u00b7 replicates 3 \u00d7 4" = "grid:grid_panel:6:12"))
  # A numbered grid panel says how its replicates lie too, and is not
  # repeated as a grid
  lattice <- alpha_lattice(t = 12, k = 3, reps = 4, seed = 36)
  numbered <- layout_choice_labels(lattice, stacked = "grid_panel")
  panel <- layout_choice_labels(lattice, stacked = "grid_panel", include_grids = TRUE)
  expect_gt(length(numbered), 0L)
  expect_identical(panel[names(numbered)], stats::setNames(as.character(numbered), names(numbered)))
  expect_identical(panel[["2 \u00d7 24 \u00b7 replicates 2 \u00d7 2"]], "grid:grid_panel:2:24")
  expect_true(all(grepl(" \u00b7 replicates [0-9]+ \u00d7 [0-9]+$", names(panel))))
  # Its stacks are in the menus of their stackings, by size alone
  expect_identical(
    c(layout_choice_labels(lattice, stacked = "vertical", include_grids = TRUE)[["4 \u00d7 12"]],
      layout_choice_labels(lattice, stacked = "horizontal", include_grids = TRUE)[["12 \u00d7 4"]]),
    c("grid:vertical:4:12", "grid:horizontal:12:4"))
})

test_that("a menu opens on a numbered layout and lists the grids after the numbered ones", {
  # Regression test: the grids were once sorted in with the numbered layouts,
  # so the horizontal menu of this split plot opened on the 6 x 4 grid, not
  # on the 3 x 8 field of layout 1 as it always had
  split <- split_plot(wp = 3, sp = 2, reps = 4, seed = 1)
  horizontal <- layout_choice_labels(split, stacked = "horizontal", include_grids = TRUE)
  expect_identical(c(app_first_layout_choice(horizontal), names(horizontal)[1L]), c("1", "3 \u00d7 8"))
  designs <- list(split = split, RCBD = RCBD(t = 6, reps = 4, seed = 1),
                  LSD = latin_square(t = 4, reps = 4, seed = 7),
                  IBD = incomplete_blocks(t = 16, k = 4, reps = 4, seed = 1),
                  alpha = alpha_lattice(t = 12, k = 3, reps = 4, seed = 36), CRD = CRD(t = 6, reps = 4, seed = 1))
  for (name in names(designs)) {
    design <- designs[[name]]
    for (stacked in layout_stacking_choices(design)) {
      numbered <- layout_choice_labels(design, stacked = stacked)
      choices <- layout_choice_labels(design, stacked = stacked, include_grids = TRUE)
      grid <- grepl("^grid:", choices)
      sizes <- vapply(strsplit(sub("^grid:[a-z_]+:", "", choices[grid]), ":"), as.numeric, numeric(2))
      # The numbered layouts in their own order, then the grids, the most
      # square first
      ok <- c(numbered_first = identical(unname(choices[!grid]), as.character(numbered)) && !is.unsorted(grid),
              squarest_grids = !length(sizes) ||
                !is.unsorted(order(abs(sizes[1L, ] - sizes[2L, ]), sizes[1L, ])))
      expect(all(ok), sprintf("%s %s: %s", name, stacked, names(ok)[!ok][1L]))
    }
    # The page opens on the layout it opens on without the grids
    expect_identical(app_first_layout_choice(layout_choice_labels(design, include_grids = TRUE)),
                     as.character(layout_choice_labels(design)[[1L]]), info = name)
  }
})

test_that("a grid of the size of a numbered layout is listed when its blocks or whole plots lie another way", {
  # How every unit of every replicate lies: the menu may say "in rows" only
  # of a field where each of them is a full row of plots
  lies <- function(book, unit) {
    shapes <- unique(vapply(split(book, paste(book$REP, book[[unit]])), function(one) {
      rows <- diff(range(one$ROW)) + 1
      columns <- diff(range(one$COLUMN)) + 1
      if (rows * columns != nrow(one)) "path" else if (rows == 1) "rows" else
        if (columns == 1) "columns" else paste(rows, "x", columns)
    }, character(1)))
    if (length(shapes) == 1L) shapes else "path"
  }
  drawn <- function(design, value, stacked) {
    app_selected_layout_view(design, value, "serpentine", 1, stacked)$allSitesFieldbook
  }
  # The 8 x 8 panel of four replicates of 4 x 4: layout 1 has the blocks in
  # columns, field_layout(nrows = 8, ncols = 8) in rows. Both are listed and
  # say so, after how the replicates lie
  blocks <- incomplete_blocks(t = 16, k = 4, reps = 4, seed = 1)
  panel <- layout_choice_labels(blocks, stacked = "grid_panel", include_grids = TRUE)
  expect_identical(panel[1:2],
                   c("8 \u00d7 8 \u00b7 replicates 2 \u00d7 2 \u00b7 blocks in columns" = "1",
                     "8 \u00d7 8 \u00b7 replicates 2 \u00d7 2 \u00b7 blocks in rows" = "grid:grid_panel:8:8"))
  expect_identical(anyDuplicated(names(panel)), 0L)
  expect_identical(drawn(blocks, panel[[2L]], "grid_panel"),
                   field_layout(blocks, nrows = 8, ncols = 8, stacked = "grid_panel"))
  # Under a stack the two are told apart by that alone
  split <- split_plot(wp = 4, sp = 2, reps = 2, seed = 1)
  vertical <- layout_choice_labels(split, stacked = "vertical", include_grids = TRUE)
  expect_identical(vertical[c("8 \u00d7 2 \u00b7 whole plots in columns", "8 \u00d7 2 \u00b7 whole plots in rows")],
                   c("8 \u00d7 2 \u00b7 whole plots in columns" = "3",
                     "8 \u00d7 2 \u00b7 whole plots in rows" = "grid:vertical:8:2"))
  # Regression test: the name read the first whole plot alone, and layout 3
  # was "whole plots in rows" although its whole plots of 6 wrap from a row
  # of 9 into the next
  wrapped <- split_split_plot(wp = 3, sp = 2, ssp = 3, reps = 2, seed = 13)
  menu <- layout_choice_labels(wrapped, stacked = "vertical", include_grids = TRUE)
  expect_identical(menu[c("4 \u00d7 9 \u00b7 whole plots along the path", "4 \u00d7 9 \u00b7 whole plots 2 \u00d7 3")],
                   c("4 \u00d7 9 \u00b7 whole plots along the path" = "3",
                     "4 \u00d7 9 \u00b7 whole plots 2 \u00d7 3" = "grid:vertical:4:9"))
  # each of these options draws its units as its name says
  shapes <- c(lies(drawn(blocks, panel[[1L]], "grid_panel"), "IBLOCK"),
              lies(drawn(blocks, panel[[2L]], "grid_panel"), "IBLOCK"),
              lies(drawn(split, "3", "vertical"), "WHOLE_PLOT"),
              lies(drawn(split, "grid:vertical:8:2", "vertical"), "WHOLE_PLOT"),
              lies(drawn(wrapped, "3", "vertical"), "WHOLE_PLOT"),
              lies(drawn(wrapped, "grid:vertical:4:9", "vertical"), "WHOLE_PLOT"))
  expect_identical(shapes, c("columns", "rows", "columns", "rows", "path", "2 x 3"))
  # A grid that is the field of a numbered layout, or differs from it only in
  # the path of its plots, is listed once: as the numbered layout
  rcbd <- layout_choice_labels(RCBD(t = 6, reps = 4, seed = 1), stacked = "vertical", include_grids = TRUE)
  expect_identical(names(rcbd), c("4 \u00d7 6", "8 \u00d7 3", "12 \u00d7 2", "24 \u00d7 1"))
  expect_false(any(grepl("^grid:", rcbd)))
  expect_identical(sum(grepl("^16 \u00d7 4", names(layout_choice_labels(
    blocks, stacked = "vertical", include_grids = TRUE)))), 3L)
})

test_that("a layout menu names its options by size and by what tells them apart", {
  # The same layout under two numbers is listed once; fields of one size
  # differ in how their whole plots or their blocks lie
  expect_identical(
    list(names(layout_choice_labels(RCBD(t = 7, reps = 3, seed = 1), stacked = "horizontal")),
         names(layout_choice_labels(split_plot(wp = 4, sp = 2, reps = 2, seed = 1))),
         names(layout_choice_labels(incomplete_blocks(t = 16, k = 4, reps = 4, seed = 1)))),
    list(c("7 \u00d7 3", "1 \u00d7 21"),
         c("4 \u00d7 4 \u00b7 whole plots in columns", "4 \u00d7 4 \u00b7 whole plots in rows", "8 \u00d7 2"),
         c("8 \u00d7 8", "16 \u00d7 4 \u00b7 blocks 2 \u00d7 2",
           "16 \u00d7 4 \u00b7 blocks in columns", "16 \u00d7 4 \u00b7 blocks in rows")))
})

test_that("the layout options are named by the shape of their field, the most square first", {
  crd <- CRD(t = 15, reps = 4, seed = 5)
  expect_identical(layout_choice_labels(crd), c("4 \u00d7 15" = 1L, "15 \u00d7 4" = 2L))
  expect_identical(checked_layout_view(crd, layout = 2, planter = "serpentine", location = 1,
                                       stacked = "vertical")$out_layout$labels$title,
                   "Completely Randomized Design 15 \u00d7 4")
  # the most square field first, the one with fewer rows of two alike
  rcbd <- RCBD(t = 18, reps = 3, seed = 4)
  for (stacked in c("vertical", "horizontal")) {
    labels <- layout_choice_labels(rcbd, stacked = stacked)
    shapes <- sub(" \u00b7 .*$", "", names(labels))
    sizes <- vapply(strsplit(shapes, " \u00d7 ", fixed = TRUE), as.numeric, numeric(2))
    key <- abs(sizes[1L, ] - sizes[2L, ])
    # the title of each drawn option is the shape of its name
    titles <- vapply(labels, function(value) {
      checked_layout_view(rcbd, layout = value, planter = "serpentine", location = 1,
                          stacked = stacked)$out_layout$labels$title
    }, "")
    ok <- c(squarest_first = !is.unsorted(key) && all(diff(sizes[1L, ])[diff(key) == 0] >= 0),
            unique = !anyDuplicated(names(labels)),
            titles = all(endsWith(titles, shapes)))
    expect(all(ok), sprintf("RCBD %s: %s", stacked, names(ok)[!ok][1L]))
  }
  # a page opens on the first
  panel <- as.character(app_classic_layout_panel(shiny::NS("x"), classic_workflow_spec("RCBD"), rcbd))
  first <- layout_choice_labels(rcbd)[1L]
  expect_match(panel, sprintf('<option value="%s" selected>%s</option>', first, names(first)), fixed = TRUE)
  # two options of one shape say how their incomplete blocks lie in the field
  alpha <- alpha_lattice(t = 36, k = 6, reps = 3, seed = 1)
  expect_identical(names(layout_choice_labels(alpha)),
                   c("9 \u00d7 12", "12 \u00d7 9", "6 \u00d7 18", "18 \u00d7 6 \u00b7 blocks 3 \u00d7 2",
                     "18 \u00d7 6 \u00b7 blocks in columns", "18 \u00d7 6 \u00b7 blocks in rows"))
  expect_false(anyDuplicated(names(layout_choice_labels(alpha, stacked = "horizontal"))) > 0)
  lattice <- square_lattice(t = 49, k = 7, reps = 3, seed = 1)
  expect_identical(
    list(names(layout_choice_labels(lattice)), names(layout_choice_labels(lattice, stacked = "horizontal")),
         layout_choice_labels(lattice, stacked = "grid_panel")),
    list(c("21 \u00d7 7 \u00b7 blocks in columns", "21 \u00d7 7 \u00b7 blocks in rows"),
         c("7 \u00d7 21 \u00b7 blocks in rows", "7 \u00d7 21 \u00b7 blocks in columns"), integer()))
})

test_that("every option of a 'Replicate layout' menu draws its replicates as its menu says", {
  skip_on_cran() # draws every option of every menu: about 20 s
  # Regression test: the field grids were once the same under every
  # stacking, so the vertical menu of an RCBD offered a 1 x 24 field (the
  # replicates side by side); then every grid was under the grid panel,
  # stacks of replicates too. A menu now lists the grids of its stacking.
  designs <- list(
    RCBD(t = 6, reps = 4, seed = 1),
    RCBD(t = 24, reps = 3, seed = 1),
    RCBD(t = 6, reps = 6, seed = 1),
    RCBD(t = 6, reps = 12, seed = 1),
    full_factorial(setfactors = c(2, 3), reps = 4, type = 2, seed = 1),
    latin_square(t = 4, reps = 4, seed = 7),
    split_plot(wp = 3, sp = 2, reps = 4, seed = 1),
    split_plot(wp = 3, sp = 2, reps = 3, seed = 11),
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 4, seed = 1),
    strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 1),
    incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1),
    incomplete_blocks(t = 12, k = 4, reps = 3, seed = 17),
    alpha_lattice(t = 12, k = 3, reps = 4, seed = 36),
    square_lattice(t = 16, k = 4, reps = 4, seed = 1),
    rectangular_lattice(t = 12, k = 3, reps = 4, seed = 1)
  )
  for (design in designs) {
    unit <- if (inherits(design, "fieldhub_latin_square")) "SQUARE" else "REP"
    problems <- character()
    for (planter in c("serpentine", "cartesian")) {
      for (stacked in layout_stacking_choices(design, planter = planter)) {
        choices <- layout_choice_labels(design, planter = planter, stacked = stacked,
                                        include_grids = TRUE)
        info <- paste(class(design)[1L], planter, stacked)
        names_ok <- c(
          options = length(choices) > 0L,
          unique = !anyDuplicated(names(choices)),
          plain_names = !any(grepl("^Grid|\\([0-9]+\\)$|stacked", names(choices))),
          # A grid carries the stacking of its menu; only a panel names its replicates
          grid_values = all(grepl(paste0("^grid:", stacked, ":"), choices[grepl("^grid:", choices)])),
          replicate_names = identical(grepl(" \u00b7 replicates [0-9]+ \u00d7 [0-9]+", names(choices)),
                                      rep(stacked == "grid_panel", length(choices))))
        problems <- c(problems, paste(info, names(names_ok)[!names_ok]))
        for (i in seq_along(choices)) {
          book <- app_selected_layout_view(design, choices[[i]], planter, 1, stacked)$allSitesFieldbook
          rows_of_replicates <- regmatches(names(choices)[i],
                                           regexec("replicates ([0-9]+) \u00d7", names(choices)[i]))[[1L]]
          drawn_ok <- c(
            lies = identical(replicates_lie(book, unit), stacked),
            size = identical(paste(max(book$ROW), "\u00d7", max(book$COLUMN)),
                             sub(" \u00b7 .*$", "", names(choices)[i])),
            replicate_rows = !length(rows_of_replicates) ||
              identical(length(unique(tapply(book$ROW, book[[unit]], min))),
                        as.integer(rows_of_replicates[2L])))
          problems <- c(problems, paste(info, names(choices)[i], names(drawn_ok)[!drawn_ok]))
        }
      }
    }
    problems <- problems[!grepl(" $", problems)]
    expect(!length(problems), paste(utils::head(problems, 1L), "fails"))
  }
})

test_that("designs without replicates to arrange have one list of fields", {
  designs <- list(CRD = CRD(t = 6, reps = 4, seed = 1),
                  FD = full_factorial(setfactors = c(2, 3), reps = 4, type = 1, seed = 1),
                  SPD = split_plot(wp = 3, sp = 2, reps = 4, type = 1, seed = 1),
                  SSPD = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 3, type = 1, seed = 1),
                  LSD = latin_square(t = 5, reps = 1, seed = 1))
  for (module in names(designs)) {
    design <- designs[[module]]
    spec <- design_app_spec(module)$workflow
    html <- as.character(app_classic_layout_panel(identity, spec, design))
    fields <- panel_select(html, spec$layout$ids[["layout"]])
    ok <- c(label = identical(fields$label, "Field dimensions"),
            options = identical(fields$options, layout_choice_labels(design, include_grids = TRUE)),
            # Sizes alone: nothing lies one way or another, and no size comes twice
            sizes_only = all(grepl("^[0-9]+ \u00d7 [0-9]+$", names(fields$options))),
            unique = !anyDuplicated(names(fields$options)),
            # No "Replicate layout" on the page; its input, where the page has one, is not shown
            no_stacking = if ("stacked" %in% names(spec$layout$ids)) {
              panel_select(html, spec$layout$ids[["stacked"]])$hidden
            } else {
              !grepl("Replicate layout", html, fixed = TRUE)
            })
    expect(all(ok), sprintf("%s: %s", module, names(ok)[!ok][1L]))
  }
  # The numbered layouts come first (a page opens on the first), then the
  # field grids of the other sizes
  crd <- layout_choice_labels(CRD(t = 15, reps = 4, seed = 1), include_grids = TRUE)
  expect_identical(names(crd), paste(c(4, 15, 6, 10, 5, 12, 3, 20, 2, 30, 1, 60), "\u00d7",
                                     c(15, 4, 10, 6, 12, 5, 20, 3, 30, 2, 60, 1)))
  expect_identical(unname(crd[1:3]), c("1", "2", "grid:none:6:10"))
  # A grid of a design without replicates is the field of its dimensions
  for (module in c("CRD", "FD", "SPD")) {
    design <- designs[[module]]
    menu <- layout_choice_labels(design, include_grids = TRUE)
    grid <- menu[grepl("^grid:", menu)][[1L]]
    dims <- as.numeric(strsplit(grid, ":", fixed = TRUE)[[1L]][3:4])
    expect_identical(app_selected_layout_view(design, grid, "serpentine", 1, "vertical")$allSitesFieldbook,
                     field_layout(design, nrows = dims[1L], ncols = dims[2L]), info = module)
  }
  # A design built with dimensions has its one grid
  stored <- RCBD(t = 6, reps = 6, nrows = 4, ncols = 9, seed = 1)
  expect_identical(layout_choice_labels(stored, include_grids = TRUE), c("4 \u00d7 9" = "1"))
})

# ---- the layout view a page draws --------------------------------------------

test_that("checked layout views reject unavailable choices instead of returning NULL", {
  design <- RCBD(t = 6, reps = 3, seed = 7)
  expect_all_classed(function(v) checked_layout_view(design, layout = v),
                     list(NULL, NA_real_, NaN, Inf, -1, 0, 1.5, "1", c(1, 2), 100), "layout")
  expect_calls_classed(list(
    grid_panel = function() checked_layout_view(design, stacked = "grid_panel"),
    location = function() checked_layout_view(design, location = 2),
    location_na = function() checked_layout_view(design, location = NA_real_),
    planter = function() checked_layout_view(design, planter = "other"),
    design = function() checked_layout_view(NULL)))
})

test_that("checked views retain complete layout field books and reconstruction settings", {
  design <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 1001), seed = 8)
  set.seed(416)
  before <- .Random.seed
  for (stacked in c("vertical", "horizontal")) {
    view <- checked_layout_view(design, planter = "cartesian", stacked = stacked, location = 2)
    expect_identical(view$allSitesFieldbook, field_layout(design, planter = "cartesian", stacked = stacked),
                     info = stacked)
    expect_identical(view$layout_metadata,
                     list(parameters = list(layout = 1, planter = "cartesian", stacked = stacked), selected = 2))
    expect_true(all(as.character(view$fieldBookXY$LOCATION) == "loc2"))
    expect_silent({
      ggplot2::ggplot_build(view$out_layout)
      ggplot2::ggplot_build(view$out_layoutPlots)
    })
  }
  expect_identical(.Random.seed, before)
})

test_that("objects saved by the previous release get the same layouts, leaving global state alone", {
  design <- RCBD(t = 6, reps = 2, seed = 25)
  legacy <- design
  class(legacy) <- "FielDHub"
  legacy$metadata <- NULL
  set.seed(31)
  before <- .Random.seed
  settings <- options()
  expect_identical(field_layout(legacy), field_layout(design))
  expect_identical(layout_choice_labels(legacy, include_grids = TRUE),
                   layout_choice_labels(design, include_grids = TRUE))
  expect_true(identical(.Random.seed, before) && identical(options(), settings))
})

test_that("selecting a grid in Shiny sends one field layout to plots and downloads", {
  design <- RCBD(t = 6, reps = 6, l = 2, plotNumber = c(101, 1001), seed = 1)
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() design, function() "cartesian",
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  shiny::testServer(server, {
    session$setInputs(stacked = "vertical", layout = "1", location = "2")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, layout = 1, planter = "cartesian"))
    session$setInputs(layout = "grid:vertical:4:9")
    # No stack of six blocks fits 4 x 9: a grid that no menu offers is a message
    expect_error(view(), class = "shiny.silent.error")
    session$setInputs(stacked = "grid_panel")
    # A new stacking opens on its first option, the most square field
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, stacked = "grid_panel", nrows = 6, ncols = 6,
                                  planter = "cartesian"))
    session$setInputs(layout = "grid:grid_panel:4:9")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, stacked = "grid_panel", nrows = 4, ncols = 9,
                                  planter = "cartesian"))
    expect_true(all(as.character(view()$fieldBookXY$LOCATION) == "loc2"))
    expect_identical(view()$layout_metadata,
                     list(parameters = list(planter = "cartesian", nrows = 4L, ncols = 9L,
                                            stacked = "grid_panel"),
                          selected = 2))
    expect_silent(ggplot2::ggplot_build(view()$out_layout))
    session$setInputs(stacked = "horizontal")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, layout = as.numeric(layout_choice_labels(
                       design, planter = "cartesian", stacked = "horizontal")[1L]),
                       planter = "cartesian", stacked = "horizontal"))
    # The one-row field is a grid of the horizontal menu: its blocks side by
    # side, the stacking that its value carries
    session$setInputs(layout = "grid:horizontal:1:36")
    expect_identical(view()$allSitesFieldbook,
                     field_layout(design, stacked = "horizontal", nrows = 1, planter = "cartesian"))
    expect_identical(view()$layout_metadata$parameters$stacked, "horizontal")
  })
})

test_that("the layout menu of a real page follows its 'Replicate layout' to the drawn field", {
  skip_on_cran() # draws every option of every menu: about 20 s
  # The module every classic page mounts: the panel it renders and the view
  # it hands to the plot, the field book and the downloads
  pages <- list(
    RCBD = RCBD(t = 6, reps = 4, seed = 1),
    FD = full_factorial(setfactors = c(2, 3), reps = 4, type = 2, seed = 1),
    LSD = latin_square(t = 4, reps = 4, seed = 7),
    SPD = split_plot(wp = 3, sp = 2, reps = 4, seed = 1),
    SSPD = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 4, seed = 1),
    STRIPD = strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 1),
    IBD = incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1),
    Alpha_Lattice = alpha_lattice(t = 12, k = 3, reps = 4, seed = 36)
  )
  for (module in names(pages)) {
    design <- pages[[module]]
    spec <- design_app_spec(module)$workflow
    ids <- spec$layout$ids
    unit <- if (module == "LSD") "SQUARE" else "REP"
    server <- function(input, output, session) {
      view <- app_classic_layout(input, output, session, design = function() design,
                                 planter = function() "serpentine", spec = spec)
    }
    shiny::testServer(server, {
      html <- as.character(output[[spec$layout$output]]$html)
      # The page opens on the vertical menu
      expect_identical(
        list(unname(panel_select(html, session$ns(ids[["stacked"]]))$options),
             unname(panel_select(html, session$ns(ids[["layout"]]))$options)),
        list(c("vertical", "horizontal", "grid_panel"),
             unname(layout_choice_labels(design, include_grids = TRUE))), info = module)
      set <- function(id, value) do.call(session$setInputs, stats::setNames(list(value), ids[[id]]))
      set("stacked", "vertical")
      set("layout", "1")
      if ("location" %in% names(ids)) set("location", "1")
      for (stacked in c("horizontal", "grid_panel", "vertical")) {
        set("stacked", stacked)
        choices <- layout_choice_labels(design, stacked = stacked, include_grids = TRUE)
        # The stacking opens on its first option, whatever the select still
        # holds; the browser then sets the select to it
        problems <- if (!identical(replicates_lie(view()$allSitesFieldbook, unit), stacked)) "opening view"
        for (i in seq_along(choices)) {
          set("layout", paste0(choices[[i]], ""))
          book <- view()$allSitesFieldbook
          # Every option, numbered or grid, lies as its menu says
          ok <- identical(replicates_lie(book, unit), stacked)
          if (grepl("^grid:", choices[[i]])) {
            grid <- strsplit(choices[[i]], ":", fixed = TRUE)[[1L]]
            ok <- ok && identical(grid[2L], stacked) &&
              identical(book, field_layout(design, stacked = grid[2L], nrows = as.numeric(grid[3L]),
                                           ncols = as.numeric(grid[4L]))) &&
              identical(view()$layout_metadata$parameters$stacked, grid[2L])
          }
          ok <- ok && !inherits(tryCatch(ggplot2::ggplot_build(view()$out_layout), error = identity), "error")
          if (!ok) problems <- c(problems, names(choices)[i])
        }
        expect(!length(problems), sprintf("%s %s: option %s", module, stacked, problems[1L]))
      }
    })
  }
})

test_that("field grids of every classic design reach the Shiny layout view", {
  designs <- list(
    CRD = CRD(t = 4, reps = 3, seed = 1),
    LSD = latin_square(t = 3, reps = 4, seed = 1),
    FD = full_factorial(setfactors = c(2, 3), reps = 2, l = 2, type = 2, seed = 9),
    SPD = split_plot(wp = 3, sp = 2, reps = 2, seed = 11),
    SSPD = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 13),
    STRIPD = strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 15),
    IBD = incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17),
    Alpha = alpha_lattice(t = 12, k = 4, reps = 2, seed = 19),
    Square = square_lattice(t = 9, k = 3, reps = 2, seed = 20),
    Rectangular = rectangular_lattice(t = 12, k = 3, reps = 2, seed = 21)
  )
  current <- NULL
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() current, function() "serpentine",
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  for (name in names(designs)) {
    current <- designs[[name]]
    # The first field grid the page offers: in its one list of fields, or
    # in the menu of its last stacking, the one that its value carries
    last <- utils::tail(layout_stacking_choices(current), 1L)[[1L]]
    menu <- layout_choice_labels(current, include_grids = TRUE, stacked = last)
    grid <- menu[grepl("^grid:", menu)][[1L]]
    stacking <- strsplit(grid, ":", fixed = TRUE)[[1L]][2L]
    dims <- as.numeric(strsplit(grid, ":", fixed = TRUE)[[1L]][3:4])
    shiny::testServer(server, {
      session$setInputs(stacked = "vertical", layout = "1", location = "1")
      first <- view()$allSitesFieldbook
      session$setInputs(stacked = last, layout = grid)
      book <- if (name == "CRD") {
        field_layout(current, nrows = dims[1L], ncols = dims[2L])
      } else {
        field_layout(current, stacked = stacking, nrows = dims[1L], ncols = dims[2L])
      }
      ok <- c(numbered = identical(first, field_layout(current)),
              grid = identical(view()$allSitesFieldbook, book),
              grid_size = identical(dim(table(view()$fieldBookXY$ROW, view()$fieldBookXY$COLUMN)),
                                    as.integer(dims)),
              draws = !inherits(tryCatch({
                ggplot2::ggplot_build(view()$out_layout)
                ggplot2::ggplot_build(view()$out_layoutPlots)
              }, error = identity), "error"))
      # A grid that does not fit this design is a message, not a crash
      session$setInputs(layout = "grid:vertical:7:7")
      ok[["unfit_is_message"]] <- inherits(tryCatch(view(), error = identity), "shiny.silent.error")
      session$setInputs(stacked = "vertical", layout = "1")
      ok[["back"]] <- identical(view()$allSitesFieldbook, field_layout(current))
      expect(all(ok), sprintf("%s: %s", name, names(ok)[!ok][1L]))
    })
  }
})

test_that("a design built on a field grid shows its grid under every Shiny selection", {
  design <- RCBD(t = 6, reps = 6, l = 2, plotNumber = c(101, 1001), seed = 1, nrows = 4)
  planter <- "serpentine"
  server <- function(input, output, session) {
    view <- app_layout_selection(input, session, function() design, function() planter,
                                 c(layout = "layout", stacked = "stacked", location = "location"))
  }
  for (planter in c("serpentine", "cartesian")) {
    shiny::testServer(server, {
      for (stacked in c("vertical", "horizontal")) {
        session$setInputs(stacked = stacked, layout = "1", location = "2")
        expect_identical(view()$allSitesFieldbook, design$fieldBook, info = paste(planter, stacked))
      }
      expect_silent(ggplot2::ggplot_build(view()$out_layout))
    })
  }
})

test_that("a design that failed is not also reported in a dialog", {
  problem <- tryCatch(fieldhub_abort("Too few entries."), error = identity)
  # outside a session a dialog is a message: there is none
  expect_silent(job <- app_accept_design_job(list(ok = FALSE, condition = problem, warnings = list()), identity))
  expect_false(job$ok)
})

# ---- the CRD page --------------------------------------------------------------

test_that("the CRD page has a summary of its own parameters and plain messages", {
  spec <- design_app_spec("CRD")
  expect_true(spec$summary && spec$summary_field)
  design <- CRD(t = 15, reps = 4, seed = 5, locationNames = "FARGO")
  book <- checked_layout_view(design, layout = 1, planter = "serpentine", location = 1,
                              stacked = "vertical")$fieldBookXY
  items <- app_summary_items(design$infoDesign, book, field = TRUE)
  expect_identical(lapply(items, `[[`, "value"),
                   list("15", "4", "60", "4 \u00d7 15", "FARGO"))
  expect_identical(vapply(items, `[[`, "", "label"),
                   c("Treatments", "Replicates", "Plots", "Field", "Location"))
  page <- as.character(app_design_summary(design, tabset = "tabs", book = book, field = TRUE))
  expect_match(page, "First 6 of 60 plots", fixed = TRUE)
  expect_match(page, "See all", fixed = TRUE)
  expect_false(grepl("Efficiency", page, fixed = TRUE))
  # without the layout in view, the parameters of the design alone
  expect_identical(vapply(app_summary_items(design$infoDesign, design$fieldBook), `[[`, "", "label"),
                   c("Treatments", "Replicates", "Location"))
  raw <- list(t = 15, reps = 0, planter = "serpentine", plot_start = 101, location_names = "FARGO", seed = NA)
  expect_error(read_design_controls(spec, raw, uploaded = FALSE), "Replicates must be 1 or more.",
               fixed = TRUE, class = "fieldhub_input_error")
  raw$reps <- 4; raw$t <- 0
  expect_error(read_design_controls(spec, raw, uploaded = FALSE), "Treatments must be 1 or more.",
               fixed = TRUE, class = "fieldhub_input_error")
})

# ---- the classic workflow: simulation and layout export ----------------------

test_that("classic workflow books preserve unsimulated data and simulation row alignment", {
  x <- CRD(t = 4, reps = 2, seed = 12)
  book <- x$fieldBook[rev(seq_len(nrow(x$fieldBook))), ]
  set.seed(811)
  before <- .Random.seed
  expect_identical(app_trait_simulation(x, book, seed = stop("unused seed")),
                   list(df = book, simulation = NULL))
  settings <- list(mean = 6, residual_sd = 0.4, trait = "YIELD")
  reference <- do.call(stats::simulate, c(list(object = x, seed = 14), settings))
  for (ordered in c(TRUE, FALSE)) {
    actual <- app_trait_simulation(x, book, settings, 14, order_by_id = ordered)
    expect_identical(actual$df$YIELD,
                     reference$field_book$YIELD[match(actual$df$ID, reference$field_book$ID)],
                     info = paste("ordered", ordered))
    expect_identical(reproduce_simulation(actual$simulation), actual$simulation)
  }
  expect_identical(.Random.seed, before)
  expect_calls_classed(lapply(list(NULL, list(), book[FALSE, ]),
                              function(bad) function() app_trait_simulation(x, bad)))
})

test_that("workflow layout export retains label and plot-number choices", {
  book <- data.frame(LOCATION = rep("A", 4), ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2),
                     PLOT = 101:104, ENTRY = 1:4, TREATMENT = letters[1:4])
  spec <- classic_workflow_spec("RCBD")
  treatments <- export_layout(book, 1, type_pref = "TREATMENT")
  expect_identical(
    list(classic_workflow_layout(book, 1, "1", spec), classic_workflow_layout(book, 1, 2, spec),
         classic_workflow_layout(book, 1, "3", spec),
         classic_workflow_layout(book, 1, 1, classic_workflow_spec("IBD"))),
    list(treatments, export_layout(book, 1, TRUE), treatments, export_layout(book, 1)))
  expect_all_classed(function(v) classic_workflow_layout(book, 1, v, spec),
                     list(NULL, NA, "bad", c(1, 2)), "plot type")
})

# ---- locations -----------------------------------------------------------------

test_that("layout downloads select the same location as plots and heatmaps", {
  design <- full_factorial(setfactors = c(2, 2), reps = 2, l = 2, plotNumber = c(1, 101),
                           locationNames = c("West", "East"), seed = 17)
  book <- field_layout(design)
  book$YIELD <- seq_len(nrow(book))
  for (site in 1:2) {
    plotted <- plot_layout(design, l = site)$fieldBookXY
    heatmap <- field_book_heatmap_data(book, "YIELD", selected = site, label_column = "TRT_COMB")
    exported <- export_layout(book, selected = site, plotOn = TRUE)$file
    expect_identical(c(as.character(exported[2L, 1L]), as.character(exported[2L, 1L])),
                     c(unique(as.character(plotted$LOCATION)), unique(as.character(heatmap$LOCATION))),
                     info = paste("site", site))
    expect_setequal(as.numeric(unlist(exported[-c(1L, 2L), -1L])), as.numeric(as.character(plotted$PLOT)))
  }
  book$LOCATION <- factor(book$LOCATION, levels = c("East", "West"))
  expect_identical(as.character(export_layout(book, 1)$file[2L, 1L]), "West")
})

test_that("location grids use named coordinates rather than positional slices", {
  book <- data.frame(LOCATION = rep(c("West", "East"), each = 4),
                     ROW = rep(c(1L, 1L, 2L, 2L), 2), COLUMN = rep(c(1L, 2L, 1L, 2L), 2),
                     EXPT = letters[1:8])
  expected <- list(matrix(c("c", "a", "d", "b"), 2), matrix(c("g", "e", "h", "f"), 2))
  expect_identical(field_book_location_grids(book, "EXPT", reverse_rows = TRUE), expected)
  altered <- book[c(3, 1, 4, 2, 7, 5, 8, 6), rev(names(book))]
  expect_identical(field_book_location_grids(altered, "EXPT", reverse_rows = TRUE), expected)
  expect_identical(field_book_location_grids(book, "EXPT")[[1]], expected[[1]][2:1, , drop = FALSE])
  different <- rbind(book, data.frame(LOCATION = "North", ROW = 1L, COLUMN = 1L, EXPT = "i"))
  expect_identical(field_book_location_grids(different, "EXPT", reverse_rows = TRUE)[[3]], matrix("i"))
  expect_calls_classed(list(
    missing_plot = function() field_book_location_grids(book[-1L, ], "EXPT"),
    reverse_na = function() field_book_location_grids(book, "EXPT", reverse_rows = NA)))
})

test_that("locations are listed in order of appearance and malformed ones are rejected", {
  values <- list(c("Z", "A", "Z"), factor(c("Z", "A", "Z")), c(10, 2, 1))
  expect_identical(lapply(values, function(v) field_book_locations(data.frame(LOCATION = v))),
                   list(c("Z", "A"), c("Z", "A"), c("10", "2", "1")))
  expect_all_classed(field_book_locations,
                     list(NULL, data.frame(LOCATION = character()), data.frame(OTHER = 1),
                          data.frame(LOCATION = NA_character_)), "book")
})

test_that("the location to view and the plant-rep counts follow the typed number of locations", {
  # The menus of a spatial page, from the value of its locations input
  spec <- design_app_spec("sparse_allocation")
  locations <- spec_control(spec, "location_view")
  copies <- spec_control(spec, "copies_per_entry")
  expect_identical(
    lapply(list(1, 3, 3L, "3"), function(l) locations$options(list(l = l), NULL)),
    list(list(choices = 1L, selected = 1L), list(choices = 1:3, selected = 1L),
         list(choices = 1:3, selected = 1L), list(choices = 1:3, selected = 1L)))
  # plant_reps picks a count out of the total, so one location offers none
  expect_identical(
    lapply(list(1, 3, "4"), function(l) copies$options(list(l = l), NULL)$choices),
    list(integer(0), 1:2, 1:3))
  # a cleared or malformed input is a message, not an ended session
  cleared <- list(NA, NA_real_, "", "abc", NULL, 0, -1, 2.5, c(2, 3))
  expect_all_classed(function(l) locations$options(list(l = l), NULL), cleared, "location view")
  expect_all_classed(function(l) copies$options(list(l = l), NULL), cleared, "copies per entry")
})

# ---- the RCBD size preview -----------------------------------------------------

test_that("the RCBD preview states the block and total size", {
  expect_identical(c(rcbd_size_preview(10, 3, 2, "2,3"), rcbd_size_preview(10, 3, 2, "2"),
                     # beyond the integer print range
                     rcbd_size_preview(10, 1e9, 2, "2")),
                   c("Block size: 15 plots. Total: 45 plots.", "Block size: 14 plots. Total: 42 plots.",
                     "Block size: 14 plots. Total: 14000000000 plots."))
  expect_error(rcbd_size_preview(10, 3, 2, "1e9"), class = "fieldhub_input_error")
  expect_warning(expect_error(rcbd_size_preview(10, 1e308, 2, "2"), "too large",
                              class = "fieldhub_input_error"), NA)
})

test_that("RCBD previews reject malformed counts before formatting", {
  bad <- list(NA_real_, Inf, 1.5, c(1, 2))
  expect_all_classed(function(v) rcbd_size_preview(v, 3, 2, "2"), bad, "treatments")
  expect_all_classed(function(v) rcbd_size_preview(10, v, 2, "2"), bad, "blocks")
  expect_error(rcbd_size_preview(10, 3, 2, "2,"), class = "fieldhub_input_error")
})

# ---- field sizes of the spatial pages -----------------------------------------

test_that("single diagonal choices are feasible and ordered by squareness", {
  for (planter in c("serpentine", "cartesian")) {
    choices <- diagonal_dimension_choices(lines = 105, checks = 1:4, planter = planter, minimum_extra = 0.11)
    expect_true(is.character(choices) && length(choices) > 0L, info = planter)
    dims <- choice_dims(choices)
    difference <- abs(dims[, 1] - dims[, 2])
    plots <- dims[, 1] * dims[, 2]
    feasible <- vapply(seq_len(nrow(dims)), function(i) {
      nrow(available_percent(n_rows = dims[i, 1], n_cols = dims[i, 2], checks = 1:4,
                             Option_NCD = TRUE, kindExpt = "SUDC", planter_mov1 = planter,
                             dim_data = 109, dim_data_1 = 105)$dt) > 0L
    }, logical(1))
    ok <- c(squarest_first = !is.unsorted(difference),
            sized = all(plots >= floor(105 * 1.11) & plots <= ceiling(105 * 1.20)),
            feasible = all(feasible))
    expect(all(ok), sprintf("%s: %s fails", planter, names(ok)[!ok][1L]))
  }
})

test_that("multiple diagonal choices accept both block orientations", {
  data <- data.frame(
    ENTRY = 1:124, NAME = paste0("E", 1:124),
    BLOCK = c(rep("ALL", 4), rep(1:3, c(30, 50, 40)))
  )
  counts <- vapply(c("By Row", "By Column"), function(stacked) {
    length(diagonal_dimension_choices(lines = 120, checks = 1:4, kindExpt = "DBUDC",
                                      stacked = stacked, data = data))
  }, integer(1))
  expect(all(counts > 0L), sprintf("%s offers no field", names(counts)[counts == 0L][1L]))
})

test_that("dimension searches return an empty vector for fields too small", {
  expect_identical(diagonal_dimension_choices(lines = 1, checks = 1:4), character())
})

test_that("dimension searches neither create nor consume a random seed", {
  with_dimension_test_rng({
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    diagonal_dimension_choices(80, 1:4)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    set.seed(28)
    before <- .Random.seed
    diagonal_dimension_choices(80, 1:4)
    expect_identical(.Random.seed, before)
  })
})

test_that("dimension searches reject malformed inputs with core conditions", {
  expect_all_classed(function(v) diagonal_dimension_choices(v, checks = 1:4),
                     list(NULL, NA_real_, Inf, "105", c(105, 106), 0, 1.5), "lines")
  expect_all_classed(function(v) diagonal_dimension_choices(105, v),
                     list(NULL, NA_real_, Inf, "4", 0, c(1, 1), 1.5), "checks")
  expect_calls_classed(list(
    kindExpt = function() diagonal_dimension_choices(105, 1:4, kindExpt = "bad"),
    stacked = function() diagonal_dimension_choices(105, 1:4, stacked = "bad"),
    planter = function() diagonal_dimension_choices(105, 1:4, planter = "bad"),
    minimum_extra = function() diagonal_dimension_choices(105, 1:4, minimum_extra = 0.3),
    data = function() diagonal_dimension_choices(105, 1:4, kindExpt = "DBUDC")))
})

test_that("field_dimensions() proposes field sizes for a prime-free range, and rejects bad counts", {
  # Regression test for the isPrime() negative-index bug: with lines = 105
  # the size range is 115:126 (no primes), and the buggy index returned no
  # candidate at all.
  expect_gt(length(unlist(field_dimensions(105))), 0)
  # Regression test: field_dimensions(NA) raised an unclassed "NA/NaN
  # argument", which validate_design() cannot turn into a message.
  expect_all_classed(field_dimensions, list(0, -1, NA_real_, NA, 2.5, c(2, 3), "abc"), "lines")
  skip_if_not_installed("shiny")
  err <- tryCatch(validate_design(field_dimensions(0)), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
})

test_that("optimized field choices list every field of at least 4 x 4, in both orientations", {
  expect_identical(optimized_dimension_choices(120),
                   c("10 x 12", "12 x 10", "15 x 8", "8 x 15", "6 x 20", "20 x 6",
                     "5 x 24", "24 x 5", "30 x 4", "4 x 30"))
  # Against all divisor pairs, including numbers with many factors
  for (plots in c(16, 120, 2^30, 73513440)) {
    choices <- optimized_dimension_choices(plots)
    divisors <- seq_len(floor(sqrt(plots)))
    divisors <- divisors[plots %% divisors == 0]
    pairs <- rbind(cbind(divisors, plots / divisors), cbind(plots / divisors, divisors))
    pairs <- unique(pairs[pairs[, 1] >= 4 & pairs[, 2] >= 4, , drop = FALSE])
    dims <- choice_dims(choices)
    expect_setequal(paste(dims[, 1], dims[, 2]), paste(pairs[, 1], pairs[, 2]))
  }
})

test_that("optimized field choices report an empty search and validate their plot count", {
  empty <- vapply(c(1, 12, 13, 26), function(plots) length(optimized_dimension_choices(plots)), integer(1))
  expect_identical(empty, rep(0L, 4))
  expect_all_classed(optimized_dimension_choices,
                     list(NULL, NA_real_, Inf, "120", c(100, 120), 0, -1, 3.5), "plots")
})
