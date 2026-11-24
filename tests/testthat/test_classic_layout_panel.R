# Plain choice calculations, HTML construction, and static bindings only.
test_that("a 'Replicate layout' is offered only where it has a layout option", {
  base <- c("Vertical" = "vertical", "Horizontal" = "horizontal")
  panel <- c(base, "Grid panel" = "grid_panel")
  # A grid panel has rows and columns of replicates, more than one of each:
  # two, three, five or seven replicates cannot lie so, and have no grid panel
  for (reps in 2:9) {
    offered <- if (reps %in% c(4, 6, 8, 9)) panel else base
    expect_identical(layout_stacking_choices(RCBD(t = 4, reps = reps, seed = 1)), offered,
                     info = paste("RCBD", reps))
    expect_identical(layout_stacking_choices(split_plot(wp = 2, sp = 2, reps = reps, seed = 1)),
                     offered, info = paste("split plot", reps))
    expect_identical(layout_stacking_choices(latin_square(t = 3, reps = reps, seed = 1)),
                     offered, info = paste("squares", reps))
  }
  for (reps in 2:6) {
    lattice <- alpha_lattice(t = 12, k = 3, reps = reps, seed = 1)
    expect_identical(layout_stacking_choices(lattice), if (reps %in% c(4, 6)) panel else base,
                     info = paste("alpha", reps))
    for (stacked in layout_stacking_choices(lattice)) {
      expect_gt(length(layout_choice_labels(lattice, stacked = stacked, include_grids = TRUE)), 0L)
    }
  }
  # A row-column design keeps the two stackings of its numbered layouts
  expect_identical(layout_stacking_choices(row_column(t = 12, nrows = 3, reps = 4, seed = 1,
                                                      iterations = 10)), base)
  # No replicates to arrange: one list of fields, and no stacking to choose
  without <- list(CRD(t = 6, reps = 4, seed = 1),
                  full_factorial(setfactors = c(2, 3), reps = 4, type = 1, seed = 1),
                  split_plot(wp = 3, sp = 2, reps = 4, type = 1, seed = 1),
                  split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 3, type = 1, seed = 1),
                  latin_square(t = 5, reps = 1, seed = 1),
                  # A design built with dimensions has its one grid
                  RCBD(t = 4, reps = 4, seed = 1, nrows = 4))
  for (design in without) {
    expect_identical(layout_stacking_choices(design), base[1L], info = class(design)[1L])
  }
})

test_that("classic layout presentation is described by the workflow registry", {
  modules <- names(fieldhub_classic_workflows())
  for (module in modules) {
    spec <- classic_workflow_spec(module)
    expect_true(is.list(spec$layout), info = module)
    ids <- spec$layout$ids
    expected <- c("layout", if (!module %in% c("CRD", "Latin_Rectangle")) "stacked",
                  if (!module %in% c("CRD", "LSD")) "location")
    expect_identical(names(ids), expected, info = module)
    expect_identical(anyDuplicated(c(spec$ids, spec$simulation_ids, ids, spec$layout$output)), 0L)
    expect_true(all(spec$layout$widths %in% 1:12))
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_layout"), 1L, info = module)
    expect_false(any(c("app_layout_selection", "layout_choices", "sqrt") %in% all.names(code)), info = module)
  }
  binding <- body(app_classic_layout)
  expect_identical(sum(all.names(binding) == "app_layout_selection"), 1L)
  expect_identical(sum(all.names(binding) == "app_classic_layout_panel"), 1L)
})

test_that("shared layout panels retain control labels, namespacing, and options", {
  skip_if_not_installed("shiny")
  set.seed(18)
  before <- .Random.seed
  x <- RCBD(t = 6, reps = 4, l = 2, plotNumber = c(101, 1001), seed = 7)
  for (module in names(fieldhub_classic_workflows())) {
    spec <- classic_workflow_spec(module)
    html <- as.character(app_classic_layout_panel(shiny::NS("panel"), spec, x,
                                                  locations = c(2, 4), planter = "cartesian"))
    for (id in c(spec$ids[["plot_type"]], spec$layout$ids)) {
      expect_true(grepl(paste0('id="panel-', id, '"'), html, fixed = TRUE), info = module)
    }
    for (label in c("Type of plot", "Entries", "Plots", "Heatmap", "Field dimensions",
                    "Layout options")) {
      expect_true(grepl(label, html, fixed = TRUE), info = module)
    }
    # Four blocks lie in a panel of two by two on every page with a "Replicate layout"
    expect_identical(grepl('<option value="grid_panel">Grid panel</option>', html, fixed = TRUE),
                     "stacked" %in% names(spec$layout$ids))
    expect_identical(grepl("Replicate layout", html, fixed = TRUE),
                     "stacked" %in% names(spec$layout$ids))
    expect_false(grepl("Reps layout|Layout option<|Stack Panel|optgroup", html))
    if ("location" %in% names(spec$layout$ids)) {
      expect_true(grepl('<option value="4">4</option>', html, fixed = TRUE))
    }
  }
  html <- as.character(app_classic_layout_panel(shiny::NS("panel"), classic_workflow_spec("RCBD"), x))
  expect_true(grepl('<option value="2">2</option>', html, fixed = TRUE))
  expect_error(app_classic_layout_panel(identity, classic_workflow_spec("RCBD"), NULL),
                class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

# The select of a panel: its label, its options as c(name = value), and whether
# it is hidden from the page
panel_select <- function(html, id) {
  at <- regexpr(paste0('(?s)<select[^>]*id="', id, '".*?</select>'), html, perl = TRUE)
  select <- regmatches(html, at)
  found <- regmatches(select, gregexpr('<option value="[^"]*"[^>]*>[^<]*', select))[[1L]]
  label <- regmatches(html, regexpr(paste0('<label[^>]*for="', id, '"[^>]*>[^<]*'), html))
  before <- substr(html, 1L, at)
  list(label = sub("^.*>", "", label),
       options = stats::setNames(sub('<option value="([^"]*)".*', "\\1", found),
                                 sub("^.*>", "", found)),
       hidden = grepl(paste0('<div hidden>\\s*<div[^>]*>\\s*<label[^>]*for="', id, '"'), html))
}

test_that("each menu holds the numbered layouts and the field grids of its own stacking", {
  design <- split_plot(wp = 3, sp = 2, reps = 4, seed = 1)
  # A grid is named by its size, as a numbered layout is; its value carries
  # the stacking of its menu. The numbered layouts come first, then the grids
  expect_identical(layout_choice_labels(design, stacked = "vertical", include_grids = TRUE),
                   c("4 \u00d7 6" = "2", "8 \u00d7 3" = "1", "12 \u00d7 2" = "grid:vertical:12:2",
                     "24 \u00d7 1" = "grid:vertical:24:1"))
  expect_identical(layout_choice_labels(design, stacked = "horizontal", include_grids = TRUE),
                   c("3 \u00d7 8" = "1", "6 \u00d7 4" = "grid:horizontal:6:4",
                     "2 \u00d7 12" = "grid:horizontal:2:12", "1 \u00d7 24" = "grid:horizontal:1:24"))
  # Every option of the grid panel says how its replicates lie
  expect_identical(layout_choice_labels(design, stacked = "grid_panel", include_grids = TRUE),
                   c("4 \u00d7 6 \u00b7 replicates 2 \u00d7 2" = "1",
                     "6 \u00d7 4 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:6:4",
                     "2 \u00d7 12 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:2:12",
                     "12 \u00d7 2 \u00b7 replicates 2 \u00d7 2" = "grid:grid_panel:12:2"))

  spec <- design_app_spec("SPD")$workflow
  html <- as.character(app_classic_layout_panel(identity, spec, design))
  stacking <- panel_select(html, spec$layout$ids[["stacked"]])
  expect_identical(stacking$label, "Replicate layout")
  expect_identical(stacking$options,
                   c("Vertical" = "vertical", "Horizontal" = "horizontal", "Grid panel" = "grid_panel"))
  expect_false(stacking$hidden)
  # The page opens on the vertical menu, on its first, the most square field
  fields <- panel_select(html, spec$layout$ids[["layout"]])
  expect_identical(fields$label, "Field dimensions")
  expect_identical(fields$options, layout_choice_labels(design, include_grids = TRUE))
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
  expect_identical(layout_stacking_choices(three), c("Vertical" = "vertical", "Horizontal" = "horizontal"))
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
  twelve <- RCBD(t = 6, reps = 12, seed = 1)
  panel <- layout_choice_labels(twelve, stacked = "grid_panel", include_grids = TRUE)
  expect_identical(panel[["6 \u00d7 12 \u00b7 replicates 3 \u00d7 4"]], "grid:grid_panel:6:12")
  expect_identical(sum(grepl("^6 \u00d7 12 ", names(panel))), 1L)
  # A numbered grid panel says how its replicates lie too, and is not
  # repeated as a grid
  lattice <- alpha_lattice(t = 12, k = 3, reps = 4, seed = 36)
  numbered <- layout_choice_labels(lattice, stacked = "grid_panel")
  panel <- layout_choice_labels(lattice, stacked = "grid_panel", include_grids = TRUE)
  expect_gt(length(numbered), 0L)
  expect_true(all(grepl(" \u00b7 replicates [0-9]+ \u00d7 [0-9]+$", names(numbered))))
  expect_identical(panel[names(numbered)], stats::setNames(as.character(numbered), names(numbered)))
  expect_identical(panel[["2 \u00d7 24 \u00b7 replicates 2 \u00d7 2"]], "grid:grid_panel:2:24")
  expect_true(all(grepl(" \u00b7 replicates [0-9]+ \u00d7 [0-9]+$", names(panel))))
  # Its stacks are in the menus of their stackings, by size alone
  expect_identical(layout_choice_labels(lattice, stacked = "vertical",
                                        include_grids = TRUE)[["4 \u00d7 12"]], "grid:vertical:4:12")
  expect_identical(layout_choice_labels(lattice, stacked = "horizontal",
                                        include_grids = TRUE)[["12 \u00d7 4"]], "grid:horizontal:12:4")
})

test_that("a menu opens on a numbered layout and lists the grids after the numbered ones", {
  # Regression test: the grids were once sorted in with the numbered layouts,
  # so the horizontal menu of this split plot opened on the 6 x 4 grid, not
  # on the 3 x 8 field of layout 1 as it always had
  split <- split_plot(wp = 3, sp = 2, reps = 4, seed = 1)
  horizontal <- layout_choice_labels(split, stacked = "horizontal", include_grids = TRUE)
  expect_identical(app_first_layout_choice(horizontal), "1")
  expect_identical(names(horizontal)[1L], "3 \u00d7 8")
  designs <- list(split, RCBD(t = 6, reps = 4, seed = 1), latin_square(t = 4, reps = 4, seed = 7),
                  incomplete_blocks(t = 16, k = 4, reps = 4, seed = 1),
                  alpha_lattice(t = 12, k = 3, reps = 4, seed = 36), CRD(t = 6, reps = 4, seed = 1))
  for (design in designs) {
    for (stacked in layout_stacking_choices(design)) {
      info <- paste(class(design)[1L], stacked)
      numbered <- layout_choice_labels(design, stacked = stacked)
      choices <- layout_choice_labels(design, stacked = stacked, include_grids = TRUE)
      grid <- grepl("^grid:", choices)
      # The numbered layouts in their own order, then the grids, the most
      # square first
      expect_identical(unname(choices[!grid]), as.character(numbered), info = info)
      expect_false(is.unsorted(grid), info = info)
      sizes <- vapply(strsplit(sub("^grid:[a-z_]+:", "", choices[grid]), ":"), as.numeric, numeric(2))
      if (length(sizes)) {
        expect_false(is.unsorted(order(abs(sizes[1L, ] - sizes[2L, ]), sizes[1L, ])), info = info)
      }
    }
    # The page opens on the layout it opens on without the grids
    opened <- layout_choice_labels(design, include_grids = TRUE)
    expect_identical(app_first_layout_choice(opened),
                     as.character(layout_choice_labels(design)[[1L]]))
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
  expect_identical(lies(drawn(blocks, panel[[1L]], "grid_panel"), "IBLOCK"), "columns")
  expect_identical(lies(drawn(blocks, panel[[2L]], "grid_panel"), "IBLOCK"), "rows")
  expect_identical(drawn(blocks, panel[[2L]], "grid_panel"),
                   field_layout(blocks, nrows = 8, ncols = 8, stacked = "grid_panel"))
  expect_identical(anyDuplicated(names(panel)), 0L)
  # Under a stack the two are told apart by that alone
  split <- split_plot(wp = 4, sp = 2, reps = 2, seed = 1)
  vertical <- layout_choice_labels(split, stacked = "vertical", include_grids = TRUE)
  expect_identical(vertical[["8 \u00d7 2 \u00b7 whole plots in columns"]], "3")
  expect_identical(vertical[["8 \u00d7 2 \u00b7 whole plots in rows"]], "grid:vertical:8:2")
  expect_identical(lies(drawn(split, "3", "vertical"), "WHOLE_PLOT"), "columns")
  expect_identical(lies(drawn(split, "grid:vertical:8:2", "vertical"), "WHOLE_PLOT"), "rows")
  # Regression test: the name read the first whole plot alone, and layout 3
  # was "whole plots in rows" although its whole plots of 6 wrap from a row
  # of 9 into the next
  wrapped <- split_split_plot(wp = 3, sp = 2, ssp = 3, reps = 2, seed = 13)
  vertical <- layout_choice_labels(wrapped, stacked = "vertical", include_grids = TRUE)
  expect_identical(vertical[["4 \u00d7 9 \u00b7 whole plots along the path"]], "3")
  expect_identical(vertical[["4 \u00d7 9 \u00b7 whole plots 2 \u00d7 3"]], "grid:vertical:4:9")
  expect_identical(lies(drawn(wrapped, "3", "vertical"), "WHOLE_PLOT"), "path")
  expect_identical(lies(drawn(wrapped, "grid:vertical:4:9", "vertical"), "WHOLE_PLOT"), "2 x 3")
  # A grid that is the field of a numbered layout, or differs from it only in
  # the path of its plots, is listed once: as the numbered layout
  rcbd <- layout_choice_labels(RCBD(t = 6, reps = 4, seed = 1), stacked = "vertical", include_grids = TRUE)
  expect_identical(names(rcbd), c("4 \u00d7 6", "8 \u00d7 3", "12 \u00d7 2", "24 \u00d7 1"))
  expect_false(any(grepl("^grid:", rcbd)))
  expect_identical(sum(grepl("^16 \u00d7 4", names(layout_choice_labels(
    blocks, stacked = "vertical", include_grids = TRUE)))), 3L)
})

test_that("a layout menu names its options by size and by what tells them apart", {
  # The same layout under two numbers is listed once
  expect_identical(names(layout_choice_labels(RCBD(t = 7, reps = 3, seed = 1), stacked = "horizontal")),
                   c("7 \u00d7 3", "1 \u00d7 21"))
  # Fields of one size differ in how their whole plots or their blocks lie
  expect_identical(names(layout_choice_labels(split_plot(wp = 4, sp = 2, reps = 2, seed = 1))),
                   c("4 \u00d7 4 \u00b7 whole plots in columns", "4 \u00d7 4 \u00b7 whole plots in rows",
                     "8 \u00d7 2"))
  expect_identical(names(layout_choice_labels(incomplete_blocks(t = 16, k = 4, reps = 4, seed = 1))),
                   c("8 \u00d7 8", "16 \u00d7 4 \u00b7 blocks 2 \u00d7 2",
                     "16 \u00d7 4 \u00b7 blocks in columns", "16 \u00d7 4 \u00b7 blocks in rows"))
})

test_that("every option of a 'Replicate layout' menu draws its replicates as its menu says", {
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
    for (planter in c("serpentine", "cartesian")) {
      for (stacked in layout_stacking_choices(design, planter = planter)) {
        choices <- layout_choice_labels(design, planter = planter, stacked = stacked,
                                        include_grids = TRUE)
        info <- paste(class(design)[1L], planter, stacked)
        expect_gt(length(choices), 0L)
        expect_identical(anyDuplicated(names(choices)), 0L, info = info)
        expect_false(any(grepl("^Grid|\\([0-9]+\\)$", names(choices))), info = info)
        # A grid carries the stacking of its menu; only a panel names its replicates
        expect_true(all(grepl(paste0("^grid:", stacked, ":"), choices[grepl("^grid:", choices)])),
                    info = info)
        expect_identical(grepl(" \u00b7 replicates [0-9]+ \u00d7 [0-9]+", names(choices)),
                         rep(stacked == "grid_panel", length(choices)), info = info)
        expect_false(any(grepl("stacked", names(choices))), info = info)
        for (i in seq_along(choices)) {
          book <- app_selected_layout_view(design, choices[[i]], planter, 1,
                                           stacked)$allSitesFieldbook
          expect_identical(replicates_lie(book, unit), stacked,
                           info = paste(info, names(choices)[i]))
          expect_identical(paste(max(book$ROW), "\u00d7", max(book$COLUMN)),
                           sub(" \u00b7 .*$", "", names(choices)[i]), info = info)
          rows_of_replicates <- regmatches(names(choices)[i],
                                           regexec("replicates ([0-9]+) \u00d7", names(choices)[i]))[[1L]]
          if (length(rows_of_replicates)) {
            tops <- tapply(book$ROW, book[[unit]], min)
            expect_identical(length(unique(tops)), as.integer(rows_of_replicates[2L]),
                             info = paste(info, names(choices)[i]))
          }
        }
      }
    }
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
    expect_identical(fields$label, "Field dimensions", info = module)
    expect_identical(fields$options, layout_choice_labels(design, include_grids = TRUE), info = module)
    # Sizes alone: nothing lies one way or another, and no size comes twice
    expect_true(all(grepl("^[0-9]+ \u00d7 [0-9]+$", names(fields$options))), info = module)
    expect_identical(anyDuplicated(names(fields$options)), 0L, info = module)
    # No "Replicate layout" on the page; its input, where the page has one,
    # is not shown
    if ("stacked" %in% names(spec$layout$ids)) {
      expect_true(panel_select(html, spec$layout$ids[["stacked"]])$hidden, info = module)
    } else {
      expect_false(grepl("Replicate layout", html, fixed = TRUE), info = module)
    }
  }
  # The numbered layouts come first (a page opens on the first), then the
  # field grids of the other sizes
  crd <- layout_choice_labels(CRD(t = 15, reps = 4, seed = 1), include_grids = TRUE)
  expect_identical(names(crd), paste(c(4, 15, 6, 10, 5, 12, 3, 20, 2, 30, 1, 60), "\u00d7",
                                     c(15, 4, 10, 6, 12, 5, 20, 3, 30, 2, 60, 1)))
  expect_identical(unname(crd[1:3]), c("1", "2", "grid:none:6:10"))
  # A grid of a design without replicates is the field of its dimensions
  for (design in designs[c("CRD", "FD", "SPD")]) {
    menu <- layout_choice_labels(design, include_grids = TRUE)
    grid <- menu[grepl("^grid:", menu)][[1L]]
    dims <- as.numeric(strsplit(grid, ":", fixed = TRUE)[[1L]][3:4])
    expect_identical(app_selected_layout_view(design, grid, "serpentine", 1, "vertical")$allSitesFieldbook,
                     field_layout(design, nrows = dims[1L], ncols = dims[2L]))
  }
  # A design built with dimensions has its one grid
  stored <- RCBD(t = 6, reps = 6, nrows = 4, ncols = 9, seed = 1)
  expect_identical(layout_choice_labels(stored, include_grids = TRUE), c("4 \u00d7 9" = "1"))
})
