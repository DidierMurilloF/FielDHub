# Package hygiene rules the project states as hard rules: dependency
# hygiene (no trivial or GPL dependencies), no process-global options left
# changed, and the CONTRIBUTING.md "Source layout".

# Dependency hygiene -----------------------------------------------------------

test_that("trivial dependencies stay out of Imports", {
  imports <- utils::packageDescription("FielDHub", fields = "Imports")
  packages <- trimws(sub("\\s*\\(.*$", "", strsplit(imports, ",")[[1L]]))
  expect_identical(intersect(c("viridis", "numbers", "config", "shinythemes"), packages), character())
  expect_identical(setdiff(c("bslib", "ggplot2", "viridisLite"), packages), character())
})

test_that("the lightweight viridis scale preserves the existing heatmap palette", {
  points <- seq(0, 1, length.out = 11)
  expected <- c(
    "#440154", "#482575", "#414487", "#35608D", "#2A788E", "#21908D",
    "#22A884", "#43BE71", "#7AD151", "#BCDF27", "#FDE725"
  )

  expect_identical(fieldhub_viridis_scale()$palette(points), expected)
})

test_that("FielDHub has no dependency on blocksdesign", {
  root <- testthat::test_path("..", "..")
  r_dir <- file.path(root, "R")
  skip_if_not(file.exists(file.path(r_dir, "fct_CRD.R")), "The R/ sources are not available.")

  description <- read.dcf(file.path(root, "DESCRIPTION"))
  for (field in c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")) {
    if (!field %in% colnames(description)) next
    declared <- trimws(sub("\\s*\\(.*$", "", strsplit(description[1L, field], ",")[[1L]]))
    expect_false("blocksdesign" %in% declared, info = field)
  }

  namespace <- readLines(file.path(root, "NAMESPACE"))
  expect_false(any(grepl("^\\s*import(From)?\\(\\s*blocksdesign\\b", namespace)))

  src <- list.files(file.path(root, "src"), full.names = TRUE)
  src <- src[!grepl("[.](o|so|dll|dylib)$", src) & !dir.exists(src)]
  src_hits <- character()
  for (path in src) {
    code <- sub("//.*$", "", readLines(path, warn = FALSE))
    hit <- grep("blocksdesign", code)
    if (length(hit) > 0L) src_hits <- c(src_hits, paste0(basename(path), ":", hit))
  }
  expect_identical(src_hits, character())

  quotes <- c("\"blocksdesign\"", "'blocksdesign'")
  offenders <- character()
  for (path in list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
    hits <- data[(data$token == "SYMBOL_PACKAGE" & data$text == "blocksdesign") |
                   (data$token == "STR_CONST" & data$text %in% quotes), ]
    if (nrow(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(path), ":", hits$line1))
    }
  }
  expect_identical(offenders, character(), info = paste(offenders, collapse = "\n"))

  loaders <- c("requireNamespace", "loadNamespace", "getFromNamespace",
               "skip_if_not_installed", "library", "require")
  allowed <- c("test-package-hygiene.R", "test-app-shell.R", "test-app-design-page.R",
               "test_app_dependencies.R", "test_design_args_parity.R")
  test_offenders <- character()
  tests <- list.files(testthat::test_path("."), pattern = "[.][Rr]$", full.names = TRUE)
  for (path in tests[!basename(tests) %in% allowed]) {
    data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
    if (is.null(data)) next
    if (any(data$token == "SYMBOL_PACKAGE" & data$text == "blocksdesign")) {
      test_offenders <- c(test_offenders, paste0(basename(path), ": blocksdesign::"))
    }
    for (h in which(data$token == "STR_CONST" & data$text %in% quotes)) {
      # The call that holds the string: the function name sits in a child
      # expression of the call expression, next to the argument.
      parent <- data$parent[h]
      call_name <- ""
      while (parent != 0L) {
        kids <- data$id[data$parent == parent & data$token == "expr"]
        fn <- data$text[data$parent %in% kids & data$token == "SYMBOL_FUNCTION_CALL"]
        if (length(fn) > 0L) {
          call_name <- fn[1L]
          break
        }
        parent <- data$parent[data$id == parent]
      }
      # The value of a `.package =` argument: the string follows `.package`, `=`.
      before <- data[data$line1 <= data$line1[h] & data$terminal, ]
      before <- before[order(before$line1, before$col1), ]
      pos <- which(before$id == data$id[h])
      is_package_arg <- length(pos) == 1L && pos > 2L &&
        before$token[pos - 1L] == "EQ_SUB" && before$text[pos - 2L] == ".package"
      if (call_name %in% loaders || is_package_arg) {
        test_offenders <- c(test_offenders, paste0(basename(path), ":", data$line1[h]))
      }
    }
  }
  expect_identical(test_offenders, character(), info = paste(test_offenders, collapse = "\n"))

  ignore <- readLines(file.path(root, ".Rbuildignore"))
  expect_true("^tools$" %in% ignore)
})

test_that("local planning artifacts are excluded from source packages", {
  root <- testthat::test_path("..", "..")
  path <- file.path(root, ".Rbuildignore")
  if (!file.exists(path)) skip("Source packaging rules are not installed")
  patterns <- readLines(path)
  expect_true(any(vapply(patterns, grepl, logical(1), x = ".superpowers", ignore.case = TRUE)))
})

# No global options left changed -----------------------------------------------

# Static architecture check: inspect code without starting a Shiny server.
writes_global_table_options <- function(code) {
  if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(FALSE)
  if (is.call(code) && identical(code[[1]], as.name("options")) &&
      "DT.options" %in% names(code)) return(TRUE)
  any(vapply(as.list(code), writes_global_table_options, logical(1)))
}

test_that("modules keep table options out of process-global state", {
  namespace <- asNamespace("FielDHub")
  modules <- ls(namespace, pattern = "^mod_.*_server$")
  offenders <- modules[vapply(modules, function(name) {
    writes_global_table_options(body(get(name, namespace)))
  }, logical(1))]
  expect_identical(offenders, character())
})

test_that("design engines leave options() unchanged", {
  calls <- list(
    quote(incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17)),
    quote(row_column(t = 12, nrows = 3, reps = 2, method = "twostage", seed = 22)),
    quote(row_column(t = 12, nrows = 3, reps = 2, method = "onestage", seed = 22)),
    quote(do_optim(design = "sparse", lines = 20, l = 4, copies_per_entry = 3,
                   add_checks = TRUE, checks = 2, seed = 1)),
    quote(multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                              rep_checks = c(4, 4), allow_fillers = TRUE, seed = 1)),
    quote(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1)))
  previous <- options("warn", "contrasts")
  on.exit(options(previous))
  for (call in calls) {
    changed <- character()
    for (warn in c(-1, 1, 2)) {
      options(warn = warn, contrasts = c("contr.sum", "contr.helmert"))
      before <- options("warn", "contrasts")
      suppressWarnings(eval(call))
      if (!identical(options("warn", "contrasts"), before)) changed <- c(changed, paste("warn =", warn))
    }
    expect(length(changed) == 0L, sprintf("%s changed options() when called with %s",
                                          deparse(call)[1], toString(changed)))
  }
})

# Source layout (CONTRIBUTING.md, "Source layout") -----------------------------
#
# Every file in R/ names its responsibility with a prefix, one fct_ file holds
# each exported design function, core code never reaches into the Shiny layer
# (app_*, mod_*), and no two core files depend on each other. File-name checks
# need the R/ sources, which R CMD check does not install, so they skip there.
# The Shiny-layer check also runs on the installed namespace through
# core_functions()/app_functions() (helper-source.R).

source_layout_prefixes <- c("fct", "engine", "layout", "render", "io",
                            "validate", "result", "sim", "api", "app", "mod")
source_layout_exceptions <- c("cpp11.R", "globals.R", "run_app.R")

# Shiny-layer functions whose names predate the app_ prefix. They live in
# app_ files (checked below when the sources are available), so they may call
# app_ functions. validate_design() (R/app_conditions.R) shows problems as
# Shiny validation messages.
unprefixed_app_functions <- "validate_design"

has_layout_sources <- function(dir) {
  file.exists(file.path(dir, "fct_CRD.R")) && file.exists(file.path(dir, "app_conditions.R"))
}

source_layout_dir <- function() {
  dir <- testthat::test_path("..", "..", "R")
  skip_if_not(has_layout_sources(dir), "The R/ sources are not available.")
  dir
}

is_app_file <- function(file) grepl("^(app|mod)_", file)

# Passes when `offenders` is empty; otherwise fails listing each one
expect_no_offenders <- function(offenders, what) {
  expect(length(offenders) == 0L,
         paste(c(paste0(what, ":"), paste0("  ", offenders)), collapse = "\n"))
}

# Every symbol an expression refers to, including default argument values,
# which fieldhub_symbol_refs() does not reach (they sit in the formals
# pairlist of a `function` call, not in a call).
source_symbol_refs <- function(expr) {
  refs <- fieldhub_symbol_refs(expr)
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (identical(e[[1]], as.name("function")) && is.pairlist(e[[2]])) {
      formals_list <- e[[2]]
      for (i in seq_along(formals_list)) {
        if (is.call(formals_list[[i]]) ||
            (is.symbol(formals_list[[i]]) && nzchar(as.character(formals_list[[i]])))) {
          refs <<- c(refs, fieldhub_symbol_refs(formals_list[[i]]))
        }
      }
    }
    for (i in seq_along(e)) if (is.call(e[[i]])) walk(e[[i]])
  }
  walk(expr)
  refs
}

# One row per top-level assignment in R/: the file, the name it defines, and
# the symbols its value refers to
source_definitions <- function(dir) {
  files <- sort(list.files(dir, pattern = "[.][Rr]$"))
  rows <- lapply(files, function(file) {
    exprs <- parse(file.path(dir, file), keep.source = FALSE)
    out <- list()
    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      assignment <- is.call(e) && is.symbol(e[[1]]) &&
        as.character(e[[1]]) %in% c("<-", "=") && is.symbol(e[[2]])
      name <- if (assignment) as.character(e[[2]]) else NA_character_
      out[[length(out) + 1L]] <- list(
        file = file, name = name,
        refs = unique(source_symbol_refs(if (assignment) e[[3]] else e))
      )
    }
    out
  })
  rows <- unlist(rows, recursive = FALSE)
  data.frame(
    file = vapply(rows, `[[`, "", "file"),
    name = vapply(rows, `[[`, "", "name"),
    refs = I(lapply(rows, `[[`, "refs")),
    stringsAsFactors = FALSE
  )
}

test_that("every R/ file names its responsibility with a known prefix", {
  dir <- source_layout_dir()
  files <- list.files(dir, pattern = "[.][Rr]$")
  pattern <- paste0("^(", paste(source_layout_prefixes, collapse = "|"), ")_[A-Za-z0-9_]+[.]R$")
  unknown <- files[!grepl(pattern, files) & !files %in% source_layout_exceptions]
  expect_no_offenders(unknown, "Files without a known prefix")
  expect_no_offenders(setdiff(source_layout_exceptions, files), "Missing exception files")
})

test_that("each exported design function has its own fct_ file", {
  dir <- source_layout_dir()
  defs <- source_definitions(dir)
  exports <- setdiff(getNamespaceExports("FielDHub"), "run_app")
  home <- defs$file[match(exports, defs$name)]
  expect_no_offenders(
    exports[is.na(home) | home != paste0("fct_", exports, ".R")],
    "Exported functions not defined in R/fct_<name>.R"
  )
  fct_files <- unique(defs$file[startsWith(defs$file, "fct_")])
  exported_per_file <- vapply(fct_files, function(file) {
    sum(defs$name[defs$file == file] %in% exports)
  }, integer(1))
  expect_no_offenders(names(exported_per_file)[exported_per_file != 1L],
                      "fct_ files without exactly one exported function")
})

test_that("the Shiny layer is referenced only from app_ and mod_ files", {
  dir <- source_layout_dir()
  defs <- source_definitions(dir)
  app_defs <- defs[is_app_file(defs$file) & !is.na(defs$name), ]
  app_symbols <- unique(app_defs$name)

  # app_*/mod_* names and the unprefixed app functions live in the app layer
  named_app <- defs[!is.na(defs$name) & grepl("^(app|mod)_", defs$name), ]
  expect_no_offenders(named_app$name[!is_app_file(named_app$file)],
                      "app_/mod_ names defined outside app_/mod_ files")
  expect_no_offenders(setdiff(unprefixed_app_functions, app_symbols),
                      "Allow-listed app functions not defined in app_/mod_ files")

  # Symbols only: a reference held in a string (do.call("mod_x"), get(),
  # match.fun()) is not detected.
  core <- defs[!is_app_file(defs$file) & defs$file != "run_app.R", ]
  offenders <- character()
  for (i in seq_len(nrow(core))) {
    hits <- intersect(core$refs[[i]], app_symbols)
    if (length(hits)) {
      offenders <- c(offenders, paste0(core$file[i], ": ",
                                       ifelse(is.na(core$name[i]), "<top level>", core$name[i]),
                                       " -> ", paste(hits, collapse = ", ")))
    }
  }
  expect_no_offenders(offenders, "Core definitions that refer to the Shiny layer")
})

test_that("no two R/ files outside the Shiny layer depend on each other", {
  dir <- source_layout_dir()
  defs <- source_definitions(dir)
  core <- defs[!is_app_file(defs$file) & !defs$file %in% source_layout_exceptions, ]
  named <- core[!is.na(core$name), ]
  home <- stats::setNames(named$file, named$name)
  adjacency <- list()
  for (i in seq_len(nrow(core))) {
    to <- setdiff(unique(unname(home[intersect(core$refs[[i]], names(home))])), core$file[i])
    adjacency[[core$file[i]]] <- unique(c(adjacency[[core$file[i]]], to))
  }
  files <- unique(core$file)
  state <- stats::setNames(integer(length(files)), files)  # 0 new, 1 open, 2 done
  path <- character()
  cycle <- character()
  visit <- function(file) {
    state[[file]] <<- 1L
    path <<- c(path, file)
    for (next_file in adjacency[[file]]) {
      if (length(cycle)) return(invisible())
      if (state[[next_file]] == 1L) {
        cycle <<- c(path[match(next_file, path):length(path)], next_file)
        return(invisible())
      }
      if (state[[next_file]] == 0L) visit(next_file)
    }
    path <<- head(path, -1L)
    state[[file]] <<- 2L
  }
  for (file in files) if (!length(cycle) && state[[file]] == 0L) visit(file)
  expect_no_offenders(if (length(cycle)) paste(cycle, collapse = " -> "),
                      "Dependency cycle between files")
})

test_that("core functions never refer to app_ or mod_ functions", {
  # Symbols only: a reference held in a string (do.call("mod_x"), get(),
  # match.fun()) is not detected.
  app <- names(app_functions())
  core <- core_functions()
  core <- core[setdiff(names(core), unprefixed_app_functions)]
  offenders <- character()
  for (name in names(core)) {
    hits <- intersect(fieldhub_symbol_refs(body(core[[name]])), app)
    if (length(hits)) offenders <- c(offenders, paste0(name, " -> ", paste(hits, collapse = ", ")))
  }
  expect_no_offenders(offenders, "Core functions that refer to app_/mod_ functions")
})
