# Source layout (CONTRIBUTING.md, "Source layout"): every file in R/ names
# its responsibility with a prefix, one fct_ file holds each exported design
# function, core code never reaches into the Shiny layer (app_*, mod_*), and
# no two core files depend on each other.
#
# File-name checks need the R/ sources, which R CMD check does not install,
# so they skip there (ruling R2). The Shiny-layer check also runs on the
# installed namespace through core_functions()/app_functions()
# (helper-source.R).

source_layout_prefixes <- c("fct", "engine", "layout", "render", "io",
                            "validate", "result", "sim", "api", "app", "mod")
source_layout_exceptions <- c("globals.R", "run_app.R")

# Shiny-layer functions whose names predate the app_ prefix. They live in
# app_ files (checked below when the sources are available), so they may call
# app_ functions.
unprefixed_app_functions <- c("check_app_dependencies", "fieldhub_design_menus")

source_layout_dir <- function() {
  dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(dir), "The R/ sources are not available.")
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
