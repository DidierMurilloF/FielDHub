# Shared helpers for static/structural tests that inspect package function
# bodies through the namespace (asNamespace("FielDHub")) instead of parsing
# R/ source files. R/ is not available under R CMD check, so these work
# there too. See .superpowers/sdd/2026-09-27-m1-m3-completion/constraints.md
# ruling R2.

#' Functions defined in the FielDHub namespace, restricted to core code
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, excluding Shiny app/module/golem entry points
#'   (`app_*`, `golem_*`, `mod_*`, `run_app`).
core_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[!grepl("^(app_|golem_|mod_)|^run_app$", names(functions))]
}

#' Shiny app/module functions defined in the FielDHub namespace
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, restricted to `app_*`/`mod_*` entry points.
app_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[grepl("^(app_|mod_)", names(functions))]
}

# --- Positional `[`/`[[` indexing, found by walking the parsed call tree ---
#
# Used by test_design_schema.R to guard "select field-book columns by name,
# never by position" per expression, not per whole function: a function that
# is allowed one positional index elsewhere must still fail if a *different*
# expression in it selects/reorders columns positionally (including a
# reverted fix). Walking `body(f)` directly (no source text, no srcrefs)
# works under R CMD check (ruling R2) and is immune to an expression being
# wrapped across lines, unlike matching on `deparse(body(f))` text.

#' Whether `e` is a bare numeric literal, or a unary-minus of one
#'
#' @noRd
fieldhub_is_numeric_literal <- function(e) {
  if (is.numeric(e)) return(TRUE)
  is.call(e) && is.symbol(e[[1]]) && identical(as.character(e[[1]]), "-") &&
    length(e) == 2 && is.numeric(e[[2]])
}

#' Whether `e` is a column-index expression built from hard-coded numbers:
#' a numeric literal, a colon range with a literal endpoint, `c()` of numeric
#' literals, or `-c()` of numeric literals. Never true for a name or a
#' variable holding one.
#'
#' @noRd
fieldhub_is_positional_index <- function(e) {
  if (fieldhub_is_numeric_literal(e)) return(TRUE)
  if (!is.call(e) || !is.symbol(e[[1]])) return(FALSE)
  nm <- as.character(e[[1]])
  if (nm == ":" && length(e) == 3) {
    return(fieldhub_is_numeric_literal(e[[2]]) || fieldhub_is_numeric_literal(e[[3]]))
  }
  if (nm == "c") {
    args <- as.list(e)[-1]
    return(length(args) > 0 && all(vapply(args, fieldhub_is_numeric_literal, logical(1))))
  }
  if (nm == "-" && length(e) == 2 && is.call(e[[2]]) &&
      is.symbol(e[[2]][[1]]) && identical(as.character(e[[2]][[1]]), "c")) {
    args <- as.list(e[[2]])[-1]
    return(length(args) > 0 && all(vapply(args, fieldhub_is_numeric_literal, logical(1))))
  }
  FALSE
}

#' The position of the first unnamed element of call `e` at or after index
#' `from`, or `NA_integer_` if none. Reads each element fresh via `e[[i]]`/
#' `names(e)[i]` and never binds a call element to a plain variable: R's
#' missing-argument sentinel (the empty row index in `x[, j]`) raises
#' "argument is missing, with no default" the next time a *variable* holding
#' it is evaluated, even outside a function call.
#'
#' @noRd
fieldhub_first_unnamed_from <- function(e, from) {
  if (length(e) < from) return(NA_integer_)
  nms <- names(e)
  for (i in from:length(e)) {
    tag <- if (is.null(nms)) "" else nms[i]
    if (is.na(tag)) tag <- ""
    if (!nzchar(tag)) return(i)
  }
  NA_integer_
}

#' Whether `e` is `names(x)`, `colnames(x)` or `dimnames(x)[[2]]`: an
#' accessor whose result is a character vector of *column names*, as
#' opposed to some other vector a plain `x[3]` might index into. Restricting
#' the single-index `[` check (below) to these keeps genuinely positional
#' vector indexing on non-name vectors (`x[3]`, `plotNumber[locs]`, ...) out
#' of scope, so it doesn't explode the allow-list with unrelated hits.
#'
#' @noRd
fieldhub_is_name_accessor <- function(e) {
  if (is.call(e) && is.symbol(e[[1]]) && length(e) == 2 &&
      as.character(e[[1]]) %in% c("names", "colnames")) {
    return(TRUE)
  }
  if (is.call(e) && is.symbol(e[[1]]) && identical(as.character(e[[1]]), "[[") &&
      length(e) >= 3) {
    idx_pos <- fieldhub_first_unnamed_from(e, 3)
    if (!is.na(idx_pos) && fieldhub_is_numeric_literal(e[[idx_pos]]) &&
        isTRUE(as.numeric(e[[idx_pos]]) == 2)) {
      obj <- e[[2]]
      if (is.call(obj) && is.symbol(obj[[1]]) &&
          identical(as.character(obj[[1]]), "dimnames") && length(obj) == 2) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Every `[`/`[[` call in `expr` that selects a column by hard-coded
#' position: `x[, 3]`, `x[, 1:3]`, `x[, c(6, 7, 9)]`, `x[, -1]`,
#' `x[, -c(1, 2)]`, `x[[3]]`, plus a single-index positional read or rename
#' of column *names* themselves: `names(x)[3]`, `colnames(x)[c(1, 2)]`,
#' `colnames(x)[3] <- v`, `dimnames(x)[[2]][3]`. Matches an assignment
#' target the same as a read (`x[, 1] <- v` / `colnames(x)[1] <- v` is, in
#' the unevaluated parse tree, a `[` call inside a `<-` call; R only
#' rewrites it to `` `[<-` `` / `` `names<-` `` at evaluation time).
#'
#' @return A list of the matching call objects (not their positions).
#' @noRd
fieldhub_positional_index_calls <- function(expr) {
  hits <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is.symbol(e[[1]])) {
      head <- as.character(e[[1]])
      if (head == "[" && length(e) >= 4 && identical(e[[3]], quote(expr = ))) {
        col_pos <- fieldhub_first_unnamed_from(e, 4)
        if (!is.na(col_pos) && fieldhub_is_positional_index(e[[col_pos]])) {
          hits[[length(hits) + 1]] <<- e
        }
      } else if (head == "[" && length(e) >= 3 && fieldhub_is_name_accessor(e[[2]])) {
        idx_pos <- fieldhub_first_unnamed_from(e, 3)
        if (!is.na(idx_pos) && fieldhub_is_positional_index(e[[idx_pos]])) {
          hits[[length(hits) + 1]] <<- e
        }
      } else if (head == "[[" && length(e) >= 3) {
        idx_pos <- fieldhub_first_unnamed_from(e, 3)
        if (!is.na(idx_pos) && fieldhub_is_numeric_literal(e[[idx_pos]])) {
          hits[[length(hits) + 1]] <<- e
        }
      }
    }
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  hits
}

#' The bare function name a call head `h` (`e[[1]]` of some call `e`)
#' refers to, or `NA_character_` if `h` is not a plain call target: a
#' symbol (`print`), or a `pkg::fun`/`pkg:::fun` namespaced reference
#' (`shinyjs::useShinyjs`, itself parsed as a `::` call, not a symbol).
#'
#' @noRd
fieldhub_call_head_name <- function(h) {
  if (is.symbol(h)) return(as.character(h))
  if (is.call(h) && is.symbol(h[[1]]) && length(h) == 3 &&
      as.character(h[[1]]) %in% c("::", ":::") && is.symbol(h[[3]])) {
    return(as.character(h[[3]]))
  }
  NA_character_
}

#' Whether `expr` contains a call whose head is one of `names`, called
#' either bare (`print(x)`) or namespaced (`shinyjs::useShinyjs()`)
#'
#' Unlike `all.names(expr)`, this only matches call position, not a plain
#' symbol read of a same-named argument or local (a function with a
#' `print = FALSE` argument does not match `"print"` just because its body
#' reads that argument). Walks `e[[i]]` directly, as
#' `fieldhub_positional_index_calls()` does above, instead of binding it to
#' a bare variable first: an unsupplied-argument slot in the parse tree
#' (the empty symbol in `x[, j]`) raises "argument is missing, with no
#' default" the next time a *variable* holding it is evaluated.
#'
#' @return A single logical.
#' @noRd
fieldhub_calls_named <- function(expr, names) {
  found <- FALSE
  walk <- function(e) {
    if (found || !is.call(e)) return(invisible())
    head_name <- fieldhub_call_head_name(e[[1]])
    if (!is.na(head_name) && head_name %in% names) {
      found <<- TRUE
      return(invisible())
    }
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
      if (found) return(invisible())
    }
  }
  walk(expr)
  found
}
