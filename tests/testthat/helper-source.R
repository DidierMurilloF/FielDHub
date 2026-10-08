# Shared helpers for static/structural tests that inspect package function
# bodies through the namespace (asNamespace("FielDHub")) instead of parsing
# R/ source files. R/ is not available under R CMD check, so these work
# there too. See .superpowers/sdd/2026-09-27-m1-m3-completion/constraints.md
# ruling R2.

# Unwrap only covr's exact counter wrapper when checking the first expression.
# Real branches and other calls must remain visible to structural assertions.
fieldhub_uninstrument <- function(expr) {
  if (is.call(expr) && length(expr) == 3L && identical(expr[[1L]], as.name("if")) &&
      identical(expr[[2L]], TRUE) && is.call(expr[[3L]]) && length(expr[[3L]]) == 3L &&
      identical(expr[[3L]][[1L]], as.name("{")) && is.call(expr[[3L]][[2L]]) &&
      identical(expr[[3L]][[2L]][[1L]], call(":::", as.name("covr"), as.name("count")))) {
    return(fieldhub_uninstrument(expr[[3L]][[3L]]))
  }
  expr
}

#' Functions defined in the FielDHub namespace, restricted to core code
#'
#' @return A named list of functions whose environment is the FielDHub
#'   namespace, excluding Shiny app/module entry points
#'   (`app_*`, `mod_*`, `run_app`).
core_functions <- function() {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions[!grepl("^(app_|mod_)|^run_app$", names(functions))]
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

#' Every function name `expr` calls, bare (`f(x)`) or namespaced
#' (`pkg::f(x)`)
#'
#' Unlike `all.names(expr)`, a plain symbol read -- an input ID such as
#' `input$get_random` or an output such as `output$field_layout` -- is not
#' mistaken for a call to a same-named package function. Walks `e[[i]]`
#' directly for the same reason `fieldhub_calls_named()` does.
#'
#' @return A character vector (with repeats, in walk order).
#' @noRd
fieldhub_call_heads <- function(expr) {
  heads <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    head_name <- fieldhub_call_head_name(e[[1]])
    if (!is.na(head_name)) heads <<- c(heads, head_name)
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  heads
}

#' The `do.call(<fun>, <builder>(...))` calls in `expr`
#'
#' @return A list with one `c(fun = , builder = )` character pair per
#'   `do.call()` whose first argument is a symbol; `builder` is the head of
#'   the second argument when it is a call, `NA` otherwise.
#' @noRd
fieldhub_do_call_pairs <- function(expr) {
  pairs <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (identical(fieldhub_call_head_name(e[[1]]), "do.call") && length(e) >= 3 &&
        is.symbol(e[[2]])) {
      builder <- if (is.call(e[[3]])) fieldhub_call_head_name(e[[3]][[1]]) else NA_character_
      pairs[[length(pairs) + 1L]] <<- c(fun = as.character(e[[2]]), builder = builder)
    }
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  pairs
}

#' Every symbol `expr` refers to, except the field names of `x$name` and
#' `x@name`
#'
#' Like `all.names(expr)`, but an input/output ID such as
#' `output$sparse_allocation` does not count as a reference to the
#' same-named package function.
#'
#' @return A character vector (with repeats).
#' @noRd
fieldhub_symbol_refs <- function(expr) {
  refs <- character()
  walk <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (nzchar(name)) refs <<- c(refs, name)
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    field_access <- is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("$", "@")
    n <- length(e)
    for (i in seq_len(n)) {
      if (field_access && i == 3L) next
      if (is.call(e[[i]]) || is.symbol(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  refs
}

#' Every `:` (range) call in `expr` with an `as.numeric()` or `sum()` call on
#' either side, such as `1:as.numeric(input$l.diagonal)` or `1:sum(repGens)`
#'
#' `as.numeric(<Shiny input>)` is `NA` for a cleared numeric input, and
#' `1:NA` raises "NA/NaN argument"; inside an observer (as opposed to a
#' `reactive()`/`render*()`), that ends the Shiny session (Task 13). Used by
#' `test_location_view_choices.R` to keep this pattern out of every
#' `mod_*_server()`/`app_*` function body, in place of the validated plain
#' helpers in R/validate_locations.R.
#'
#' @return A list of the matching call objects.
#' @noRd
fieldhub_colon_as_numeric_calls <- function(expr) {
  hits <- list()
  operand_is_risky <- function(e) fieldhub_calls_named(e, c("as.numeric", "sum"))
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), ":") && length(e) == 3 &&
        (operand_is_risky(e[[2]]) || operand_is_risky(e[[3]]))) {
      hits[[length(hits) + 1]] <<- e
    }
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  hits
}

#' Body of the server function that runs a design of the app registry
#'
#' The classic designs share one generic server (`mod_design_server()`,
#' driven by `design_app_spec(module)`); the spatial designs have their
#' own. Structural tests that used to inspect `mod_<Module>_server()` ask
#' for the server that runs `module` instead.
#'
#' @param module Registry workflow name (`"CRD"`, `"Diagonal"`, ...).
design_server_body <- function(module) {
  entries <- Filter(function(entry) identical(entry$workflow, module), fieldhub_app_registry())
  stopifnot(length(entries) == 1L)
  body(get(entries[[1L]]$server, asNamespace("FielDHub")))
}

#' Body of the code that runs the steps and results of a spatial design
#'
#' A spatial design with a page spec runs through the generic server
#' (`mod_design_server()`), which hands its steps and results to the
#' generic spatial page (`app_spatial_page()`); a spatial design without one
#' has a server of its own. Structural tests that used to inspect
#' `mod_<Module>_server()` ask for the code that runs `module` instead.
#'
#' @param module Registry workflow name (`"Diagonal"`, `"Optim"`, ...).
spatial_server_body <- function(module) {
  entries <- Filter(function(entry) identical(entry$workflow, module), fieldhub_app_registry())
  stopifnot(length(entries) == 1L)
  name <- if (is.null(entries[[1L]]$spec)) entries[[1L]]$server else "app_spatial_page"
  body(get(name, asNamespace("FielDHub")))
}

#' Every function a page spec runs while the page is used
#'
#' The spec's `values`, `data` and `upload_shape`, the parsers, choices and
#' previews of its controls and steps, and its result views (`entries`,
#' `panels`, `setup`, `field_size`, `accept`), with the local helpers these
#' call (a parser a constructor wraps, `entry_count()`, ...). FielDHub
#' functions they reach through a variable (an `upload_check` such as
#' `check_reps_upload()`) are returned by name.
#'
#' @return `list(closures = <list of functions>, named = <character>)`.
spec_runtime_functions <- function(spec) {
  namespace <- asNamespace("FielDHub")
  package <- Filter(function(f) is.function(f) && identical(environment(f), namespace),
                    mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE))
  found <- list()
  named <- character()
  add <- function(f) {
    if (!is.function(f) || is.primitive(f)) return(invisible())
    if (isNamespace(environment(f))) {
      hit <- names(Filter(function(g) identical(g, f), package))
      named <<- unique(c(named, hit))
      return(invisible())
    }
    if (any(vapply(found, identical, logical(1), f))) return(invisible())
    found[[length(found) + 1L]] <<- f
    env <- environment(f)
    for (name in intersect(unique(all.names(body(f))), ls(env, all.names = TRUE))) {
      add(get(name, envir = env))
    }
  }
  controls <- c(spec$controls, spec$steps)
  roots <- c(list(spec$values, spec$data, spec$upload_shape, spec$field_size, spec$accept,
                  spec$setup$view),
             lapply(controls, `[[`, "parse"), lapply(controls, `[[`, "options"),
             lapply(controls, `[[`, "preview"), spec$entries, lapply(spec$panels, `[[`, "view"))
  for (f in roots) add(f)
  list(closures = found, named = named)
}
