#' Plain argument builders bridging app inputs to the public design API
#'
#' @description
#' One function per classic design module, each translating a named list of
#' already-parsed Shiny control values (`values`) and an optional parsed
#' upload (`data`) into the exact named argument list its public API function
#' expects. Every classic module server calls its engine only through
#' `do.call(<engine>, design_args_<Module>(values, data))`, so the app and a
#' direct API call with the same inputs and seed produce identical results
#' (M2 exit criterion). These builders are plain R: no Shiny, no
#' randomization, no validation beyond what `do.call()` itself triggers
#' inside the engine.
#'
#' `values` is a named list of parsed control values (numbers, character
#' vectors, logicals); an entry the module never fills in is simply absent
#' from `values`, and `values[["name"]]` then reads as `NULL`. For an argument
#' whose own API default is `NULL` (almost all of them: `t`, `k`, `nrows`,
#' `checks`, `rep_checks`, `setfactors`, `wp`/`sp`/`ssp`, `Hplots`/`Vplots`,
#' `locationNames`, `seed`, `data`), passing that `NULL` through reproduces
#' the default exactly. The few arguments whose API default is not `NULL`
#' (`l`, `planter`, `type`, `continuous`, `spread_checks`, `randomizeH`,
#' `randomizeV`) get an explicit fallback to that same default via `%||%`
#' below, so an incomplete `values` list still reproduces a direct call that
#' omits the argument. `data` is the parsed upload data frame, or `NULL`
#' when the design's entries are generated from counts.
#'
#' @name design_args
#' @noRd
NULL

#' Fall back to an API's own non-`NULL` default when `values` omits a field
#'
#' @description
#' Most builder fields default to `NULL` in both `values` and the API
#' function they feed, so a missing `values` entry already reproduces the
#' API default once passed through `do.call()`. A handful of arguments
#' default to something else (`l = 1`, `planter = "serpentine"`, `type = 2`,
#' `continuous = FALSE`, `spread_checks = TRUE`, `randomizeH = TRUE`,
#' `randomizeV = FALSE`); for those, substitute the same default here so an
#' incomplete `values` list still reproduces a direct call that omits the
#' argument, instead of overriding the default with an explicit `NULL`.
#' @noRd
`%||%` <- function(x, default) if (is.null(x)) default else x

#' Build CRD() arguments
#'
#' `values`: `t` (treatment count; `NULL` when `data` supplies the entries),
#' `reps`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_CRD <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    reps = values[["reps"]],
    plotNumber = values[["plot_start"]],
    locationNames = values[["location_names"]],
    seed = values[["seed"]],
    data = data
  )
}

#' Build RCBD() arguments
#'
#' `values`: `t`, `reps`, `l`, `planter`, `plot_start`, `location_names`,
#' `continuous`, `seed`, `checks`, `rep_checks`, `spread_checks`.
#' @noRd
design_args_RCBD <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    continuous = values[["continuous"]] %||% FALSE,
    planter = values[["planter"]] %||% "serpentine",
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data,
    checks = values[["checks"]],
    rep_checks = values[["rep_checks"]],
    spread_checks = values[["spread_checks"]] %||% TRUE
  )
}

#' Build latin_square() arguments
#'
#' `values`: `t`, `reps`, `plot_start`, `planter`, `location_names`, `seed`.
#' @noRd
design_args_LSD <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    reps = values[["reps"]],
    plotNumber = values[["plot_start"]],
    planter = values[["planter"]] %||% "serpentine",
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build full_factorial() arguments
#'
#' `values`: `setfactors` (`NULL` when `data` supplies factors/levels),
#' `reps`, `l`, `type` (`1` = CRD, `2` = RCBD), `plot_start`, `planter`,
#' `location_names`, `seed`.
#' @noRd
design_args_FD <- function(values, data = NULL) {
  list(
    setfactors = values[["setfactors"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    type = values[["type"]] %||% 2,
    plotNumber = values[["plot_start"]],
    planter = values[["planter"]] %||% "serpentine",
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build split_plot() arguments
#'
#' `values`: `wp`, `sp`, `reps`, `l`, `type` (`1` = CRD, `2` = RCBD),
#' `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_SPD <- function(values, data = NULL) {
  list(
    wp = values[["wp"]],
    sp = values[["sp"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    type = values[["type"]] %||% 2,
    plotNumber = values[["plot_start"]],
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build split_split_plot() arguments
#'
#' `values`: `wp`, `sp`, `ssp`, `reps`, `l`, `type` (`1` = CRD, `2` = RCBD),
#' `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_SSPD <- function(values, data = NULL) {
  list(
    wp = values[["wp"]],
    sp = values[["sp"]],
    ssp = values[["ssp"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    type = values[["type"]] %||% 2,
    plotNumber = values[["plot_start"]],
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build strip_plot() arguments
#'
#' `values`: `Hplots`, `Vplots`, `reps`, `l`, `planter`, `plot_start`,
#' `location_names`, `seed`, `randomizeH`, `randomizeV`.
#' @noRd
design_args_STRIPD <- function(values, data = NULL) {
  list(
    Hplots = values[["Hplots"]],
    Vplots = values[["Vplots"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    planter = values[["planter"]] %||% "serpentine",
    plotNumber = values[["plot_start"]],
    locationNames = values[["location_names"]],
    seed = values[["seed"]],
    randomizeH = values[["randomizeH"]] %||% TRUE,
    randomizeV = values[["randomizeV"]] %||% FALSE,
    data = data
  )
}

#' Build incomplete_blocks() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_IBD <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    k = values[["k"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build row_column() arguments
#'
#' `values`: `t`, `nrows`, `reps`, `l`, `plot_start`, `location_names`,
#' `seed`.
#' @noRd
design_args_RowCol <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    nrows = values[["nrows"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    seed = values[["seed"]],
    locationNames = values[["location_names"]],
    data = data
  )
}

#' Build alpha_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Alpha_Lattice <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    k = values[["k"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    locationNames = values[["location_names"]],
    seed = values[["seed"]],
    data = data
  )
}

#' Build square_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Square_Lattice <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    k = values[["k"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    locationNames = values[["location_names"]],
    seed = values[["seed"]],
    data = data
  )
}

#' Build rectangular_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Rectangular_Lattice <- function(values, data = NULL) {
  list(
    t = values[["t"]],
    k = values[["k"]],
    reps = values[["reps"]],
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]],
    locationNames = values[["location_names"]],
    seed = values[["seed"]],
    data = data
  )
}
