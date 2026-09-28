#' Plain argument builders bridging app inputs to the public design API
#'
#' @description
#' One function per design module, each translating a named list of
#' already-parsed Shiny control values (`values`) and an optional parsed
#' upload (`data`) into the exact named argument list its public API function
#' expects. Every module server calls its engine only through
#' `do.call(<engine>, design_args_<Module>(values, data))` (the sparse and
#' multi-location p-rep modules also build their `do_optim()` allocation
#' through `design_args_<Module>_optim()`), so the app and a direct API call
#' with the same inputs and seed produce identical results (M2 exit
#' criterion). These builders are plain R: no Shiny, no randomization, no
#' validation beyond what `do.call()` itself triggers inside the engine.
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

#' Assemble the `values` crd_inputs() sends design_args_CRD(), from already-
#' parsed scalars. Shared by the module and its tests so both build `values`
#' the same way.
#'
#' `data`: the parsed upload data frame, or `NULL` on the generated path.
#' @noRd
design_values_CRD <- function(treatment_count, reps, planter, plot_start,
                              location_names, seed, data) {
  list(
    # CRD() derives everything from `data` when it is supplied and ignores
    # `t`; only send a bare treatment count on the generated path, so a
    # recorded metadata$parameters$t matches a direct CRD(data = ...) call
    # (which never supplies t) instead of recording an unused count.
    t = if (is.null(data)) treatment_count else NULL,
    reps = reps,
    planter = planter,
    plot_start = plot_start,
    location_names = location_names,
    seed = seed
  )
}

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

#' Assemble the `values` spd_inputs() sends design_args_SPD(), from already-
#' parsed scalars. Shared by the module and its tests so both build `values`
#' the same way.
#'
#' `data`: the parsed upload data frame, or `NULL` on the generated path.
#' @noRd
design_values_SPD <- function(wp_count, sp_count, reps, l, seed, planter,
                              plot_start, location_names, type, data) {
  list(
    # split_plot() ignores wp/sp when data is supplied (it recomputes both
    # from the data instead), so only send the generated-path counts; on the
    # upload path wp_count/sp_count are label strings pulled from the
    # uploaded entries, not a wp/sp count, and must not be recorded as one.
    wp = if (is.null(data)) wp_count else NULL,
    sp = if (is.null(data)) sp_count else NULL,
    reps = reps,
    l = l,
    seed = seed,
    planter = planter,
    plot_start = plot_start,
    location_names = location_names,
    type = type
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

#' Assemble the `values` sspd_inputs() sends design_args_SSPD(), from
#' already-parsed scalars. Shared by the module and its tests so both build
#' `values` the same way.
#'
#' `data`: the parsed upload data frame, or `NULL` on the generated path.
#' @noRd
design_values_SSPD <- function(wp_count, sp_count, ssp_count, reps, l, seed,
                               planter, plot_start, location_names, type, data) {
  list(
    # split_split_plot() ignores wp/sp/ssp when data is supplied (it
    # recomputes all three from the data instead), so only send the
    # generated-path counts; on the upload path wp_count/sp_count/ssp_count
    # are label strings pulled from the uploaded entries, not counts, and
    # must not be recorded as one.
    wp = if (is.null(data)) wp_count else NULL,
    sp = if (is.null(data)) sp_count else NULL,
    ssp = if (is.null(data)) ssp_count else NULL,
    reps = reps,
    l = l,
    seed = seed,
    planter = planter,
    plot_start = plot_start,
    location_names = location_names,
    type = type
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

#' Shared shape behind incomplete_blocks()/alpha_lattice()/square_lattice()/
#' rectangular_lattice(): all four take the same (t, k, reps, l, plotNumber,
#' locationNames, seed, data) arguments.
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_incomplete_block_family <- function(values, data = NULL) {
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

#' Build incomplete_blocks() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_IBD <- design_args_incomplete_block_family

#' Build alpha_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Alpha_Lattice <- design_args_incomplete_block_family

#' Build square_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Square_Lattice <- design_args_incomplete_block_family

#' Build rectangular_lattice() arguments
#'
#' `values`: `t`, `k`, `reps`, `l`, `plot_start`, `location_names`, `seed`.
#' @noRd
design_args_Rectangular_Lattice <- design_args_incomplete_block_family

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

# --- Spatial designs -------------------------------------------------------
#
# The spatial modules build their design at "Randomize!" time from the
# values parsed at "Run!" plus the field dimensions (and, for the diagonal
# designs, the percentage of checks) chosen afterwards. `values` then also
# carries `nrows`/`ncols` and those later choices. Design-specific entries
# use the API argument name (`lines`, `checks`, `blocks`, `checksPercent`,
# `copies_per_entry`, ...); the shared ones keep the classic names (`l`,
# `planter`, `plot_start`, `location_names`, `seed`), plus `expt_name` for
# the experiment name(s) every spatial engine takes as `exptName`.

#' Starting plots a spatial builder sends for `l` locations (ruling R9)
#'
#' @description The user's starting plots when there is one per location;
#' otherwise the per-location default the engine itself falls back to,
#' `default_plot_starts(l, base)`. The modules used to rebuild this default
#' with their own inline `seq()` formulas, and the sparse and multi-location
#' p-rep modules disagreed with their engines about the base (1001 in the
#' app, 1 in `sparse_allocation()`/`multi_location_prep()`); the builders now
#' pass the engine's own base, so the app and R agree.
#'
#' @param plot_start Parsed starting plot numbers, or NULL.
#' @param l Number of locations.
#' @param base First starting plot of the engine's default.
#' @noRd
location_plot_starts <- function(plot_start, l, base) {
  if (length(plot_start) == l) plot_start else default_plot_starts(l, base)
}

#' Location names a spatial builder sends: the user's when there is one per
#' location, otherwise NULL so the engine uses its own default names.
#' @noRd
location_names_or_null <- function(location_names, l) {
  if (length(location_names) == l) location_names
}

#' Drop builder arguments whose value is NULL
#'
#' @description `do.call()` then leaves them missing, so the engine applies
#' its own default. Needed for arguments that `sparse_allocation()` and
#' `multi_location_prep()` test with `missing()`: for those, an explicit
#' NULL is not the same as leaving the argument out.
#'
#' @param args Named argument list.
#' @param optional Names in `args` to drop when their value is NULL.
#' @noRd
drop_null_args <- function(args, optional) {
  drop <- optional[vapply(args[optional], is.null, logical(1))]
  args[setdiff(names(args), drop)]
}

#' Build diagonal_arrangement() arguments for the single diagonal module
#'
#' `values`: `nrows`, `ncols`, `lines` (generated entries; ignored when
#' `data` supplies the entry list), `checks`, `planter`, `l`, `plot_start`,
#' `seed`, `expt_name`, `location_names`, `checksPercent`. Wrong-length
#' `plot_start`/`location_names` fall back to the engine's own defaults.
#' @noRd
design_args_Diagonal <- function(values, data = NULL) {
  l <- values[["l"]] %||% 1
  list(
    nrows = values[["nrows"]],
    ncols = values[["ncols"]],
    lines = if (is.null(data)) values[["lines"]],
    checks = values[["checks"]],
    planter = values[["planter"]] %||% "serpentine",
    l = l,
    plotNumber = location_plot_starts(values[["plot_start"]] %||% 101, l, base = 1001),
    kindExpt = "SUDC",
    seed = values[["seed"]],
    exptName = values[["expt_name"]],
    locationNames = location_names_or_null(values[["location_names"]], l),
    data = data,
    checksPercent = values[["checksPercent"]]
  )
}

#' Build diagonal_arrangement() arguments for the multiple diagonal module
#'
#' `values`: `nrows`, `ncols`, `lines` (generated entries; ignored when
#' `data` supplies the entry list), `checks`, `planter`, `l`, `plot_start`,
#' `stacked` (`"By Row"`/`"By Column"`, sent as `splitBy`), `seed`,
#' `blocks` (entries per experiment), `expt_name`, `location_names`,
#' `checksPercent`, `sameEntries`.
#'
#' Every location gets the same starting plots: one start per experiment,
#' or one start for all of them. Any other number of starts falls back to
#' the engine's own per-location default, `default_plot_starts(l, 1001)`
#' (ruling R9).
#' @noRd
design_args_diagonal_multiple <- function(values, data = NULL) {
  l <- values[["l"]] %||% 1
  plot_start <- values[["plot_start"]] %||% 101
  plot_starts <- if (length(plot_start) == length(values[["blocks"]])) {
    rep(list(plot_start), l)
  } else if (length(plot_start) == 1L) {
    rep(plot_start, l)
  } else {
    default_plot_starts(l, 1001)
  }
  list(
    nrows = values[["nrows"]],
    ncols = values[["ncols"]],
    lines = if (is.null(data)) values[["lines"]],
    checks = values[["checks"]],
    planter = values[["planter"]] %||% "serpentine",
    l = l,
    plotNumber = plot_starts,
    kindExpt = "DBUDC",
    splitBy = if (identical(values[["stacked"]], "By Column")) "column" else "row",
    seed = values[["seed"]],
    blocks = values[["blocks"]],
    exptName = values[["expt_name"]],
    locationNames = location_names_or_null(values[["location_names"]], l),
    data = data,
    checksPercent = values[["checksPercent"]],
    sameEntries = values[["sameEntries"]] %||% FALSE
  )
}

#' Build do_optim() arguments for the sparse allocation module's Run! step
#'
#' `values`: `lines`, `l`, `copies_per_entry`, `checks`, `seed`. `data`
#' (the uploaded entry list, checks first) is only validated here; the
#' allocation itself depends on the counts, and `sparse_allocation()`
#' merges the list into the locations.
#' @noRd
design_args_sparse_allocation_optim <- function(values, data = NULL) {
  list(
    design = "sparse",
    lines = values[["lines"]],
    l = values[["l"]],
    copies_per_entry = values[["copies_per_entry"]],
    add_checks = TRUE,
    checks = values[["checks"]],
    seed = values[["seed"]],
    data = data
  )
}

#' Build sparse_allocation() arguments for the sparse allocation module
#'
#' `values`: the `design_args_sparse_allocation_optim()` values plus
#' `nrows`, `ncols`, `planter`, `plot_start`, `expt_name`,
#' `location_names`, `sparse_list` (the allocation computed at Run!) and
#' `checksPercent`.
#'
#' `sparse_allocation()` tests `missing()` for several arguments, so the
#' ones the app cannot fill are left out instead of sent as NULL: misfit
#' location names and a blank experiment name get the engine's own
#' defaults (`LOC1`, ... and `"SparseExpt"`), and without `sparse_list` or
#' field dimensions the engine computes them itself. Misfit starting plots
#' become the engine's own default, `default_plot_starts(l, 1)` (ruling R9;
#' the module used to start its default at 1001).
#' @noRd
design_args_sparse_allocation <- function(values, data = NULL) {
  l <- values[["l"]]
  expt_name <- values[["expt_name"]]
  args <- list(
    lines = values[["lines"]],
    nrows = values[["nrows"]],
    ncols = values[["ncols"]],
    l = l,
    planter = values[["planter"]] %||% "serpentine",
    plotNumber = location_plot_starts(values[["plot_start"]], l, base = 1),
    copies_per_entry = values[["copies_per_entry"]],
    checks = values[["checks"]],
    exptName = if (length(expt_name) > 0L) expt_name[1],
    locationNames = location_names_or_null(values[["location_names"]], l),
    sparse_list = values[["sparse_list"]],
    seed = values[["seed"]],
    data = data,
    checksPercent = values[["checksPercent"]]
  )
  drop_null_args(args, c("nrows", "ncols", "exptName", "locationNames", "sparse_list"))
}

#' Names of the entries of an allocation table, in allocation-row order
#'
#' @description The allocation table of the sparse and multi-location p-rep
#' modules has one row per entry `1..lines`. Its names are those
#' `do_optim()` gave the entries (`G-1`, ... for a generated list), read
#' back from the allocation instead of rebuilt by the module, so the table
#' always shows the labels of the field book.
#'
#' @param allocation A `do_optim()` result.
#' @param lines Number of entries, excluding checks.
#' @return A character vector of length `lines`.
#' @noRd
allocation_entry_names <- function(allocation, lines) {
  entries <- allocation$multi_location_data
  as.character(entries$NAME[match(seq_len(lines), entries$ENTRY)])
}

#' Build optimized_arrangement() arguments for the Optim module
#'
#' `values`: `nrows`, `ncols`, `lines`, `checks` and `rep_checks` (the
#' generated path: optimized_arrangement() builds the CH1.., G.. entry list
#' from these counts; ignored when `data` supplies the ENTRY/NAME/REPS
#' list), `planter`, `l`, `plot_start`, `seed`, `expt_name`,
#' `location_names`.
#' @noRd
design_args_Optim <- function(values, data = NULL) {
  generated <- is.null(data)
  list(
    nrows = values[["nrows"]],
    ncols = values[["ncols"]],
    lines = if (generated) values[["lines"]],
    checks = if (generated) values[["checks"]],
    planter = values[["planter"]] %||% "serpentine",
    l = values[["l"]] %||% 1,
    plotNumber = values[["plot_start"]] %||% 101,
    seed = values[["seed"]],
    exptName = values[["expt_name"]],
    locationNames = values[["location_names"]],
    data = data,
    rep_checks = if (generated) values[["rep_checks"]]
  )
}
