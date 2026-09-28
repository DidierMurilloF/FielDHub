#' Column and missing-value rules for the app's legacy upload formats
#' @noRd
upload_validation_rule <- function(design) {
  if (is.factor(design)) design <- as.character(design)
  if (!is.character(design) || length(design) != 1L || is.na(design)) return(NULL)
  columns <- switch(design,
    crd = , rcbd = 1L,
    lsd = , sspd = 1:3,
    sdiag = , mdiag = , optim = , arcbd = , prep = , square = , rect = ,
    alpha = , ibd = , rcd = , factorial = , spd = , strip = 1:2,
    NULL
  )
  if (is.null(columns)) return(NULL)
  list(columns = columns, omit_na = design %in% c("spd", "sspd", "strip"),
        paired = design == "factorial")
}

#' Check upload uniqueness, returning NULL for missing columns or rules
#' @noRd
check_input <- function(design, dataIn) {
  rule <- upload_validation_rule(design)
  if (is.null(rule) || length(dim(dataIn)) != 2L ||
      ncol(dataIn) < max(rule$columns)) return(NULL)
  if (rule$paired) return(factorial_levels_unique(dataIn))
  all(vapply(rule$columns, function(column) {
    values <- dataIn[, column]
    if (rule$omit_na) values <- as.vector(na.omit(values))
    isTRUE(all.equal(values, unique(values)))
  }, logical(1)))
}

#' Example entries table shown in a module's upload-format dialog
#'
#' @description The table each module's format-info dialog has always
#' shown next to its own upload rule (see \code{upload_validation_rule()}
#' for the columns/uniqueness rule of the same designs). Content is
#' preserved from each module. \code{design} identifies the module the way
#' \code{app_upload_ui()}/\code{app_upload_dialog()} do: a few modules share
#' the same upload validation rule (e.g. \code{"sdiag"}, used by the single
#' Diagonal, multi-location prep and sparse-allocation modules) but keep an
#' example table of their own, so this recognises more specific keys than
#' \code{upload_validation_rule()} does for those (\code{"multi_loc_prep"},
#' \code{"sparse_allocation"}).
#'
#' @param design One of the module upload keys.
#' @return A data frame.
#' @noRd
upload_format_example <- function(design) {
  genotypes <- function(letters) paste0("Genotype", letters)
  checks_then_genotypes <- function() data.frame(
    ENTRY = 1:9,
    NAME = c("CHECK1", "CHECK2", "CHECK3", genotypes(LETTERS[1:6]))
  )
  switch(design,
    crd = ,
    rcbd = data.frame(TREATMENT = paste0("TRT_", LETTERS[1:9])),
    alpha = ,
    rect = ,
    rcd = ,
    square = data.frame(ENTRY = 1:9, NAME = genotypes(LETTERS[1:9])),
    mdiag = ,
    sdiag = ,
    sparse_allocation = ,
    arcbd = checks_then_genotypes(),
    factorial = data.frame(
      FACTOR = rep(c("A", "B", "C"), c(2, 3, 2)),
      LEVEL = c("a0", "a1", "b0", "b1", "b2", "c0", "c1")
    ),
    ibd = data.frame(ENTRY = 1:9, NAME = paste0("TX-", 1:9)),
    lsd = data.frame(
      ROW = paste0("Period", 1:5),
      COLUMN = paste0("Cow", 1:5),
      TREATMENT = paste0("Diet", 1:5)
    ),
    multi_loc_prep = data.frame(ENTRY = 1:10, NAME = paste0("Genotype-", LETTERS[1:10])),
    optim = data.frame(
      ENTRY = 1:9,
      NAME = c("CHECK1", "CHECK2", "CHECK3", genotypes(LETTERS[1:6])),
      REPS = as.factor(c(rep(10, times = 3), rep(1, 6)))
    ),
    prep = data.frame(
      ENTRY = 1:9,
      NAME = genotypes(LETTERS[1:9]),
      REPS = as.factor(c(rep(2, times = 3), rep(1, 6)))
    ),
    spd = data.frame(
      WHOLEPLOT = c("NFung", paste0("Fung", 1:4), rep("", 5)),
      SUBPLOT = paste0("Beans", 1:10)
    ),
    sspd = data.frame(
      WHOLPLOT = c(paste0("IRR_", c("NO", "Yes")), rep("", 8)),
      SUBPLOT = c("NFung", paste0("Fung", 1:4), rep("", 5)),
      SUB_SUBPLOT = paste0("Beans", 1:10)
    ),
    strip = data.frame(HPLOTS = LETTERS[1:5], VPLOTS = LETTERS[1:5]),
    fieldhub_abort("Unknown upload design: ", design)
  )
}
