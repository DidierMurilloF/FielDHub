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
