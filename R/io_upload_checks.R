#' One source for upload examples, column guidance and uniqueness rules
#' @noRd
upload_rule_table <- function() {
  genotypes <- function(letters) paste0("Genotype", letters)
  entries <- data.frame(ENTRY = 1:9, NAME = genotypes(LETTERS[1:9]))
  checks <- data.frame(ENTRY = 1:9,
                       NAME = c("CHECK1", "CHECK2", "CHECK3", genotypes(LETTERS[1:6])))
  controls_note <- "Note that the controls must be in the first rows of the CSV file."
  consecutive_note <- "Entry numbers can be any set of consecutive positive numbers."
  rule <- function(example, columns = seq_along(example), omit_na = FALSE, paired = FALSE,
                   note = NULL, note_tag = "h4", fields = names(example)) {
    count <- length(fields)
    field_names <- if (count == 1L) fields else {
      paste(paste(utils::head(fields, -1L), collapse = ", "), utils::tail(fields, 1L), sep = " and ")
    }
    list(columns = columns, omit_na = omit_na, paired = paired, example = example,
         missing_columns = paste0("Data input needs at least ",
           c("one", "two", "three")[count], if (count == 1L) " column: " else " columns: ", field_names),
         note = note, note_tag = note_tag)
  }
  list(
    crd = rule(data.frame(TREATMENT = paste0("TRT_", LETTERS[1:9])),
               note = "Note that only the TREATMENT column is required."),
    rcbd = rule(data.frame(TREATMENT = paste0("TRT_", LETTERS[1:9])),
                note = paste("Note that only the TREATMENT column is required. When repeated",
                             "checks are enabled, the first rows of the file are taken as the checks.")),
    alpha = rule(entries, note = consecutive_note),
    rect = rule(entries, note = consecutive_note),
    rcd = rule(entries, note = consecutive_note),
    square = rule(entries, note = consecutive_note),
    mdiag = rule(checks, note = controls_note),
    sdiag = rule(checks, note = controls_note),
    sparse_allocation = rule(checks, note = controls_note),
    arcbd = rule(checks, note = controls_note),
    factorial = rule(data.frame(FACTOR = rep(c("A", "B", "C"), c(2, 3, 2)),
                                LEVEL = c("a0", "a1", "b0", "b1", "b2", "c0", "c1")), paired = TRUE),
    ibd = rule(data.frame(ENTRY = 1:9, NAME = paste0("TX-", 1:9)), note = consecutive_note),
    lsd = rule(data.frame(ROW = paste0("Period", 1:5), COLUMN = paste0("Cow", 1:5),
                          TREATMENT = paste0("Diet", 1:5))),
    multi_loc_prep = rule(data.frame(ENTRY = 1:10, NAME = paste0("Genotype-", LETTERS[1:10])),
      note = "Remark: If you want to include checks, please add them in the first rows of the file.",
      note_tag = "h5"),
    optim = rule(data.frame(ENTRY = 1:9,
      NAME = c("CHECK1", "CHECK2", "CHECK3", genotypes(LETTERS[1:6])),
      REPS = as.factor(c(rep(10, times = 3), rep(1, 6)))), columns = 1:2, note = controls_note),
    prep = rule(data.frame(ENTRY = 1:9, NAME = genotypes(LETTERS[1:9]),
      REPS = as.factor(c(rep(2, times = 3), rep(1, 6)))), columns = 1:2),
    spd = rule(data.frame(WHOLEPLOT = c("NFung", paste0("Fung", 1:4), rep("", 5)),
                          SUBPLOT = paste0("Beans", 1:10)), omit_na = TRUE),
    sspd = rule(data.frame(WHOLPLOT = c(paste0("IRR_", c("NO", "Yes")), rep("", 8)),
                           SUBPLOT = c("NFung", paste0("Fung", 1:4), rep("", 5)),
                           SUB_SUBPLOT = paste0("Beans", 1:10)), omit_na = TRUE,
                 fields = c("WHOLEPLOT", "SUBPLOT", "SUB_SUBPLOT")),
    strip = rule(data.frame(HPLOTS = LETTERS[1:5], VPLOTS = LETTERS[1:5]), omit_na = TRUE)
  )
}

#' Look up a module's upload rule without accepting incomplete keys
#' @noRd
upload_rule <- function(design) {
  if (is.factor(design)) design <- as.character(design)
  if (!is.character(design) || length(design) != 1L || is.na(design)) return(NULL)
  upload_rule_table()[[design]]
}

#' Column and missing-value rules for the app's legacy upload formats
#' @noRd
upload_validation_rule <- function(design) {
  rule <- upload_rule(design)
  if (is.null(rule)) return(NULL)
  rule[c("columns", "omit_na", "paired")]
}

#' Check upload uniqueness, returning NULL for missing columns or rules
#' @noRd
check_input <- function(design, dataIn) {
  rule <- upload_validation_rule(design)
  if (is.null(rule) || length(dim(dataIn)) != 2L || ncol(dataIn) < max(rule$columns)) return(NULL)
  if (rule$paired) return(factorial_levels_unique(dataIn))
  all(vapply(rule$columns, function(column) {
    values <- dataIn[, column]
    if (rule$omit_na) values <- as.vector(na.omit(values))
    isTRUE(all.equal(values, unique(values)))
  }, logical(1)))
}

#' Example entries table shown in a module's upload-format dialog
#' @param design One of the module upload keys.
#' @return A data frame.
#' @noRd
upload_format_example <- function(design) {
  rule <- upload_rule(design)
  if (is.null(rule)) fieldhub_abort("Unknown upload design: ", design)
  rule$example
}
