#' Build the spatial field book from its maps, preserving the legacy schema
#' @noRd
export_design <- function(G, movement_planter = NULL, location = NULL, Year = NULL,
                          data_file = NULL, reps = FALSE) {
  validate_planter(movement_planter)
  if (is.null(Year)) Year <- format(Sys.Date(), "%Y")
  rows <- nrow(G[[3]])
  cols <- ncol(G[[3]])
  size <- rows * cols
  planting <- planting_path(rows, cols, movement_planter)
  path <- field_path(rows, cols, movement_planter)
  location <- toupper(location)
  location_codes <- c(
    PROSPER = "PRO", BERTHOLD = "BER", CARRINGTON = "CAR", CASSELTON = "CAS",
    LANGDON = "LAN", OSNABROCK = "HOS", HETTINGER = "HET", MINOT = "MNT",
    "POLK CO" = "POL", WILLISTON = "WIL", WOLVERTON = "WOL", MANDAN = "MAN",
    HEBRON = "HEB", FARGO = "FAR"
  )
  code <- if (location %in% names(location_codes)) {
    unname(location_codes[location])
  } else {
    substr(location, start = 1, stop = 3)
  }
  book <- data.frame(
    ROW = planting[, "ROW"], COLUMN = planting[, "COLUMN"],
    ENTRY = values_along_path(G[[1]], path),
    PLOT = values_along_path(G[[2]], path),
    CHECKS = values_along_path(G[[3]], path),
    EXPT = values_along_path(G[[4]], path),
    LOCATION = rep(location, size), LOC = rep(code, size), YEAR = rep(Year, size),
    row.names = NULL, stringsAsFactors = FALSE
  )
  if (reps) book$BLOCK <- values_along_path(G[[5]], path)
  entry_names <- dplyr::distinct(data_file, ENTRY, .keep_all = TRUE)
  book <- merge(book, entry_names, by = "ENTRY", sort = FALSE)
  book[order(book$ROW, book$PLOT), ]
}
