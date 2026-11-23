# Records the phase 2 test fixtures from a pre-change FielDHub library and
# blocksdesign (black-box calls only):
#   Rscript tools/record-phase2-fixtures.R <package-library>
# Writes tests/testthat/fixtures/rectangular-lattice-bounds.csv (t,k,r,bound: the
# Level 2 A-Bound of rectangular_lattice(t, k, reps = r, seed = 1)) and
# tests/testthat/fixtures/blocksdesign-references.rds (output shape of
# blocksdesign::blocks() and its optima for two balanced shapes), and beside them
# phase2-fixtures.meta.rds: the path and version of blocksdesign and of
# FielDHub, the R version and the platform of the run.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Usage: Rscript tools/record-phase2-fixtures.R <package-library>")
.libPaths(c(normalizePath(arguments[1], mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
out <- file.path(script_dir, "..", "tests", "testthat", "fixtures")
suppressMessages(library(FielDHub))
meta <- list(FielDHub = as.character(utils::packageVersion("FielDHub")),
             FielDHub_path = find.package("FielDHub"),
             blocksdesign = tryCatch(as.character(utils::packageVersion("blocksdesign")),
                                     error = function(e) NA_character_),
             blocksdesign_path = tryCatch(find.package("blocksdesign"),
                                          error = function(e) NA_character_),
             R = R.version.string, platform = R.version$platform)
cat("FielDHub", meta$FielDHub, meta$FielDHub_path, "blocksdesign", meta$blocksdesign,
    meta$blocksdesign_path, "\n")

shapes <- data.frame(t = c(42, 56, 72, 30, 90, 182, 20, 72, 22350),
                     k = c(6, 7, 8, 5, 9, 13, 4, 8, 149),
                     r = c(4, 4, 4, 3, 3, 3, 5, 3, 3))
shapes$bound <- vapply(seq_len(nrow(shapes)), function(i) {
  d <- rectangular_lattice(shapes$t[i], shapes$k[i], reps = shapes$r[i], seed = 1)
  model <- d$blocksModel
  if (!is.data.frame(model)) model <- model[[1]]
  as.numeric(model[["A-Bound"]][as.character(model$Level) == "2"])
}, numeric(1))
stopifnot(!anyNA(shapes$bound))
utils::write.csv(shapes, file.path(out, "rectangular-lattice-bounds.csv"), row.names = FALSE)

b <- blocksdesign::blocks(10, 2, 4, searches = 1, seed = 1)
shape <- list(design = vapply(b$Design, function(x) class(x)[1], character(1)),
              blocks_model = vapply(b$Blocks_model, function(x) class(x)[1], character(1)),
              level_1 = levels(b$Design$Level_1))
optima <- do.call(rbind, lapply(list(c(7, 3, 7), c(13, 4, 13)), function(s) {
  m <- blocksdesign::blocks(s[1], s[2], s[3], searches = 20, seed = 1)$Blocks_model
  data.frame(v = s[1], r = s[2], b = s[3], D = m[["D-Efficiency"]][1], A = m[["A-Efficiency"]][1])
}))
saveRDS(list(shape = shape, optima = optima), file.path(out, "blocksdesign-references.rds"))
saveRDS(meta, file.path(out, "phase2-fixtures.meta.rds"))
