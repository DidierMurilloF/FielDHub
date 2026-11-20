# The shapes that the onestage build does not admit (phase 2 spec 7,
# "admission coverage"; Task 8 Step 6):
#
#   Rscript tools/onestage-admission-list.R <out.csv> [library]
#
# Universe: every (t, nrows, r) with t in {20, 30, 50, 100, 200, 300, 500, 750,
# 1000, 1500, 2000}, thin(divisors(t, 2)) nrows (as nested_golden_grid()) and
# r in 2:4, plus every shape of nested_golden_grid("os"), for both codings
# (latinize FALSE / TRUE). It writes one row per shape that
# onestage_admitted() refuses, with the columns
#   t,nrows,r,latinize,predicted_work,in_os_grid
# (predicted_work from onestage_predicted_work(); in_os_grid TRUE when the
# shape is in the os grid) and prints the work cap and the number of rows.
# No os shape may be listed (spec 7): the script exits with status 1 when
# one is. [library] defaults to ~/fieldhub-libs/dev; install with
# `R CMD INSTALL . --preclean` first.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop("Usage: Rscript tools/onestage-admission-list.R <out.csv> [library]")
}
library_path <- if (length(arguments) == 2L) arguments[2] else "~/fieldhub-libs/dev"
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
ns <- asNamespace("FielDHub")

thin <- function(x) if (length(x) <= 4L) x else x[unique(round(seq(1, length(x), length.out = 4)))]
divisors <- function(t, from) {
  d <- seq.int(from, t %/% 2L)
  d[t %% d == 0L]
}
universe <- do.call(rbind, lapply(c(20, 30, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000),
                                  function(t) expand.grid(t = t, nrows = thin(divisors(t, 2L)), r = 2:4)))
os <- nested_golden_grid("os")
os <- os[!duplicated(os[c("t", "k", "r")]), c("t", "k", "r")]
names(os) <- c("t", "nrows", "r")
universe <- rbind(universe, os)
universe[] <- lapply(universe, as.integer)
universe <- universe[!duplicated(universe), ]
os_key <- paste(os$t, os$nrows, os$r)

rows <- list()
for (i in seq_len(nrow(universe))) {
  u <- universe[i, ]
  ncols <- u$t %/% u$nrows
  for (latinize in c(FALSE, TRUE)) {
    if (ns$onestage_admitted(u$t, u$r, u$nrows, ncols, latinize)) next
    rows[[length(rows) + 1L]] <- data.frame(
      t = u$t, nrows = u$nrows, r = u$r, latinize = latinize,
      predicted_work = ns$onestage_predicted_work(u$t, u$r, u$nrows, ncols, latinize),
      in_os_grid = paste(u$t, u$nrows, u$r) %in% os_key)
  }
}
out <- if (length(rows)) do.call(rbind, rows) else
  data.frame(t = integer(), nrows = integer(), r = integer(), latinize = logical(),
             predicted_work = numeric(), in_os_grid = logical())
out <- out[order(out$t * out$r, out$t, out$nrows, out$r, out$latinize), , drop = FALSE]
utils::write.csv(out, arguments[1], row.names = FALSE)
cat(sprintf("work cap %g, admission at %g; %d of %d shape-codings refused, %d in the os grid -> %s\n",
            ns$onestage_work_limit, 0.8 * ns$onestage_work_limit, nrow(out),
            2L * nrow(universe), sum(out$in_os_grid), arguments[1]))
if (any(out$in_os_grid)) {
  cat("FAIL: os grid shapes are not admitted\n")
  quit(status = 1L)
}
