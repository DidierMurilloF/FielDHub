# Compares a candidate run with the recorded baseline under the acceptance
# rules of spec 7 and writes <candidate>.md with the failures first. Exits with
# status 1 when any shape fails.
#   Rscript tools/check-nested-goldens.R tools/nested-goldens-baseline.csv.gz candidate.csv [pairs.csv] [accepted-loss.csv]
# The first two arguments are raw per-seed records from
# tools/generate-nested-goldens.R, as .csv or .csv.gz; the committed baseline is
# tools/nested-goldens-baseline.csv.gz. pairs.csv holds paired timings from
# tools/time-nested-pairs.R (recognised by its bd_seconds column): the speed
# rule nested_speed() is applied to it and a shape slower than blocksdesign's
# median + 0.05 s fails. accepted-loss.csv lists caller,t,k,r of the shapes
# whose loss the owner accepted (spec 10): they are reported with their
# shortfall and do not fail the run.
# Shapes are matched by caller, t, k, r and, where a shape was run at several
# iteration counts, by iterations; the candidate and the pairs must be complete
# runs of the baseline's grid. In a partial run that holds one iteration count
# of such a shape, that count is not kept, and the lone variant is compared
# with the default-iterations baseline.
# The table has one column per rule, including fallback_ok and clash_ok (the
# onestage callers of protocol os; TRUE for every other caller).
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 2L || length(arguments) > 4L) {
  stop("Usage: Rscript tools/check-nested-goldens.R baseline.csv.gz candidate.csv [pairs.csv] [accepted-loss.csv]")
}
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
baseline <- nested_summary(utils::read.csv(arguments[1], stringsAsFactors = FALSE))
candidate <- nested_summary(utils::read.csv(arguments[2], stringsAsFactors = FALSE))
pairs <- NULL
accepted <- NULL
for (extra in arguments[-(1:2)]) {
  x <- utils::read.csv(extra, stringsAsFactors = FALSE)
  if ("bd_seconds" %in% names(x)) pairs <- x else accepted <- x
}
res <- nested_acceptance(baseline, candidate, accepted_loss = accepted)
res$gap <- mapply(nested_gap, res$caller, res$t, res$r)
speed_lines <- character(0)
if (!is.null(pairs)) {
  speed <- nested_speed(pairs)
  j <- match(nested_shape_key(res), nested_shape_key(speed))
  res$bd_median <- speed$bd_median[j]
  res$engine_median <- speed$engine_median[j]
  res$speed_ok <- speed$speed_ok[j]
  res$pass <- ifelse(is.na(res$pass), NA, res$pass & !is.na(res$speed_ok) & res$speed_ok)
  slow <- speed[!speed$speed_ok, ]
  speed_lines <- c(sprintf("Speed rule (paired timing): %d of %d shapes timed, %d slower than blocksdesign + 0.05 s.",
                           sum(!is.na(j)), nrow(res), nrow(slow)),
                   if (nrow(slow) > 0L) sprintf("- slow: %s (t = %d, k = %d, r = %d%s): engine %.3f s, blocksdesign %.3f s",
                                                slow$caller, slow$t, slow$k, slow$r,
                                                ifelse(is.na(slow$iterations), "",
                                                       paste0(", iterations = ", slow$iterations)),
                                                slow$engine_median, slow$bd_median),
                   "")
}
failed <- !is.na(res$pass) & !res$pass
res <- res[order(!failed, is.na(res$pass), res$caller, res$t, res$k, res$r), ]

# Wall time at the largest shape of each caller (reported only)
largest <- do.call(rbind, lapply(split(res, res$caller), function(x) x[which.max(x$t * x$r), ]))
times <- sprintf("- %s (t = %d, k = %d, r = %d): candidate / baseline = %.2f", largest$caller,
                 largest$t, largest$k, largest$r, largest$time_ratio)
table <- c(paste("|", paste(names(res), collapse = " | "), "|"),
           paste("|", paste(rep("---", ncol(res)), collapse = " | "), "|"),
           apply(format(res, digits = 4), 1, function(r) paste("|", paste(trimws(r), collapse = " | "), "|")))
header <- c("# Nested golden comparison",
            sprintf("%d shapes: %d pass, %d fail, %d accepted loss.", nrow(res),
                    sum(res$pass, na.rm = TRUE), sum(failed), sum(is.na(res$pass))),
            "", speed_lines,
            "Wall time at the largest shape of each caller (reported only):", times, "")
writeLines(c(header, table), paste0(arguments[2], ".md"))
cat(header, sep = "\n")
if (any(failed)) quit(status = 1L)
