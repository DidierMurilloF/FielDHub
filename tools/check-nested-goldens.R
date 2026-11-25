# Compares a candidate run with the recorded baseline under the acceptance
# rules of spec 7 and writes <candidate>.md with the failures first. Exits with
# status 1 when any shape fails.
#   Rscript tools/check-nested-goldens.R tools/nested-goldens-baseline.csv.gz candidate.csv [pairs.csv] [accepted-loss.csv] [write-accepted=<out.csv>]
# The first two arguments are raw per-seed records from
# tools/generate-nested-goldens.R, as .csv or .csv.gz; the committed baseline is
# tools/nested-goldens-baseline.csv.gz. pairs.csv holds paired timings from
# tools/time-nested-pairs.R (recognised by its bd_seconds column): the speed
# rule nested_speed() is applied to it and a shape slower than blocksdesign's
# median + 0.05 s fails. accepted-loss.csv lists caller,t,k,r of the shapes
# whose loss the owner accepted (spec 10): they are reported with their
# shortfall and do not fail the run.
# Onestage callers, nested and latinized (phase 2 spec 7, owner decisions
# 2026-11-22): the list is
# keyed by caller,t,k,r,iterations, and a listed row is an accepted loss only
# when nested_acceptance() finds it eligible (accepted_loss_eligible) and its
# speed rule passes. The list gives a row no exemption beyond that rule: a
# listed row that passes every rule outright stays a pass, and a listed row
# that fails a rule and is not eligible fails. With
# write-accepted=<out.csv> the tool writes the eligible rows whose speed rule
# passes (when pairs are given): caller,t,k,r,iterations,noninferior_failed
# and the candidate mean less the baseline mean of Column D, Column A, joint D
# and joint A. tools/onestage-accepted-loss.csv is made from these files only.
# The optimum rule is gated on the recorded baseline bound (phase 2 spec 6.4):
# a candidate seed is a hit when A >= the baseline's recorded bound - 1.5e-7.
# The A-Bound the candidate reports itself never gates.
# Shapes are matched by caller, t, k, r and, where a shape was run at several
# iteration counts, by iterations (a shape recorded at one count is matched
# with its paired timing at that count); the candidate and the pairs must be complete
# runs of the baseline's grid. In a partial run that holds one iteration count
# of such a shape, that count is not kept, and the lone variant is compared
# with the default-iterations baseline.
# The table has one column per rule, including fallback_ok (the onestage
# callers of protocol os) and clash_ok (onestage_latinized only); each is TRUE
# for every other caller.
arguments <- commandArgs(trailingOnly = TRUE)
write_flag <- startsWith(arguments, "write-accepted=")
write_accepted <- sub("^write-accepted=", "", arguments[write_flag])
arguments <- arguments[!write_flag]
if (length(arguments) < 2L || length(arguments) > 4L || length(write_accepted) > 1L ||
    any(!nzchar(write_accepted))) {
  stop("Usage: Rscript tools/check-nested-goldens.R baseline.csv.gz candidate.csv [pairs.csv] [accepted-loss.csv] [write-accepted=<out.csv>]")
}
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(script_dir, "nested-goldens-helpers.R"))
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
speed_fine <- rep(TRUE, nrow(res))
speed_lines <- character(0)
if (!is.null(pairs)) {
  speed <- nested_speed(pairs)
  j <- match(nested_shape_key(res), nested_shape_key(speed))
  # nested_summary() keeps `iterations` only for a shape recorded at several
  # counts, and the timings carry the count they ran with (the os shapes with
  # t >= 300 run at iterations = 20 only). A shape that the comparison keys
  # without `iterations` and that was timed at exactly one count is matched
  # with that timing.
  speed_base <- paste(speed$caller, speed$t, speed$k, speed$r, sep = "|")
  single <- !speed_base %in% speed_base[duplicated(speed_base)]
  lone <- is.na(j) & is.na(res$iterations)
  j[lone] <- match(paste(res$caller, res$t, res$k, res$r, sep = "|")[lone],
                   ifelse(single, speed_base, NA))
  res$bd_median <- speed$bd_median[j]
  res$engine_median <- speed$engine_median[j]
  res$speed_ok <- speed$speed_ok[j]
  speed_fine <- !is.na(res$speed_ok) & res$speed_ok
  # an accepted loss of an onestage caller still fails on speed
  slow_listed <- is.na(res$pass) & res$caller %in% nested_onestage_callers & !speed_fine
  res$pass <- ifelse(is.na(res$pass), ifelse(slow_listed, FALSE, NA), res$pass & speed_fine)
  res$accepted_loss[slow_listed] <- FALSE
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
if (length(write_accepted) == 1L) {
  listable <- res[res$accepted_loss_eligible & speed_fine, ]
  i <- match(nested_shape_key(listable), nested_shape_key(baseline))
  j <- match(nested_shape_key(listable), nested_shape_key(candidate))
  out <- listable[c("caller", "t", "k", "r", "iterations", "noninferior_failed")]
  for (e in c("col_D", "col_A", "joint_D", "joint_A")) {
    column <- paste0(e, "_mean")
    out[[paste0(e, "_diff")]] <- candidate[[column]][j] - baseline[[column]][i]
  }
  utils::write.csv(out, write_accepted, row.names = FALSE)
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
