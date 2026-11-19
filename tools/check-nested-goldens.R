# Compares a candidate run with the recorded baseline under the acceptance
# rules of spec 7 and writes <candidate>.md with the failures first. Exits with
# status 1 when any shape fails.
#   Rscript tools/check-nested-goldens.R baseline.csv candidate.csv [accepted-loss.csv]
# Both CSVs come from tools/generate-nested-goldens.R. accepted-loss.csv lists
# caller,t,k,r of the shapes whose loss the owner accepted (spec 10): they are
# reported with their shortfall and do not fail the run.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 2L || length(arguments) > 3L) {
  stop("Usage: Rscript tools/check-nested-goldens.R baseline.csv candidate.csv [accepted-loss.csv]")
}
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
baseline <- nested_summary(utils::read.csv(arguments[1], stringsAsFactors = FALSE))
candidate <- nested_summary(utils::read.csv(arguments[2], stringsAsFactors = FALSE))
accepted <- if (length(arguments) == 3L) utils::read.csv(arguments[3], stringsAsFactors = FALSE)
res <- nested_acceptance(baseline, candidate, accepted_loss = accepted)
res$gap <- mapply(nested_gap, res$caller, res$t, res$r)
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
            "", "Wall time at the largest shape of each caller (reported only):", times, "")
writeLines(c(header, table), paste0(arguments[2], ".md"))
cat(header, sep = "\n")
if (any(failed)) quit(status = 1L)
