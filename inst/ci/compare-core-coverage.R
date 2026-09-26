core_coverage_summary <- function(lines) {
  if (!is.data.frame(lines) || !all(c("filename", "value") %in% names(lines))) {
    stop("Coverage records must contain filename and value columns.", call. = FALSE)
  }
  filenames <- gsub("\\\\", "/", lines$filename)
  is_core <- grepl("(^|/)R/(fct_|utils_).*[.]R$", filenames)
  values <- lines$value[is_core]
  if (length(values) == 0L) {
    stop("No core coverage records were produced.", call. = FALSE)
  }
  covered <- sum(values > 0, na.rm = TRUE)
  list(
    covered = as.integer(covered),
    total = as.integer(length(values)),
    percent = covered / length(values) * 100
  )
}

measure_core_coverage <- function(path, label) {
  old <- setwd(path)
  on.exit(setwd(old), add = TRUE)
  coverage <- covr::package_coverage(
    quiet = FALSE,
    clean = FALSE,
    install_path = tempfile(paste0("fieldhub-coverage-", label, "-"))
  )
  core_coverage_summary(covr::tally_coverage(coverage, by = "line"))
}

write_step_summary <- function(base, head) {
  summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (!nzchar(summary_file)) return(invisible(NULL))
  delta <- head$percent - base$percent
  text <- sprintf(
    paste0(
      "## Core R coverage\n\n",
      "| Revision | Covered lines | Coverage |\n",
      "|---|---:|---:|\n",
      "| Base | %d / %d | %.2f%% |\n",
      "| Pull request | %d / %d | %.2f%% |\n\n",
      "Change: %+.2f percentage points.\n"
    ),
    base$covered, base$total, base$percent,
    head$covered, head$total, head$percent,
    delta
  )
  cat(text, file = summary_file, append = TRUE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop("Usage: Rscript tools/compare-core-coverage.R <base-dir> <head-dir>",
         call. = FALSE)
  }
  base <- measure_core_coverage(args[1L], "base")
  head <- measure_core_coverage(args[2L], "head")
  write_step_summary(base, head)
  message(sprintf("Base core coverage: %.2f%%", base$percent))
  message(sprintf("Pull-request core coverage: %.2f%%", head$percent))
  if (head$percent + sqrt(.Machine$double.eps) < base$percent) {
    stop(sprintf(
      "Core coverage decreased from %.2f%% to %.2f%%.",
      base$percent, head$percent
    ), call. = FALSE)
  }
}

if (sys.nframe() == 0L) main()
