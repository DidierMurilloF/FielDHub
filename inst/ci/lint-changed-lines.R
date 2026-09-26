changed_lines_from_patch <- function(patch) {
  if (length(patch) == 0L) return(integer())

  changed <- integer()
  new_line <- NA_integer_
  for (line in patch) {
    if (startsWith(line, "@@")) {
      match <- regexec(
        "^@@ -[0-9]+(?:,[0-9]+)? \\+([0-9]+)(?:,[0-9]+)? @@",
        line,
        perl = TRUE
      )
      groups <- regmatches(line, match)[[1L]]
      new_line <- if (length(groups) == 2L) as.integer(groups[2L]) else NA_integer_
      next
    }
    if (is.na(new_line) || startsWith(line, "\\ No newline")) next

    prefix <- substr(line, 1L, 1L)
    if (identical(prefix, "+")) {
      changed <- c(changed, new_line)
      new_line <- new_line + 1L
    } else if (!identical(prefix, "-")) {
      new_line <- new_line + 1L
    }
  }
  unique(changed)
}

git_output <- function(args) {
  output <- system2("git", shQuote(args), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  output
}

lint_changed_lines <- function(base, head) {
  files <- git_output(c(
    "diff", "--name-only", "--diff-filter=ACMRT", base, head,
    "--", "*.R", "*.Rmd"
  ))
  files <- files[file.exists(files)]
  if (length(files) == 0L) return(structure(list(), class = "lints"))

  pkgload::load_all(".", quiet = TRUE)
  linters <- list(
    seq_linter = lintr::seq_linter(),
    object_usage_linter = lintr::object_usage_linter(),
    vector_logic_linter = lintr::vector_logic_linter(),
    T_and_F_symbol_linter = lintr::T_and_F_symbol_linter()
  )

  findings <- list()
  for (file in files) {
    patch <- git_output(c("diff", "--unified=0", base, head, "--", file))
    changed <- changed_lines_from_patch(patch)
    if (length(changed) == 0L) next

    file_lints <- lintr::lint(file, linters = linters)
    keep <- vapply(
      file_lints,
      function(finding) finding$line_number %in% changed,
      logical(1)
    )
    findings <- c(findings, file_lints[keep])
  }
  structure(findings, class = "lints")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop("Usage: Rscript tools/lint-changed-lines.R <base-sha> <head-sha>",
         call. = FALSE)
  }
  findings <- lint_changed_lines(args[1L], args[2L])
  print(findings)
  if (length(findings) > 0L) quit(status = 1L)
}

if (sys.nframe() == 0L) main()
