# Add the two row/column parameters to the recorded parameter lists in a golden
# snapshot file, without re-recording any layout (spec-m15 5.4). The snapshots
# are macOS recordings, so snapshot_accept() must never run on another platform:
# this edit changes only the parameter names and appends the two default values.
#   Rscript tools/add-snapshot-parameters.R <snapshot.md> <design> [<design> ...]
# where each <design> is swap_pairs, partially_replicated or multi_location_prep
# (partially_replicated also covers its partially_replicated_fillers entry).
args <- commandArgs(TRUE)
path <- args[1]
selected <- args[-1]
lines <- readLines(path)
new_names <- c("rowcol_penalty", "rowcol_scope")
targets <- c(swap_pairs = "\"seed\"]", partially_replicated = "\"year\"]", multi_location_prep = "\"year\"]")
stopifnot(length(selected) > 0L, all(selected %in% names(targets)))
targets <- targets[selected]
headers <- grep("^# ", lines)
section_of <- function(i) sub("^# (.*) keeps its output$", "\\1", lines[max(headers[headers <= i])])
edits <- 0L
i <- 1L
while (i <= length(lines)) {
  line <- lines[i]
  section <- if (any(headers <= i)) section_of(i) else ""
  base <- sub("_fillers$", "", section)
  is_target <- base %in% names(targets) && grepl("^ *\"value\": \\[\"", line) &&
    endsWith(line, targets[[base]]) && !grepl("rowcol_penalty", line, fixed = TRUE) &&
    grepl(if (base == "swap_pairs") "\"candidate_sample_size\"" else "\"border_penalization\"|\"max_fillers\"", line)
  if (is_target) {
    indent <- nchar(line) - nchar(sub("^ +", "", line))
    lines[i] <- sub("\\]$", paste0(", ", paste0("\"", new_names, "\"", collapse = ", "), "]"), line)
    close <- which(lines == paste0(strrep(" ", indent - 4L), "]") & seq_along(lines) > i)[1]
    stopifnot(!is.na(close), lines[close - 1L] == paste0(strrep(" ", indent - 2L), "}"))
    pad <- strrep(" ", indent - 2L)
    block <- c(paste0(pad, "},"),
               paste0(pad, "{"), paste0(pad, "  \"type\": \"double\","), paste0(pad, "  \"attributes\": {},"),
               paste0(pad, "  \"value\": [0]"), paste0(pad, "},"),
               paste0(pad, "{"), paste0(pad, "  \"type\": \"character\","), paste0(pad, "  \"attributes\": {},"),
               paste0(pad, "  \"value\": [\"pairs\"]"), paste0(pad, "}"))
    lines <- c(lines[seq_len(close - 2L)], block, lines[close:length(lines)])
    edits <- edits + 1L
    i <- close + length(block)
    headers <- grep("^# ", lines)  # the insertion shifted every later line
  } else {
    i <- i + 1L
  }
}
writeLines(lines, path)
cat("edited", edits, "parameter lists in", path, "\n")
