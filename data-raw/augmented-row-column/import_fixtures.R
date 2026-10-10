# Regenerates tests/testthat/fixtures/augmented-row-column/ (contraction and
# layout CSVs plus manifest.csv) for the augmented row-column tests.
#
# Run from the repository root:
#   FIELDHUB_ARC_SUPPLEMENT_ZIP=/path/to/bimj70182-sup-0003-data.zip \
#     Rscript data-raw/augmented-row-column/import_fixtures.R
#
# Inputs: the tracked CSV literals in data-raw/augmented-row-column/inputs/ and
# the Piepho and Williams (2026) supplement archive (SHA-256 verified below).
# readxl is needed here only; it is not a package dependency. Nothing is read
# from research/, and nothing from R/: the expansion routine below is this
# script's own, so the production expander stays independently testable.

stopifnot(file.exists("DESCRIPTION"), dir.exists("data-raw/augmented-row-column"))

supplement_sha256 <- "ee6f945f4deff3f32878a846c8e400c2c50e0ba0a4a99b8940b1b26f83adfcda"
supplement_url <- "https://onlinelibrary.wiley.com/doi/10.1002/bimj70182"
inputs_dir <- file.path("data-raw", "augmented-row-column", "inputs")
output_dir <- file.path("tests", "testthat", "fixtures", "augmented-row-column")

test_path <- function(...) file.path("tests", "testthat", ...)
source(test_path("helper-arc-oracle.R"))

# ---- supplement archive ----------------------------------------------------

zip_path <- Sys.getenv("FIELDHUB_ARC_SUPPLEMENT_ZIP")
if (!nzchar(zip_path) || !file.exists(zip_path)) {
  stop("Set FIELDHUB_ARC_SUPPLEMENT_ZIP to the supplement archive (", supplement_url, ").")
}
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(list(hash = digest::digest(path, algo = "sha256", file = TRUE), method = "digest"))
  }
  out <- suppressWarnings(tryCatch(
    system2("sha256sum", shQuote(path), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  ))
  if (length(out) == 1L && grepl("^[0-9a-f]{64}", out)) {
    return(list(hash = substr(out, 1L, 64L), method = "sha256sum"))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    hash <- as.character(openssl::sha256(file(path, "rb")))
    return(list(hash = hash, method = "openssl"))
  }
  stop("No SHA-256 tool available (digest, sha256sum or openssl).")
}
checksum <- file_sha256(zip_path)
message("SHA-256 via ", checksum$method, ": ", checksum$hash)
if (!identical(tolower(checksum$hash), supplement_sha256)) {
  stop("Supplement SHA-256 mismatch: expected ", supplement_sha256, ", got ", checksum$hash)
}
if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is required for the import.")
unpack_dir <- tempfile("arc-supplement-")
dir.create(unpack_dir)
on.exit(unlink(unpack_dir, recursive = TRUE), add = TRUE)
utils::unzip(zip_path, exdir = unpack_dir)
xlsx_files <- sort(list.files(unpack_dir, pattern = "^con\\(.*\\)\\.xlsx$", full.names = TRUE))

# ---- helpers ---------------------------------------------------------------

read_input <- function(name) {
  values <- utils::read.csv(file.path(inputs_dir, name), comment.char = "#", check.names = FALSE)
  matrix <- as.matrix(values)
  storage.mode(matrix) <- "integer"
  dimnames(matrix) <- NULL
  matrix
}

# Contraction -> layout codes: tests 1..T fill the empty cells column-major;
# check i (code T + i) sits at (l, j) wherever contraction[i, j] == l.
expand_contraction <- function(contraction, v) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  n_tests <- (v - k) * s
  layout <- matrix(0L, v, s)
  for (i in seq_len(k)) {
    for (j in seq_len(s)) {
      layout[contraction[i, j], j] <- as.integer(n_tests + i)
    }
  }
  layout[layout == 0L] <- seq_len(n_tests)
  layout
}

is_binary <- function(x, margin) {
  all(apply(x, margin, function(z) !anyDuplicated(z)))
}

design_mode <- function(contraction, v) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  counts <- tabulate(contraction, nbins = v)
  balanced <- all(counts %in% c(floor(k * s / v), ceiling(k * s / v))) && all(counts >= 1L)
  if (is_binary(contraction, 1L) && is_binary(contraction, 2L) && balanced) {
    "restricted"
  } else {
    "column_binary"
  }
}

write_design <- function(matrix, file, comment) {
  lines <- c(
    paste0("# ", comment),
    paste(paste0("column_", seq_len(ncol(matrix))), collapse = ","),
    apply(matrix, 1L, paste, collapse = ",")
  )
  con <- file(file.path(output_dir, file), open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
}

num <- function(x) if (is.na(x)) NA_character_ else format(x, digits = 15)

manifest_columns <- c(
  "file", "kind", "v", "s", "k", "mode", "source", "licence",
  "pub_E_aug", "pub_E_aug_dec", "pub_Cbar_v", "pub_Cbar_v_dec",
  "pub_Cbar_s", "pub_Cbar_s_dec", "pub_A_tt", "pub_A_tt_dec",
  "pub_A_ct", "pub_A_ct_dec", "ref_connected", "ref_df_formula",
  "ref_residual_df_rank", "ref_E_aug", "ref_V_tt", "ref_V_tc", "ref_V_cc",
  "ref_max_tt", "ref_max_tc"
)
manifest <- list()

# `pub` is a named list: E_aug, Cbar_v, Cbar_s, A_tt, A_ct, each c(value, decimals)
add_design <- function(file, matrix, kind, v, source, licence, pub = list(), mode = NULL, oracle_codes = NULL) {
  k <- if (kind == "contraction") nrow(matrix) else NA_integer_
  s <- ncol(matrix)
  layout <- if (kind == "contraction") expand_contraction(matrix, v) else matrix
  if (kind == "layout") k <- length(unique(layout[duplicated(as.vector(layout))]))
  ref <- arc_oracle(layout)
  if (is.null(mode)) mode <- if (kind == "contraction") design_mode(matrix, v) else "layout"
  write_design(matrix, file, paste0("source: ", source, "; licence: ", licence))
  field <- function(name, part) if (is.null(pub[[name]])) NA_character_ else num(pub[[name]][part])
  row <- c(
    file = file, kind = kind, v = v, s = s, k = k, mode = mode,
    source = source, licence = licence,
    pub_E_aug = field("E_aug", 1), pub_E_aug_dec = field("E_aug", 2),
    pub_Cbar_v = field("Cbar_v", 1), pub_Cbar_v_dec = field("Cbar_v", 2),
    pub_Cbar_s = field("Cbar_s", 1), pub_Cbar_s_dec = field("Cbar_s", 2),
    pub_A_tt = field("A_tt", 1), pub_A_tt_dec = field("A_tt", 2),
    pub_A_ct = field("A_ct", 1), pub_A_ct_dec = field("A_ct", 2),
    ref_connected = as.character(ref$connected),
    ref_df_formula = if (kind == "contraction") as.character(k * s - k - v - s + 2) else NA_character_,
    ref_residual_df_rank = as.character(ref$residual_df_rank),
    ref_E_aug = num(if (ref$connected) ref$E_aug else NA_real_),
    ref_V_tt = num(if (ref$connected) ref$V_tt else NA_real_),
    ref_V_tc = num(if (ref$connected) ref$V_tc else NA_real_),
    ref_V_cc = num(if (ref$connected) ref$V_cc else NA_real_),
    ref_max_tt = num(if (ref$connected) ref$max_tt else NA_real_),
    ref_max_tc = num(if (ref$connected) ref$max_tc else NA_real_)
  )
  manifest[[file]] <<- row
  invisible(ref)
}

# ---- published values ------------------------------------------------------

# Piepho and Williams (2026), Table 1: k, v, s, r_bar, E_con (= Cbar_v), Cbar_s,
# E_dual, E_aug. E_con and Cbar_s have 4 decimals, E_aug 6.
table1 <- utils::read.table(text = "
3 12 8 2 0.5739 0.4828 0.4828 0.388112
3 15 10 2 0.5359 0.4424 0.4467 0.368217
3 18 12 2 0.5135 0.4176 0.4205 0.356396
4 16 8 2 0.6618 0.5385 0.5385 0.450683
4 16 12 3 0.7547 0.7097 0.7097 0.560000
4 18 9 2 0.6479 0.5111 0.5111 0.441030
4 20 10 2 0.6423 0.5000 0.5000 0.437095
4 20 15 3 0.7339 0.6825 0.6825 0.549752
4 22 11 2 0.6338 0.4851 0.4851 0.431698
4 24 12 2 0.6310 0.4793 0.4793 0.429763
4 24 18 3 0.7203 0.6652 0.6652 0.543467
4 26 13 2 0.6232 0.4688 0.4688 0.425538
5 20 8 2 0.6976 0.5453 0.5453 0.480081
5 20 12 3 0.7881 0.7201 0.7213 0.590627
5 20 16 4 0.8244 0.7993 0.8000 0.646791
5 25 10 2 0.6966 0.5294 0.5294 0.476348
5 25 15 3 0.7780 0.6992 0.7000 0.584213
5 25 20 4 0.8096 0.7801 0.7808 0.640252
5 30 12 2 0.6867 0.5038 0.5038 0.468846
5 30 18 3 0.7669 0.6805 0.6814 0.578506
5 30 24 4 0.7988 0.7661 0.7665 0.635813",
  col.names = c("k", "v", "s", "r", "Econ", "Cs", "Edual", "Eaug"))

pw_source <- "Piepho & Williams (2026) Biometrical Journal 68(5):e70182"
pw_licence <- "CC-BY"
supplement_note <- "Supporting File 3 bimj70182-sup-0003-Data.zip"

# ---- supplement contractions ----------------------------------------------

example2_literal <- read_input("example2-contraction.csv")
supplement_rows <- 0L
matched <- integer()
for (path in xlsx_files) {
  d <- as.data.frame(readxl::read_excel(path))
  d <- data.frame(row = d[["row!"]], column = d[["column!"]], treatment = d[["treatment!"]])
  stopifnot(!anyNA(d), all(d == round(d)))
  grid <- matrix(NA_integer_, max(d$row), max(d$column))
  stopifnot(nrow(d) == length(grid), !anyDuplicated(d[c("row", "column")]))
  grid[cbind(d$row, d$column)] <- as.integer(d$treatment)
  stopifnot(!anyNA(grid))
  if (nrow(grid) > ncol(grid)) grid <- t(grid) # long dimension is the s columns
  stopifnot(nrow(grid) < ncol(grid))
  v <- max(grid)
  k <- nrow(grid)
  s <- ncol(grid)
  stopifnot(is_binary(grid, 1L), is_binary(grid, 2L))
  supplement_rows <- supplement_rows + 1L
  source <- paste0(pw_source, ", ", supplement_note, ", ", basename(path))
  index <- which(table1$v == v & table1$s == s & table1$k == k)
  if (length(index) == 1L) {
    matched <- c(matched, index)
    pub <- list(E_aug = c(table1$Eaug[index], 6L),
                Cbar_v = c(table1$Econ[index], 4L),
                Cbar_s = c(table1$Cs[index], 4L))
    stopifnot(table1$r[index] * v == k * s)
    ref <- add_design(sprintf("table1-v%d-s%d-k%d.csv", v, s, k), grid, "contraction", v,
                      paste0("supplement: ", source), pw_licence, pub)
    stopifnot(abs(ref$E_aug - table1$Eaug[index]) <= 5e-7)
  } else if (length(index) == 0L && v == 24 && s == 16 && k == 5) {
    stopifnot(identical(unname(grid), example2_literal))
    pub <- list(E_aug = c(0.6031, 4L), Cbar_v = c(0.7749, 4L), Cbar_s = c(0.7332, 4L))
    ref <- add_design("example2-contraction.csv", grid, "contraction", v,
                      paste0("supplement: ", source), pw_licence, pub)
    stopifnot(abs(ref$E_aug - 0.6031) <= 5e-5)
  } else {
    stop("Supplement file ", basename(path), " matches no Table 1 row: v=", v, " s=", s, " k=", k)
  }
}
stopifnot(supplement_rows == 22L, setequal(matched, seq_len(nrow(table1))), length(matched) == 21L)

# ---- printed examples (Example 2 contraction came from the supplement) ----

example1 <- read_input("example1-contraction.csv")
layout1 <- read_input("example1-layout.csv")
layout2 <- read_input("example2-layout.csv")
stopifnot(identical(expand_contraction(example1, 12L), layout1),
          identical(expand_contraction(example2_literal, 24L), layout2))
ex_source <- paste0(pw_source, ", Section 2")
ref <- add_design("example1-contraction.csv", example1, "contraction", 12L, ex_source, pw_licence,
                  list(E_aug = c(0.3881, 4L), Cbar_v = c(0.5739, 4L), Cbar_s = c(0.4828, 4L)))
stopifnot(abs(ref$E_aug - 0.3881) <= 5e-5)
add_design("example1-layout.csv", layout1, "layout", 12L, paste0(ex_source, ", printed layout"), pw_licence,
           list(E_aug = c(0.3881, 4L)))
add_design("example2-layout.csv", layout2, "layout", 24L, paste0(ex_source, ", printed layout"), pw_licence,
           list(E_aug = c(0.6031, 4L)))
ref <- arc_oracle(layout2)
stopifnot(abs(ref$E_aug - 0.6031) <= 5e-5)

# ---- project-internal designs ----------------------------------------------

internal <- list(
  list("restricted-12x8-a.csv", 12L, "independent specification review, 2026-10-10 (restricted 12x8 pair, A)"),
  list("restricted-12x8-b.csv", 12L, "independent specification review, 2026-10-10 (restricted 12x8 pair, B)"),
  list("square-a.csv", 6L, "mathematical audit report (column-binary square pair, A)"),
  list("square-b.csv", 6L, "mathematical audit report (column-binary square pair, B)"),
  list("recoloring-counterexample.csv", 6L, "implementation-plan audit (recoloring counterexample)"),
  list("cyclic-5x5-k4.csv", 5L, "specification review (cyclic 5x5, k = 4)"),
  list("zero-df.csv", 6L, "mathematical audit (zero-df design)"),
  list("false-qr.csv", 54L, "M1 spike 2b (false-QR design)")
)
refs <- list()
for (item in internal) {
  design <- read_input(item[[1]])
  refs[[item[[1]]]] <- add_design(item[[1]], design, "contraction", item[[2]], item[[3]], "project-internal")
}
stopifnot(
  !refs[["false-qr.csv"]]$connected,
  5 * 14 - 5 - 54 - 14 + 2 == -1,
  refs[["zero-df.csv"]]$connected, refs[["zero-df.csv"]]$residual_df_rank == 0,
  refs[["recoloring-counterexample.csv"]]$connected,
  abs(refs[["recoloring-counterexample.csv"]]$E_aug - 1 / 6) <= 1e-10,
  refs[["recoloring-counterexample.csv"]]$residual_df_rank == 1
)

# ---- Bailey and Haines (2025) cyclic squares --------------------------------

blocks <- utils::read.csv(file.path(inputs_dir, "bailey-haines-blocks.csv"), comment.char = "#",
                          check.names = FALSE, stringsAsFactors = FALSE)
bh_pub <- list("C(3,4,5)" = c(4.0341, 2.0910), "C(1,4,7)" = c(4.0363, 2.0921),
               "C(1,5,6)" = c(4.5607, 2.3519), "C(1,1,10)" = c(5.0013, 2.5701))
for (i in seq_len(nrow(blocks))) {
  t <- blocks$t[i]
  block <- unlist(blocks[i, c("block_1", "block_2", "block_3")], use.names = FALSE)
  n_tests <- t * (t - length(block))
  layout <- matrix(0L, t, t)
  for (control in seq_along(block)) {
    for (j in 0:(t - 1L)) layout[j + 1L, ((block[control] + j) %% t) + 1L] <- as.integer(n_tests + control)
  }
  layout[layout == 0L] <- seq_len(n_tests)
  name <- blocks$name[i]
  pub <- list()
  if (!is.null(bh_pub[[name]])) pub <- list(A_tt = c(bh_pub[[name]][1], 4L), A_ct = c(bh_pub[[name]][2], 4L))
  file <- tolower(paste0("bailey-haines-", gsub("[(),]+", "-", sub("\\)$", "", name)), ".csv"))
  ref <- add_design(file, layout, "layout", t, "Bailey & Haines (2025) JABES, Table 1", "CC-BY", pub)
  if (length(pub)) {
    stopifnot(ref$connected, abs(ref$V_tt - pub$A_tt[1]) <= 5e-5, abs(ref$V_tc - pub$A_ct[1]) <= 5e-5,
              abs(ref$V_cc - 1 / 6) <= 1e-10)
  } else {
    stopifnot(!ref$connected)
  }
}

# ---- manifest ----------------------------------------------------------------

table <- do.call(rbind, manifest)
table <- table[order(table[, "file"], method = "radix"), manifest_columns, drop = FALSE]
quote_field <- function(x) ifelse(is.na(x), "NA", paste0("\"", gsub("\"", "\"\"", x), "\""))
text_columns <- c("file", "kind", "mode", "source", "licence")
lines <- paste(manifest_columns, collapse = ",")
for (i in seq_len(nrow(table))) {
  fields <- vapply(manifest_columns, function(column) {
    value <- table[i, column]
    if (column %in% text_columns) quote_field(value) else if (is.na(value)) "NA" else value
  }, character(1))
  lines <- c(lines, paste(fields, collapse = ","))
}
con <- file(file.path(output_dir, "manifest.csv"), open = "wb")
writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
close(con)
message("Wrote ", nrow(table), " manifest rows to ", output_dir)
