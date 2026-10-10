# Augmented row-column designs: engine tests (fast suite; exhaustive blocks run
# only when FIELDHUB_ARC_EXHAUSTIVE=true). Research M1 specification, sections
# 3-7 and 10. Reference values come from Piepho and Williams (2026), Biometrical
# Journal 68(5):e70182 (CC-BY), and Bailey and Haines (2025), JABES (CC-BY).

# Task 4: structural checks (spec 4.2) and expansion (spec 3) ----------------

# Printed Example 1 (v = 12, k = 3, s = 8) and Example 2 (v = 24, k = 5,
# s = 16) of Piepho and Williams (2026): contractions and expanded layouts.
arc_ex1_contraction <- matrix(c(
  3, 7, 9, 1, 10, 8, 2, 6,
  12, 10, 4, 3, 11, 9, 5, 2,
  7, 5, 6, 4, 1, 11, 8, 12
), nrow = 3, byrow = TRUE)
arc_ex1_layout <- matrix(as.integer(c(
  1, 10, 19, 73, 75, 46, 55, 64,
  2, 11, 20, 28, 37, 47, 73, 74,
  73, 12, 21, 74, 38, 48, 56, 65,
  3, 13, 74, 75, 39, 49, 57, 66,
  4, 75, 22, 29, 40, 50, 74, 67,
  5, 14, 75, 30, 41, 51, 58, 73,
  75, 73, 23, 31, 42, 52, 59, 68,
  6, 15, 24, 32, 43, 73, 75, 69,
  7, 16, 73, 33, 44, 74, 60, 70,
  8, 74, 25, 34, 73, 53, 61, 71,
  9, 17, 26, 35, 74, 75, 62, 72,
  74, 18, 27, 36, 45, 54, 63, 75
)), nrow = 12, byrow = TRUE)
arc_ex2_contraction <- matrix(c(
  13, 20, 7, 8, 19, 15, 9, 23, 11, 2, 3, 18, 10, 24, 16, 1,
  6, 15, 9, 10, 12, 1, 14, 18, 23, 17, 5, 3, 21, 16, 22, 8,
  21, 4, 19, 22, 15, 5, 11, 2, 17, 9, 8, 14, 24, 6, 13, 18,
  20, 9, 5, 23, 21, 10, 13, 6, 7, 24, 17, 15, 4, 8, 18, 12,
  1, 22, 6, 19, 16, 2, 10, 4, 21, 12, 20, 7, 3, 14, 17, 11
), nrow = 5, byrow = TRUE)
arc_ex2_layout <- matrix(as.integer(c(
  309, 20, 39, 58, 77, 306, 115, 134, 153, 172, 191, 210, 229, 248, 267, 305,
  1, 21, 40, 59, 78, 309, 116, 307, 154, 305, 192, 211, 230, 249, 268, 286,
  2, 22, 41, 60, 79, 96, 117, 135, 155, 173, 305, 306, 309, 250, 269, 287,
  3, 307, 42, 61, 80, 97, 118, 309, 156, 174, 193, 212, 308, 251, 270, 288,
  4, 23, 308, 62, 81, 307, 119, 136, 157, 175, 306, 213, 231, 252, 271, 289,
  306, 24, 309, 63, 82, 98, 120, 308, 158, 176, 194, 214, 232, 307, 272, 290,
  5, 25, 305, 64, 83, 99, 121, 137, 308, 177, 195, 309, 233, 253, 273, 291,
  6, 26, 43, 305, 84, 100, 122, 138, 159, 178, 307, 215, 234, 308, 274, 306,
  7, 308, 306, 65, 85, 101, 305, 139, 160, 307, 196, 216, 235, 254, 275, 292,
  8, 27, 44, 306, 86, 308, 309, 140, 161, 179, 197, 217, 305, 255, 276, 293,
  9, 28, 45, 66, 87, 102, 307, 141, 305, 180, 198, 218, 236, 256, 277, 309,
  10, 29, 46, 67, 306, 103, 123, 142, 162, 309, 199, 219, 237, 257, 278, 308,
  305, 30, 47, 68, 88, 104, 308, 143, 163, 181, 200, 220, 238, 258, 307, 294,
  11, 31, 48, 69, 89, 105, 306, 144, 164, 182, 201, 307, 239, 309, 279, 295,
  12, 306, 49, 70, 307, 305, 124, 145, 165, 183, 202, 308, 240, 259, 280, 296,
  13, 32, 50, 71, 309, 106, 125, 146, 166, 184, 203, 221, 241, 306, 305, 297,
  14, 33, 51, 72, 90, 107, 126, 147, 307, 306, 308, 222, 242, 260, 309, 298,
  15, 34, 52, 73, 91, 108, 127, 306, 167, 185, 204, 305, 243, 261, 308, 307,
  16, 35, 307, 309, 305, 109, 128, 148, 168, 186, 205, 223, 244, 262, 281, 299,
  308, 305, 53, 74, 92, 110, 129, 149, 169, 187, 309, 224, 245, 263, 282, 300,
  307, 36, 54, 75, 308, 111, 130, 150, 309, 188, 206, 225, 306, 264, 283, 301,
  17, 309, 55, 307, 93, 112, 131, 151, 170, 189, 207, 226, 246, 265, 306, 302,
  18, 37, 56, 308, 94, 113, 132, 305, 306, 190, 208, 227, 247, 266, 284, 303,
  19, 38, 57, 76, 95, 114, 133, 152, 171, 308, 209, 228, 307, 305, 285, 304
)), nrow = 24, byrow = TRUE)

# Balanced (r_h = 3) with row repeats: column-binary only. v = 6, k = 3, s = 6.
arc_row_repeat_contraction <- matrix(c(
  6, 2, 3, 3, 5, 1,
  1, 5, 2, 6, 1, 2,
  4, 4, 6, 5, 4, 3
), nrow = 3, byrow = TRUE)
# Audit counterexample: column-binary, r = (4, 2, 2, 2, 1, 1), v = 6.
arc_unbalanced_contraction <- matrix(c(
  1, 4, 4, 1,
  2, 1, 2, 5,
  3, 3, 1, 6
), nrow = 3, byrow = TRUE)
# No collisions or row repeats, but r = (3, 2, 2, 3, 1, 1) with v = 6 where
# ks / v = 2: only the restricted replication rule fails.
arc_unbalanced_rowbinary <- matrix(c(
  1, 2, 3, 4,
  2, 1, 4, 5,
  3, 4, 1, 6
), nrow = 3, byrow = TRUE)

expect_arc_error <- function(expr, specific, parent) {
  condition <- tryCatch(expr, error = function(e) e)
  expect_s3_class(condition, specific)
  expect_s3_class(condition, parent)
  expect_identical(class(condition)[1:2], c(specific, parent))
  invisible(condition)
}

test_that("valid printed contractions pass the structural checks", {
  ok1 <- arc_check_contraction(arc_ex1_contraction, 12, "restricted")
  expect_identical(ok1$k, 3L)
  expect_identical(ok1$s, 8L)
  expect_identical(ok1$v, 12L)
  expect_identical(ok1$r, rep(2L, 12))
  expect_identical(ok1$mode, "restricted")
  ok2 <- arc_check_contraction(arc_ex2_contraction, 24, "restricted")
  expect_identical(sort(unique(ok2$r)), c(3L, 4L))
  expect_silent(arc_check_contraction(arc_ex1_contraction, 12L,
                                      "column_binary"))
})

test_that("rule 1: malformed contraction, v, or mode is an input error", {
  input <- c("fieldhub_arc_input_error", "fieldhub_input_error")
  with_na <- arc_ex1_contraction
  with_na[2, 3] <- NA
  non_integral <- arc_ex1_contraction
  non_integral[1, 1] <- 2.5
  expect_error(arc_check_contraction(with_na, 12, "restricted"),
               class = "fieldhub_arc_input_error")
  expect_arc_error(arc_check_contraction(with_na, 12, "restricted"),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(non_integral, 12, "restricted"),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(as.vector(arc_ex1_contraction), 12,
                                         "restricted"), input[1], input[2])
  character_matrix <- arc_ex1_contraction
  storage.mode(character_matrix) <- "character"
  expect_arc_error(arc_check_contraction(character_matrix, 12, "restricted"),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, 12.5,
                                         "restricted"), input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, c(12, 13),
                                         "restricted"), input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, NA_real_,
                                         "restricted"), input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, 12),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, 12, "binary"),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, 12, NA),
                   input[1], input[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction, 12,
                                         c("restricted", "column_binary")),
                   input[1], input[2])
})

test_that("rule 2: k and s outside 2 <= k < v, s >= 2 are dimension errors", {
  dimension <- c("fieldhub_arc_dimension_error", "fieldhub_dimension_error")
  expect_error(arc_check_contraction(arc_ex1_contraction[1, , drop = FALSE],
                                     12, "restricted"),
               class = "fieldhub_arc_dimension_error")
  expect_arc_error(arc_check_contraction(arc_ex1_contraction[1, , drop = FALSE],
                                         12, "restricted"),
                   dimension[1], dimension[2])
  expect_arc_error(arc_check_contraction(arc_ex1_contraction[, 1, drop = FALSE],
                                         12, "column_binary"),
                   dimension[1], dimension[2])
  # Three checks but only three field rows, so k < v fails.
  expect_arc_error(arc_check_contraction(matrix(c(1, 2, 3, 2, 3, 1), 3), 3,
                                         "restricted"),
                   dimension[1], dimension[2])
})

test_that("rule 3: s > v is rejected in both modes", {
  wide <- matrix(c(1, 2, 2, 3, 3, 1, 1, 2), nrow = 2)
  for (mode in c("restricted", "column_binary")) {
    expect_error(arc_check_contraction(wide, 3, mode),
                 class = "fieldhub_arc_dimension_error")
    expect_arc_error(arc_check_contraction(wide, 3, mode),
                     "fieldhub_arc_dimension_error", "fieldhub_dimension_error")
  }
})

test_that("rule 4: entries outside 1..v are input errors", {
  too_large <- arc_ex1_contraction
  too_large[1, 1] <- 13
  zero <- arc_ex1_contraction
  zero[3, 8] <- 0
  expect_error(arc_check_contraction(too_large, 12, "restricted"),
               class = "fieldhub_arc_input_error")
  expect_arc_error(arc_check_contraction(too_large, 12, "restricted"),
                   "fieldhub_arc_input_error", "fieldhub_input_error")
  expect_arc_error(arc_check_contraction(zero, 12, "column_binary"),
                   "fieldhub_arc_input_error", "fieldhub_input_error")
})

test_that("rule 5: a column repeat is a collision, reported before rule 7", {
  # Column 1 becomes (3, 7, 7): a collision, and r_7 = 3, r_12 = 1 breaks
  # the restricted replication rule too. Row 2 has no other 7, so rule 6
  # does not apply.
  collided <- arc_ex1_contraction
  collided[2, 1] <- 7
  for (mode in c("restricted", "column_binary")) {
    expect_error(arc_check_contraction(collided, 12, mode),
                 class = "fieldhub_arc_collision_error")
    condition <- expect_arc_error(arc_check_contraction(collided, 12, mode),
                                  "fieldhub_arc_collision_error",
                                  "fieldhub_input_error")
    expect_identical(condition$column, 1L)
  }
  # Expansion rejects the collision before writing anything.
  expect_error(arc_expand(collided, 12, "restricted"),
               class = "fieldhub_arc_collision_error")
})

test_that("rule 6: restricted rejects a row repeat column-binary accepts", {
  expect_error(arc_check_contraction(arc_row_repeat_contraction, 6,
                                     "restricted"),
               class = "fieldhub_arc_row_repeat_error")
  condition <- expect_arc_error(
    arc_check_contraction(arc_row_repeat_contraction, 6, "restricted"),
    "fieldhub_arc_row_repeat_error", "fieldhub_input_error")
  expect_identical(condition$row, 1L)
  ok <- arc_check_contraction(arc_row_repeat_contraction, 6, "column_binary")
  expect_identical(ok$r, rep(3L, 6))
  expect_identical(ok$mode, "column_binary")
})

test_that("rule 7: replication failures are replication errors", {
  replication <- c("fieldhub_arc_replication_error", "fieldhub_input_error")
  # r = (3, 2, 2, 3, 1, 1) is unbalanced: restricted rejects, column-binary
  # accepts.
  expect_error(arc_check_contraction(arc_unbalanced_rowbinary, 6, "restricted"),
               class = "fieldhub_arc_replication_error")
  expect_arc_error(arc_check_contraction(arc_unbalanced_rowbinary, 6,
                                         "restricted"),
                   replication[1], replication[2])
  expect_identical(
    arc_check_contraction(arc_unbalanced_rowbinary, 6, "column_binary")$r,
    c(3L, 2L, 2L, 3L, 1L, 1L))
  # A pseudo-treatment that never occurs (r_6 = 0) fails in both modes.
  missing_six <- matrix(c(1, 2, 3, 4, 5, 1), nrow = 2, byrow = TRUE)
  for (mode in c("restricted", "column_binary")) {
    expect_arc_error(arc_check_contraction(missing_six, 6, mode),
                     replication[1], replication[2])
  }
})

test_that("the unbalanced audit design expands only in column-binary mode", {
  expanded <- arc_expand(arc_unbalanced_contraction, 6, "column_binary")
  expect_identical(dim(expanded$codes), c(6L, 4L))
  expect_type(expanded$codes, "integer")
  expect_null(expanded$labels)
  n_tests <- (6L - 3L) * 4L
  expect_identical(sort(expanded$codes[expanded$codes <= n_tests]),
                   seq_len(n_tests))
  expect_identical(as.vector(table(expanded$codes[expanded$codes > n_tests])),
                   c(4L, 4L, 4L))
  # Field row h holds r_h checks.
  expect_identical(unname(rowSums(expanded$codes > n_tests)),
                   c(4, 2, 2, 2, 1, 1))
  # Restricted mode: row 1 (1 4 4 1) repeats, and rule 6 precedes rule 7, so
  # the row-repeat class is reported although r is also unbalanced.
  expect_error(arc_expand(arc_unbalanced_contraction, 6, "restricted"),
               class = "fieldhub_arc_row_repeat_error")
  # Without row repeats, an unbalanced r is a replication error in
  # restricted mode.
  expect_error(arc_expand(arc_unbalanced_rowbinary, 6, "restricted"),
               class = "fieldhub_arc_replication_error")
})

test_that("Examples 1 and 2 expand to exactly the printed layouts", {
  ex1 <- arc_expand(arc_ex1_contraction, 12, "restricted")
  expect_identical(ex1$codes, arc_ex1_layout)
  expect_null(ex1$labels)
  ex2 <- arc_expand(arc_ex2_contraction, 24, "restricted")
  expect_identical(ex2$codes, arc_ex2_layout)
  storage.mode(arc_ex1_contraction) <- "integer"
  expect_identical(arc_expand(arc_ex1_contraction, 12L, "column_binary")$codes,
                   arc_ex1_layout)
})

# v = 6, k = 2, s = 3, so (v - k)s = 12 tests.
arc_small_contraction <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
arc_small_checks <- c("Check A", "Línea ñ ü")
arc_small_tests <- c("line-01 (x)", " lead", "trail ", "a,b;c", "tab\there",
                     "Quote\"d", "back\\slash", "été #7", "UPPER",
                     "upper", "x/y", "中文")

test_that("labels round-trip byte-for-byte", {
  expanded <- arc_expand(arc_small_contraction, 6, "restricted",
                         check_labels = arc_small_checks,
                         test_labels = arc_small_tests)
  codes <- expanded$codes
  expect_type(expanded$labels, "character")
  expect_identical(dim(expanded$labels), c(6L, 3L))
  expected <- matrix(c(arc_small_tests, arc_small_checks)[codes], 6, 3)
  expect_identical(expanded$labels, expected)
  expect_setequal(as.vector(expanded$labels[codes > 12L]), arc_small_checks)
  expect_setequal(as.vector(expanded$labels[codes <= 12L]), arc_small_tests)
  check_two <- expanded$labels[codes == 14L][1]
  expect_identical(check_two, arc_small_checks[2])
  expect_identical(charToRaw(check_two), charToRaw(arc_small_checks[2]))
  expect_identical(Encoding(check_two), Encoding(arc_small_checks[2]))
  expect_identical(charToRaw(expanded$labels[codes == 1L]),
                   charToRaw("line-01 (x)"))
  expect_identical(expanded$labels[codes == 2L], " lead")
  expect_identical(expanded$labels[codes == 3L], "trail ")
})

test_that("invalid label vectors are input errors", {
  input <- c("fieldhub_arc_input_error", "fieldhub_input_error")
  expand_labels <- function(check_labels, test_labels) {
    arc_expand(arc_small_contraction, 6, "restricted",
               check_labels = check_labels, test_labels = test_labels)
  }
  # Test-label count other than (v - k)s = 12, and check count other than k.
  expect_error(expand_labels(arc_small_checks, arc_small_tests[-1]),
               class = "fieldhub_arc_input_error")
  expect_arc_error(expand_labels(arc_small_checks, arc_small_tests[-1]),
                   input[1], input[2])
  expect_arc_error(expand_labels(arc_small_checks, c(arc_small_tests, "extra")),
                   input[1], input[2])
  expect_arc_error(expand_labels(c(arc_small_checks, "Check C"),
                                 arc_small_tests), input[1], input[2])
  # Duplicates within checks and within tests.
  expect_arc_error(expand_labels(c("Check A", "Check A"), arc_small_tests),
                   input[1], input[2])
  duplicated_tests <- arc_small_tests
  duplicated_tests[12] <- duplicated_tests[1]
  expect_arc_error(expand_labels(arc_small_checks, duplicated_tests),
                   input[1], input[2])
  # A label shared by a check and a test.
  shared <- arc_small_tests
  shared[5] <- "Check A"
  expect_arc_error(expand_labels(arc_small_checks, shared), input[1], input[2])
  # Only one of the two label vectors.
  expect_arc_error(expand_labels(arc_small_checks, NULL), input[1], input[2])
  expect_arc_error(expand_labels(NULL, arc_small_tests), input[1], input[2])
  # Non-character and missing labels.
  expect_arc_error(expand_labels(c(1, 2), arc_small_tests), input[1], input[2])
  expect_arc_error(expand_labels(c("Check A", NA), arc_small_tests),
                   input[1], input[2])
})

# Task 5: connectivity, df, and recoloring (spec 4.2, 4.3, 5) ----------------

# Preserved false-QR design of spike 2b (v = 54, s = 14, k = 5): balanced
# (r = 1 or 2) and row-binary, with df_formula = -1.
arc_false_qr_contraction <- matrix(c(
  37, 6, 46, 17, 49, 53, 47, 32, 8, 9, 11, 19, 45, 28,
  48, 4, 2, 52, 18, 31, 6, 10, 54, 5, 51, 24, 7, 37,
  26, 19, 44, 42, 30, 14, 13, 35, 27, 25, 12, 16, 33, 18,
  3, 20, 47, 48, 29, 21, 35, 43, 45, 24, 22, 44, 38, 15,
  36, 42, 50, 34, 53, 30, 40, 22, 39, 12, 3, 41, 1, 23
), nrow = 5, byrow = TRUE)

# Whether `recolored` is row-binary and keeps every column set of `original`
# and the replication vector.
arc_recoloring_ok <- function(recolored, original, v) {
  identical(dim(recolored), dim(original)) &&
    all(apply(recolored, 1L, anyDuplicated) == 0L) &&
    all(vapply(seq_len(ncol(original)), function(j) {
      setequal(recolored[, j], original[, j]) &&
        length(recolored[, j]) == length(unique(recolored[, j]))
    }, logical(1))) &&
    identical(tabulate(recolored, nbins = v), tabulate(original, nbins = v))
}

test_that("the spectrum classifier applies both thresholds at the boundary", {
  cases <- list(
    list(x = 0, connected = FALSE, near = FALSE),
    list(x = 0.5e-8, connected = FALSE, near = FALSE),
    list(x = 1e-8, connected = TRUE, near = TRUE),
    list(x = 2e-8, connected = TRUE, near = TRUE),
    list(x = 0.5e-6, connected = TRUE, near = TRUE),
    list(x = 1e-6, connected = TRUE, near = FALSE),
    list(x = 2e-6, connected = TRUE, near = FALSE)
  )
  for (case in cases) {
    # Any order is accepted.
    for (lambda in list(c(case$x, 0.3, 1), c(1, case$x, 0.3))) {
      out <- arc_classify_spectrum(lambda)
      info <- paste("lambda_min_rel =", case$x)
      expect_identical(names(out), c("connected", "lambda_min_rel",
                                     "near_singular"), info = info)
      expect_identical(out$connected, case$connected, info = info)
      expect_identical(out$near_singular, case$near, info = info)
      expect_identical(out$lambda_min_rel, case$x, info = info)
    }
  }
  # The ratio is relative to the largest eigenvalue.
  scaled <- arc_classify_spectrum(c(2e-8, 0.6, 2))
  expect_equal(scaled$lambda_min_rel, 1e-8)
  expect_true(scaled$connected)
  expect_silent(arc_classify_spectrum(c(0, 0.3, 1)))
  # Custom thresholds are honoured with the same boundary semantics.
  custom <- arc_classify_spectrum(c(0.01, 0.3, 1), tau_rank = 0.01,
                                  tau_warn = 0.1)
  expect_true(custom$connected)
  expect_true(custom$near_singular)
})

test_that("the df formula is integer and -1 for the false-QR design", {
  expect_identical(arc_df_formula(54, 14, 5), -1L)
  expect_identical(arc_df_formula(12, 8, 3), 3L)
  expect_identical(arc_df_formula(24, 16, 5), 37L)
})

test_that("the false-QR design is disconnected by the M-tilde spectrum", {
  # Row-binary and balanced, so restricted mode applies.
  checked <- arc_check_contraction(arc_false_qr_contraction, 54, "restricted")
  expect_identical(sort(unique(checked$r)), c(1L, 2L))
  # The df check is bypassed here: the classifier alone must say no.
  mats <- arc_contraction_matrices(arc_false_qr_contraction, 54)
  lambda <- eigen(mats$M_tilde, symmetric = TRUE, only.values = TRUE)$values
  verdict <- arc_classify_spectrum(lambda)
  expect_false(verdict$connected)
  expect_false(verdict$near_singular)
  expect_lt(verdict$lambda_min_rel, 1e-12)
})

test_that("Example 1 is connected and well conditioned", {
  arc_check_contraction(arc_ex1_contraction, 12, "restricted")
  mats <- arc_contraction_matrices(arc_ex1_contraction, 12)
  lambda <- eigen(mats$M_tilde, symmetric = TRUE, only.values = TRUE)$values
  verdict <- arc_classify_spectrum(lambda)
  expect_true(verdict$connected)
  expect_false(verdict$near_singular)
})

test_that("contraction matrices have the null space spanned by Q0", {
  v <- 12
  s <- 8
  k <- 3
  mats <- arc_contraction_matrices(arc_ex1_contraction, v)
  expect_identical(dim(mats$nc), c(12L, 8L))
  expect_identical(dim(mats$nr), c(12L, 3L))
  expect_true(all(mats$nc %in% c(0, 1)))
  expect_identical(as.vector(colSums(mats$nc)), rep(k, s))
  expect_identical(as.vector(rowSums(mats$nr)), as.numeric(mats$r))
  expect_identical(as.integer(mats$r), rep(2L, 12))
  expect_equal(mats$w, mats$nr %*% t(mats$nr))
  expect_identical(dim(mats$M), c(20L, 20L))
  expect_identical(dim(mats$Q0), c(20L, 2L))
  expect_true(isSymmetric(mats$M))
  expect_true(isSymmetric(mats$M_tilde))
  expect_equal(mats$M_tilde, mats$M + mats$Q0 %*% t(mats$Q0))
  expect_equal(crossprod(mats$Q0), diag(2))
  ev <- eigen(mats$M, symmetric = TRUE, only.values = TRUE)$values
  rel <- abs(ev) / max(ev)
  expect_identical(sum(rel < 1e-12), 2L)
  expect_gt(sort(rel)[3], 1e-8)
  ones_rows <- c(rep(1, v), rep(0, s))
  ones_cols <- c(rep(0, v), rep(1, s))
  expect_lt(max(abs(mats$M %*% ones_rows)), 1e-12)
  expect_lt(max(abs(mats$M %*% ones_cols)), 1e-12)
  ev_tilde <- eigen(mats$M_tilde, symmetric = TRUE, only.values = TRUE)$values
  expect_gt(min(ev_tilde), 1e-8 * max(ev_tilde))
})

test_that("all 85 fixed-replication 6 x 4 column multisets recolor", {
  v <- 6L
  s <- 4L
  k <- 3L
  subsets <- utils::combn(v, k, simplify = FALSE)
  multisets <- list()
  extend <- function(start, chosen, counts) {
    if (length(chosen) == s) {
      if (all(counts == 2L)) multisets[[length(multisets) + 1L]] <<- chosen
      return(invisible(NULL))
    }
    for (i in start:length(subsets)) {
      next_counts <- counts
      next_counts[subsets[[i]]] <- next_counts[subsets[[i]]] + 1L
      if (all(next_counts <= 2L)) extend(i, c(chosen, i), next_counts)
    }
  }
  extend(1L, integer(0), integer(v))
  expect_identical(length(multisets), 85L)
  recolored_ok <- 0L
  for (chosen in multisets) {
    columns <- do.call(cbind, subsets[chosen])
    recolored <- arc_recolor_rows(columns, v)
    if (arc_recoloring_ok(recolored, columns, v)) {
      recolored_ok <- recolored_ok + 1L
    }
  }
  expect_identical(recolored_ok, 85L)
})

test_that("random column-binary designs recolor with sets and r unchanged", {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  if (had_seed) saved <- get(".Random.seed", envir = global, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", saved, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  })
  set.seed(20261010)
  # Random fill, then random swaps until no column repeats a label.
  random_column_binary <- function(k, s, r) {
    repeat {
      m <- matrix(sample(rep(seq_along(r), r)), k, s)
      for (iteration in seq_len(2000L)) {
        bad <- which(apply(m, 2L, anyDuplicated) > 0L)
        if (length(bad) == 0L) return(m)
        j <- bad[1L]
        i <- anyDuplicated(m[, j])
        i2 <- sample(k, 1L)
        j2 <- sample(s, 1L)
        held <- m[i, j]
        m[i, j] <- m[i2, j2]
        m[i2, j2] <- held
      }
    }
  }
  parameter_sets <- list(
    list(v = 24L, s = 16L, k = 5L, r = rep(c(4L, 3L), c(8L, 16L))),
    list(v = 7L, s = 5L, k = 3L, r = c(3L, 3L, 2L, 2L, 2L, 2L, 1L)),
    list(v = 10L, s = 6L, k = 4L, r = c(4L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L,
                                        1L)),
    list(v = 12L, s = 12L, k = 4L, r = rep(4L, 12L)),
    list(v = 30L, s = 24L, k = 5L, r = rep(4L, 30L))
  )
  per_set <- 80L
  generated <- 0L
  recolored_ok <- 0L
  row_repeats_seen <- 0L
  for (p in parameter_sets) {
    for (draw in seq_len(per_set)) {
      columns <- random_column_binary(p$k, p$s, p$r)
      generated <- generated + 1L
      if (any(apply(columns, 1L, anyDuplicated) > 0L)) {
        row_repeats_seen <- row_repeats_seen + 1L
      }
      recolored <- arc_recolor_rows(columns, p$v)
      if (arc_recoloring_ok(recolored, columns, p$v)) {
        recolored_ok <- recolored_ok + 1L
      }
    }
  }
  expect_identical(generated, 400L)
  expect_gte(generated, 300L)
  expect_identical(recolored_ok, generated)
  # The inputs genuinely needed recoloring.
  expect_gt(row_repeats_seen, 0L)
})

test_that("recoloring is deterministic and leaves row-binary input valid", {
  first <- arc_recolor_rows(arc_row_repeat_contraction, 6)
  expect_identical(arc_recolor_rows(arc_row_repeat_contraction, 6), first)
  expect_true(arc_recoloring_ok(first, arc_row_repeat_contraction, 6))
  expect_type(first, "integer")
  ex1 <- arc_recolor_rows(arc_ex1_contraction, 12)
  expect_true(arc_recoloring_ok(ex1, arc_ex1_contraction, 12))
})

test_that("recoloring needs max(r) <= k and s <= v", {
  condition <- expect_arc_error(
    arc_recolor_rows(arc_unbalanced_contraction, 6),
    "fieldhub_arc_recolor_error", "fieldhub_input_error")
  expect_identical(condition$r, c(4L, 2L, 2L, 2L, 1L, 1L))
  expect_identical(condition$k, 3L)
  expect_error(arc_recolor_rows(arc_unbalanced_contraction, 6),
               class = "fieldhub_arc_recolor_error")
  wide <- matrix(c(1, 2, 2, 3, 3, 1, 1, 2), nrow = 2)
  expect_arc_error(arc_recolor_rows(wide, 3), "fieldhub_arc_dimension_error",
                   "fieldhub_dimension_error")
  # Column collisions are rejected by the structural checks first.
  collided <- arc_ex1_contraction
  collided[2, 1] <- 7
  expect_error(arc_recolor_rows(collided, 12),
               class = "fieldhub_arc_collision_error")
})

# Task 6: public validator (spec 4.1, 4.3, 4.4) -------------------------------

# A restricted 6 x 4, k = 3 design with every r_h = 2 and df_formula = 1 that
# is disconnected: columns 1-2 hold field rows 1-3 and columns 3-4 hold field
# rows 4-6, so the row-column graph splits into two components. Found by
# recoloring the 85 fixed-replication 6 x 4 column multisets (10 of them are
# disconnected) and taking the first disconnected one.
arc_disconnected_contraction <- matrix(c(
  3, 1, 6, 4,
  1, 2, 4, 5,
  2, 3, 5, 6
), nrow = 3, byrow = TRUE)

test_that("validator: malformed input raises the structural condition", {
  collided <- arc_ex1_contraction
  collided[2, 1] <- 7
  expect_arc_error(arc_validate_contraction(collided, 12),
                   "fieldhub_arc_collision_error", "fieldhub_input_error")
})

test_that("validator: negative df is infeasible without matrix work", {
  condition <- expect_arc_error(
    arc_validate_contraction(arc_fixture("false-qr.csv"), 54),
    "fieldhub_arc_infeasible_error", "fieldhub_dimension_error")
  expect_identical(condition$df_formula, -1L)
  expect_true(is.na(condition$lambda_min_rel))
  expect_identical(c(condition$v, condition$s, condition$k), c(54L, 14L, 5L))
  expect_match(condition$message, "connected design")
})

test_that("validator: a df-feasible disconnected design is rejected", {
  # Independent check with the test oracle on the expanded layout.
  codes <- arc_expand(arc_disconnected_contraction, 6,
                      mode = "restricted")$codes
  expect_false(arc_oracle(codes)$connected)
  expect_identical(arc_df_formula(6, 4, 3), 1L)

  condition <- expect_arc_error(
    arc_validate_contraction(arc_disconnected_contraction, 6),
    "fieldhub_arc_disconnected_error", "fieldhub_input_error")
  expect_identical(condition$df_formula, 1L)
  expect_lt(condition$lambda_min_rel, 1e-8)
  expect_identical(c(condition$v, condition$s, condition$k), c(6L, 4L, 3L))
})

test_that("validator: a connected design with zero df is a df error", {
  condition <- expect_arc_error(
    arc_validate_contraction(arc_fixture("zero-df.csv"), 6),
    "fieldhub_arc_df_error", "fieldhub_dimension_error")
  expect_identical(condition$df_formula, 0L)
  expect_gt(condition$lambda_min_rel, 1e-6)
  expect_identical(c(condition$v, condition$s, condition$k), c(6L, 6L, 2L))
})

test_that("validator: low df warns with the df and how to raise it", {
  contraction <- arc_fixture("example1-contraction.csv")
  expect_warning(result <- arc_validate_contraction(contraction, 12),
                 class = "fieldhub_arc_low_df_warning")
  expect_identical(result$df_formula, 3L)
  expect_true(result$connected)

  condition <- tryCatch(arc_validate_contraction(contraction, 12),
                        fieldhub_arc_low_df_warning = function(w) w)
  expect_s3_class(condition, "fieldhub_warning")
  expect_identical(condition$df_formula, 3L)
  expect_match(condition$message, "3", fixed = TRUE)
  expect_match(condition$message, "about 82%", fixed = TRUE)
  expect_match(condition$message, "adding a check", fixed = TRUE)
  expect_match(condition$message, "rows or columns", fixed = TRUE)
  expect_no_match(condition$message, "optimi|search|longer|iteration",
                  ignore.case = TRUE)
})

test_that("validator: a healthy design returns diagnostics without warning", {
  expect_no_warning(
    result <- arc_validate_contraction(arc_fixture("example2-contraction.csv"),
                                       24)
  )
  expect_identical(result$df_formula, 37L)
  expect_true(result$connected)
  expect_gt(result$lambda_min_rel, 1e-6)
  expect_identical(c(result$v, result$s, result$k), c(24L, 16L, 5L))
  # ks / v = 80 / 24, so every r_h is 3 or 4 and they sum to 80.
  expect_true(all(result$r %in% c(3L, 4L)))
  expect_identical(sum(result$r), 80L)
  expect_length(result$r, 24L)
  expect_identical(result$mode, "restricted")
})

test_that("validator: near-singular spectra warn, others do not", {
  contraction <- arc_fixture("example2-contraction.csv")
  local({
    local_mocked_bindings(arc_classify_spectrum = function(lambda, ...) {
      list(connected = TRUE, lambda_min_rel = 5e-7, near_singular = TRUE)
    })
    expect_warning(result <- arc_validate_contraction(contraction, 24),
                   class = "fieldhub_arc_near_singular_warning")
    expect_identical(result$lambda_min_rel, 5e-7)
  })
  local({
    local_mocked_bindings(arc_classify_spectrum = function(lambda, ...) {
      list(connected = TRUE, lambda_min_rel = 2e-6, near_singular = FALSE)
    })
    expect_no_warning(arc_validate_contraction(contraction, 24),
                      class = "fieldhub_arc_near_singular_warning")
  })
})

test_that("validator: mode passes through to the structural checks", {
  square <- arc_fixture("square-a.csv")
  expect_arc_error(arc_validate_contraction(square, 6),
                   "fieldhub_arc_row_repeat_error", "fieldhub_input_error")
  # Column-binary mode reaches the df stage: connected with df 5 (manifest).
  expect_warning(result <- arc_validate_contraction(square, 6,
                                                    mode = "column_binary"),
                 class = "fieldhub_arc_low_df_warning")
  expect_true(result$connected)
  expect_identical(result$df_formula, 5L)
  expect_identical(result$mode, "column_binary")
})

# Task 7: full-layout evaluator (spec 7.1) ------------------------------------

# Every fixture of the manifest with its field layout: "layout" files are the
# layout itself; contractions are expanded with the manifest mode.
arc_layout_manifest <- function() {
  utils::read.csv(
    test_path("fixtures", "augmented-row-column", "manifest.csv"),
    stringsAsFactors = FALSE
  )
}

arc_manifest_codes <- function(entry) {
  fixture <- arc_fixture(entry$file)
  if (identical(entry$kind, "layout")) {
    return(fixture)
  }
  arc_expand(fixture, entry$v, mode = entry$mode)$codes
}

arc_objective_names <- c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc")

test_that("layout evaluator reproduces every manifest reference", {
  manifest <- arc_layout_manifest()
  for (i in seq_len(nrow(manifest))) {
    entry <- manifest[i, ]
    result <- arc_evaluate_layout(arc_manifest_codes(entry))
    label <- entry$file
    expect_identical(result$connected, entry$ref_connected, label = label)
    expect_equal(result$residual_df_rank, entry$ref_residual_df_rank,
                 tolerance = 0, label = label)
    if (entry$ref_connected) {
      for (name in arc_objective_names) {
        expect_lt(abs(result[[name]] - entry[[paste0("ref_", name)]]), 1e-10,
                  label = paste(label, name))
      }
    } else {
      for (name in c("cefs", arc_objective_names)) {
        expect_null(result[[name]], label = paste(label, name))
      }
    }
  }
})

test_that("layout evaluator agrees with the independent oracle", {
  manifest <- arc_layout_manifest()
  subset <- c("example1-layout.csv", "example1-contraction.csv",
              "example2-layout.csv", "example2-contraction.csv",
              "restricted-12x8-a.csv", "restricted-12x8-b.csv", "zero-df.csv",
              "square-a.csv", "square-b.csv", "recoloring-counterexample.csv",
              "cyclic-5x5-k4.csv", "bailey-haines-c-4-4-4.csv")
  expect_true(all(subset %in% manifest$file))
  for (file in subset) {
    codes <- arc_manifest_codes(manifest[manifest$file == file, ])
    result <- arc_evaluate_layout(codes)
    reference <- arc_oracle(codes)
    expect_identical(result$connected, reference$connected, label = file)
    expect_equal(result$residual_df_rank, reference$residual_df_rank,
                 tolerance = 0, label = file)
    if (reference$connected) {
      for (name in arc_objective_names) {
        expect_lt(abs(result[[name]] - reference[[name]]), 1e-10,
                  label = paste(file, name))
      }
      expect_length(result$cefs, length(unique(as.vector(codes))) - 1L)
      expect_identical(result$rank_C, length(result$cefs))
    }
  }
})

test_that("Bailey-Haines C(4,4,4) is disconnected and gets no objectives", {
  result <- arc_evaluate_layout(arc_fixture("bailey-haines-c-4-4-4.csv"))
  expect_false(result$connected)
  expect_equal(result$residual_df_rank, 14, tolerance = 0)
  expect_identical(names(result),
                   c("connected", "rank_C", "residual_df_rank"))
  for (name in c("cefs", arc_objective_names)) {
    expect_null(result[[name]])
  }
})

test_that("the false-QR layout is disconnected with zero rank-based df", {
  codes <- arc_expand(arc_fixture("false-qr.csv"), 54, "restricted")$codes
  result <- arc_evaluate_layout(codes)
  expect_false(result$connected)
  expect_equal(result$residual_df_rank, 0, tolerance = 0)
  expect_null(result$E_aug)
})

test_that("a transposed Example 1 layout gives identical objectives", {
  codes <- arc_fixture("example1-layout.csv")
  original <- arc_evaluate_layout(codes)
  transposed <- arc_evaluate_layout(t(codes))
  expect_true(transposed$connected)
  for (name in arc_objective_names) {
    expect_lt(abs(transposed[[name]] - original[[name]]), 1e-10, label = name)
  }
})

test_that("integral doubles and explicit check codes are accepted", {
  codes <- arc_fixture("example1-layout.csv")
  as_integer <- arc_evaluate_layout(codes)
  as_double <- arc_evaluate_layout(codes + 0, check_codes = c(73, 74, 75))
  expect_identical(as_double$connected, TRUE)
  for (name in arc_objective_names) {
    expect_lt(abs(as_double[[name]] - as_integer[[name]]), 1e-12, label = name)
  }
})

test_that("malformed layouts raise fieldhub_arc_layout_error", {
  layout <- c("fieldhub_arc_layout_error", "fieldhub_input_error")
  codes <- arc_fixture("example1-layout.csv")

  # Test code 2 also replaces test 1 at cell (1, 1), so code 2 occurs twice.
  duplicated <- codes
  duplicated[1, 1] <- 2L
  expect_arc_error(arc_evaluate_layout(duplicated, check_codes = 73:75),
                   layout[1], layout[2])
  # With check_codes = NULL a duplicated code is by definition a check (spec
  # 7.1 inference rule), so the same matrix is a valid layout with four
  # checks (2, 73, 74, 75) and 70 tests (v* = 74). It is evaluated without
  # error and matches the oracle, which applies the same rule.
  inferred <- arc_evaluate_layout(duplicated)
  reference <- arc_oracle(duplicated)
  expect_identical(inferred$connected, reference$connected)
  expect_length(inferred$cefs, 73L)
  for (name in arc_objective_names) {
    expect_lt(abs(inferred[[name]] - reference[[name]]), 1e-10, label = name)
  }

  # Exactly one check: keep check 73 and turn every 74 and 75 into new tests.
  one_check <- codes
  others <- which(one_check %in% c(74L, 75L))
  one_check[others] <- 100L + seq_along(others)
  expect_arc_error(arc_evaluate_layout(one_check), layout[1], layout[2])
  expect_arc_error(arc_evaluate_layout(one_check, check_codes = 73L),
                   layout[1], layout[2])

  with_na <- codes
  with_na[2, 3] <- NA_integer_
  expect_arc_error(arc_evaluate_layout(with_na), layout[1], layout[2])
  expect_arc_error(arc_evaluate_layout(codes + 0.5), layout[1], layout[2])
  expect_arc_error(arc_evaluate_layout(as.vector(codes)), layout[1],
                   layout[2])
  expect_arc_error(arc_evaluate_layout(codes, check_codes = c(73, 74, 999)),
                   layout[1], layout[2])
  expect_arc_error(arc_evaluate_layout(codes, check_codes = c(73, NA)),
                   layout[1], layout[2])
})

test_that("layouts above the plot cap raise before evaluation", {
  codes <- arc_fixture("example1-layout.csv")
  before <- getOption("FielDHub.arc_oracle_max_plots")
  local({
    old <- options(FielDHub.arc_oracle_max_plots = 50)
    on.exit(options(old), add = TRUE)
    condition <- expect_arc_error(arc_evaluate_layout(codes),
                                  "fieldhub_arc_size_error",
                                  "fieldhub_dimension_error")
    expect_identical(condition$n, 96L)
    expect_identical(condition$cap, 50)
  })
  expect_identical(getOption("FielDHub.arc_oracle_max_plots"), before)
  expect_true(arc_evaluate_layout(codes)$connected)
})

# Task 8: paper-route contraction evaluator (spec 7.2) ------------------------

# Contraction fixtures of the manifest that are connected (every contraction
# fixture except the false-QR design).
arc_connected_contractions <- function() {
  manifest <- arc_layout_manifest()
  manifest[manifest$kind == "contraction" & manifest$ref_connected, ]
}

arc_published_tol <- function(decimals) 0.5 * 10^(-decimals) + 1e-12

arc_paper_fields <- c("Cbar_v", "Cbar_s", "E_aug", "a", "b")

test_that("paper route reproduces the published Examples 1 and 2", {
  manifest <- arc_layout_manifest()
  for (name in c("example1-contraction.csv", "example2-contraction.csv")) {
    entry <- manifest[manifest$file == name, ]
    result <- arc_evaluate_contraction(arc_fixture(name), entry$v, entry$mode)
    expect_true(result$connected, label = name)
    expect_identical(result$df_formula, as.integer(entry$ref_df_formula),
                     label = name)
    expect_lt(abs(result$Cbar_v - entry$pub_Cbar_v),
              arc_published_tol(entry$pub_Cbar_v_dec), label = name)
    expect_lt(abs(result$Cbar_s - entry$pub_Cbar_s),
              arc_published_tol(entry$pub_Cbar_s_dec), label = name)
    expect_lt(abs(result$E_aug - entry$ref_E_aug), 1e-10, label = name)
  }
  example1 <- arc_evaluate_contraction(arc_ex1_contraction, 12, "restricted")
  expect_lt(abs(example1$Cbar_v - 0.5739), 5e-5 + 1e-12)
  expect_lt(abs(example1$Cbar_s - 0.4828), 5e-5 + 1e-12)
})

test_that("paper route reproduces all 21 rows of Table 1", {
  manifest <- arc_layout_manifest()
  table1 <- manifest[startsWith(manifest$file, "table1-"), ]
  expect_identical(nrow(table1), 21L)
  for (i in seq_len(nrow(table1))) {
    entry <- table1[i, ]
    label <- entry$file
    result <- arc_evaluate_contraction(arc_fixture(entry$file), entry$v,
                                       entry$mode)
    expect_true(result$connected, label = label)
    expect_lt(abs(result$E_aug - entry$pub_E_aug),
              arc_published_tol(entry$pub_E_aug_dec), label = label)
    expect_lt(abs(result$E_aug - entry$ref_E_aug), 1e-10, label = label)
    expect_lt(abs(result$Cbar_v - entry$pub_Cbar_v),
              arc_published_tol(entry$pub_Cbar_v_dec), label = label)
    expect_lt(abs(result$Cbar_s - entry$pub_Cbar_s),
              arc_published_tol(entry$pub_Cbar_s_dec), label = label)
  }
})

test_that("the trivial eigenvalue is projected out, not dropped as smallest", {
  contraction <- arc_ex1_contraction
  v <- 12
  k <- nrow(contraction)
  s <- ncol(contraction)
  # The corrected eq. (4) bracket, built here without the projection P_s.
  mats <- arc_contraction_matrices(contraction, v)
  m12 <- mats$nc - outer(mats$r, rep(1, s)) / s
  row_block <- diag(mats$r) - mats$w / s + matrix(1, v, v)
  bracket <- diag(s) - crossprod(m12, solve(row_block, m12)) / k
  lambda <- eigen((bracket + t(bracket)) / 2, symmetric = TRUE,
                  only.values = TRUE)$values
  # Its trivial eigenvalue is 1 (eigenvector 1_s), not the smallest one.
  expect_lt(min(abs(lambda - 1)), 1e-10)
  naive <- (s - 1) / sum(1 / lambda[seq_len(s - 1)])
  expect_lt(abs(naive - 0.5600), 5e-5)

  result <- arc_evaluate_contraction(contraction, v, "restricted")
  expect_gt(abs(result$Cbar_s - naive), 1e-3)
  expect_lt(abs(result$Cbar_s - 0.4828), 5e-5 + 1e-12)
})

test_that("paper route agrees with the full-layout evaluator on E_aug", {
  fixtures <- arc_connected_contractions()
  expect_gt(nrow(fixtures), 25L)
  for (i in seq_len(nrow(fixtures))) {
    entry <- fixtures[i, ]
    contraction <- arc_fixture(entry$file)
    paper <- arc_evaluate_contraction(contraction, entry$v, entry$mode)
    layout <- arc_evaluate_layout(
      arc_expand(contraction, entry$v, mode = entry$mode)$codes
    )
    expect_true(paper$connected, label = entry$file)
    expect_lt(abs(paper$E_aug - layout$E_aug), 1e-10, label = entry$file)
    expect_lt(abs(paper$E_aug - entry$ref_E_aug), 1e-10, label = entry$file)
  }
})

test_that("a and b satisfy the 1/E_aug identity, equal or unequal r", {
  fixtures <- arc_connected_contractions()
  for (i in seq_len(nrow(fixtures))) {
    entry <- fixtures[i, ]
    v <- entry$v
    s <- entry$s
    k <- entry$k
    result <- arc_evaluate_contraction(arc_fixture(entry$file), v, entry$mode)
    v_star <- (v - k) * s + k
    lhs <- (v_star - 1) / result$E_aug
    rhs <- v_star - v - s + 1 + s * result$a + v * result$b
    expect_lt(abs(lhs - rhs), 1e-9, label = entry$file)
    # The two halves separately: s a and v b against the eq. (5) terms.
    expect_lt(abs(s * result$a - (v / k) * (v - 1) / result$Cbar_v), 1e-9,
              label = entry$file)
    expect_lt(abs(v * result$b - (v / k) * (s - 1) / result$Cbar_s), 1e-9,
              label = entry$file)
  }
})

test_that("paper route passes the mode through to the structural checks", {
  square <- arc_fixture("square-a.csv")
  expect_arc_error(arc_evaluate_contraction(square, 6, "restricted"),
                   "fieldhub_arc_row_repeat_error", "fieldhub_input_error")
  manifest <- arc_layout_manifest()
  reference <- manifest$ref_E_aug[manifest$file == "square-a.csv"]
  result <- arc_evaluate_contraction(square, 6, "column_binary")
  expect_true(result$connected)
  expect_lt(abs(result$E_aug - reference), 1e-10)
})

test_that("paper route returns early for a negative df", {
  result <- arc_evaluate_contraction(arc_fixture("false-qr.csv"), 54,
                                     "restricted")
  expect_identical(names(result),
                   c("connected", "df_formula", "lambda_min_rel"))
  expect_false(result$connected)
  expect_identical(result$df_formula, -1L)
  expect_identical(result$lambda_min_rel, NA_real_)
  expect_null(result$E_aug)
})

test_that("paper route gives a disconnected design diagnostics only", {
  result <- arc_evaluate_contraction(arc_disconnected_contraction, 6,
                                     "restricted")
  expect_identical(names(result),
                   c("connected", "df_formula", "lambda_min_rel"))
  expect_false(result$connected)
  expect_identical(result$df_formula, 1L)
  expect_true(is.finite(result$lambda_min_rel))
  expect_lt(result$lambda_min_rel, 1e-8)
  for (name in arc_paper_fields) {
    expect_null(result[[name]], label = name)
  }
})

# Task 9: single-matrix evaluator (spec 6, 7.3) ------------------------------

arc_mean_names <- c("E_aug", "V_tt", "V_tc", "V_cc")

test_that("single-matrix route reproduces every connected contraction", {
  fixtures <- arc_connected_contractions()
  expect_gt(nrow(fixtures), 25L)
  for (i in seq_len(nrow(fixtures))) {
    entry <- fixtures[i, ]
    label <- entry$file
    contraction <- arc_fixture(entry$file)
    result <- arc_evaluate_objectives(contraction, entry$v, entry$mode,
                                      worst_pair = TRUE)
    layout <- arc_evaluate_layout(
      arc_expand(contraction, entry$v, mode = entry$mode)$codes
    )
    expect_true(result$connected, label = label)
    expect_identical(result$df_formula, as.integer(entry$ref_df_formula),
                     label = label)
    expect_gte(result$lambda_min_rel, 1e-8, label = label)
    for (name in arc_objective_names) {
      expect_lt(abs(result[[name]] - entry[[paste0("ref_", name)]]), 1e-10,
                label = paste(label, name, "reference"))
      expect_lt(abs(result[[name]] - layout[[name]]), 1e-10,
                label = paste(label, name, "layout"))
    }
  }
})

test_that("single-matrix route agrees with the independent oracle", {
  manifest <- arc_layout_manifest()
  subset <- c("example1-contraction.csv", "example2-contraction.csv",
              "restricted-12x8-a.csv", "restricted-12x8-b.csv", "zero-df.csv",
              "square-a.csv", "square-b.csv", "recoloring-counterexample.csv",
              "cyclic-5x5-k4.csv")
  expect_true(all(subset %in% manifest$file))
  for (file in subset) {
    entry <- manifest[manifest$file == file, ]
    contraction <- arc_fixture(file)
    result <- arc_evaluate_objectives(contraction, entry$v, entry$mode,
                                      worst_pair = TRUE)
    reference <- arc_oracle(
      arc_expand(contraction, entry$v, mode = entry$mode)$codes
    )
    expect_true(reference$connected, label = file)
    expect_identical(result$connected, reference$connected, label = file)
    for (name in arc_objective_names) {
      expect_lt(abs(result[[name]] - reference[[name]]), 1e-10,
                label = paste(file, name))
    }
  }
})

test_that("the restricted 12 x 8 pair reverses E_aug and V_tt", {
  manifest <- arc_layout_manifest()
  a <- arc_evaluate_objectives(arc_fixture("restricted-12x8-a.csv"), 12,
                               "restricted")
  b <- arc_evaluate_objectives(arc_fixture("restricted-12x8-b.csv"), 12,
                               "restricted")
  expect_gt(a$E_aug, b$E_aug)
  expect_lt(b$V_tt, a$V_tt)
  expect_lt(abs(a$E_aug - 0.366768940288), 1e-10)
  expect_lt(abs(a$V_tt - 5.575452716298), 1e-10)
  expect_lt(abs(b$E_aug - 0.366429008708), 1e-10)
  expect_lt(abs(b$V_tt - 5.566184662506), 1e-10)
  for (pair in list(list(a, "restricted-12x8-a.csv"),
                    list(b, "restricted-12x8-b.csv"))) {
    entry <- manifest[manifest$file == pair[[2]], ]
    expect_lt(abs(pair[[1]]$E_aug - entry$ref_E_aug), 1e-10)
    expect_lt(abs(pair[[1]]$V_tt - entry$ref_V_tt), 1e-10)
  }
})

test_that("the weighted-variance identity holds, equal or unequal r", {
  fixtures <- arc_connected_contractions()
  unequal <- 0L
  for (i in seq_len(nrow(fixtures))) {
    entry <- fixtures[i, ]
    v <- entry$v
    s <- entry$s
    k <- entry$k
    contraction <- arc_fixture(entry$file)
    if (length(unique(tabulate(contraction, nbins = v))) > 1L) {
      unequal <- unequal + 1L
    }
    result <- arc_evaluate_objectives(contraction, v, entry$mode)
    n_tests <- (v - k) * s
    v_star <- n_tests + k
    # Pair weights u_i u_j: 1 (test-test), s (test-check), s^2 (check-check)
    weighted <- choose(n_tests, 2) * result$V_tt +
      s * n_tests * k * result$V_tc + s^2 * choose(k, 2) * result$V_cc
    lhs <- v * s * (v_star - 1) / result$E_aug
    expect_lt(abs(lhs - weighted) / abs(lhs), 1e-8, label = entry$file)
  }
  # Example 2 and the recoloring counterexample have unequal replication.
  expect_gte(unequal, 2L)
})

test_that("V_tt has the equal-replication closed form", {
  fixtures <- arc_connected_contractions()
  checked <- 0L
  for (i in seq_len(nrow(fixtures))) {
    entry <- fixtures[i, ]
    v <- entry$v
    s <- entry$s
    k <- entry$k
    contraction <- arc_fixture(entry$file)
    r <- tabulate(contraction, nbins = v)
    if (length(unique(r)) > 1L) {
      next
    }
    checked <- checked + 1L
    paper <- arc_evaluate_contraction(contraction, v, entry$mode)
    result <- arc_evaluate_objectives(contraction, v, entry$mode)
    n_tests <- (v - k) * s
    closed <- 2 + 2 / (n_tests - 1) *
      ((s - mean(r)) * paper$a + (v + k) * paper$b - 2 * (s - 1))
    expect_lt(abs(result$V_tt - closed), 1e-10, label = entry$file)
  }
  expect_gt(checked, 20L)
})

test_that("the recoloring counterexample has E_aug = 1/6", {
  result <- arc_evaluate_objectives(arc_fixture("recoloring-counterexample.csv"),
                                    6, "column_binary")
  expect_true(result$connected)
  expect_lt(abs(result$E_aug - 1 / 6), 1e-10)
})

test_that("single-matrix route returns early for a negative df", {
  result <- arc_evaluate_objectives(arc_fixture("false-qr.csv"), 54,
                                    "restricted", worst_pair = TRUE)
  expect_identical(names(result),
                   c("connected", "df_formula", "lambda_min_rel"))
  expect_false(result$connected)
  expect_identical(result$df_formula, -1L)
  expect_identical(result$lambda_min_rel, NA_real_)
})

test_that("single-matrix route gives a disconnected design diagnostics only", {
  result <- arc_evaluate_objectives(arc_disconnected_contraction, 6,
                                    "restricted", worst_pair = TRUE)
  expect_identical(names(result),
                   c("connected", "df_formula", "lambda_min_rel"))
  expect_false(result$connected)
  expect_identical(result$df_formula, 1L)
  expect_true(is.finite(result$lambda_min_rel))
  expect_lt(result$lambda_min_rel, 1e-8)
})

test_that("worst-pair maxima are returned only on request", {
  plain <- arc_evaluate_objectives(arc_ex2_contraction, 24, "restricted")
  expect_identical(names(plain),
                   c("connected", "df_formula", "lambda_min_rel",
                     arc_mean_names))
  full <- arc_evaluate_objectives(arc_ex2_contraction, 24, "restricted",
                                  worst_pair = TRUE)
  expect_identical(names(full), c(names(plain), "max_tt", "max_tc"))
  for (name in arc_mean_names) {
    expect_identical(full[[name]], plain[[name]], label = name)
  }
})

test_that("single-matrix route passes the mode through", {
  square <- arc_fixture("square-a.csv")
  expect_arc_error(arc_evaluate_objectives(square, 6, "restricted"),
                   "fieldhub_arc_row_repeat_error", "fieldhub_input_error")
})
