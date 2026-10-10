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
