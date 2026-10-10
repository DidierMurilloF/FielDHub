# Augmented row-column designs: independent oracle tests.
# Research M1 specification, sections 7 and 10. Reference values come from
# Piepho and Williams (2026), Biometrical Journal 68(5):e70182 (CC-BY), and
# Bailey and Haines (2025), JABES (CC-BY), as cited at each test.

# Printed field layouts of Piepho and Williams (2026), Example 1 (12 x 8,
# checks 73 to 75) and Example 2 (24 x 16, checks 305 to 309), copied once
# from the article as literals.
arc_example_1 <- matrix(as.integer(c(
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

arc_example_2 <- matrix(as.integer(c(
  309,  20,  39,  58,  77, 306, 115, 134, 153, 172, 191, 210, 229, 248, 267, 305,
    1,  21,  40,  59,  78, 309, 116, 307, 154, 305, 192, 211, 230, 249, 268, 286,
    2,  22,  41,  60,  79,  96, 117, 135, 155, 173, 305, 306, 309, 250, 269, 287,
    3, 307,  42,  61,  80,  97, 118, 309, 156, 174, 193, 212, 308, 251, 270, 288,
    4,  23, 308,  62,  81, 307, 119, 136, 157, 175, 306, 213, 231, 252, 271, 289,
  306,  24, 309,  63,  82,  98, 120, 308, 158, 176, 194, 214, 232, 307, 272, 290,
    5,  25, 305,  64,  83,  99, 121, 137, 308, 177, 195, 309, 233, 253, 273, 291,
    6,  26,  43, 305,  84, 100, 122, 138, 159, 178, 307, 215, 234, 308, 274, 306,
    7, 308, 306,  65,  85, 101, 305, 139, 160, 307, 196, 216, 235, 254, 275, 292,
    8,  27,  44, 306,  86, 308, 309, 140, 161, 179, 197, 217, 305, 255, 276, 293,
    9,  28,  45,  66,  87, 102, 307, 141, 305, 180, 198, 218, 236, 256, 277, 309,
   10,  29,  46,  67, 306, 103, 123, 142, 162, 309, 199, 219, 237, 257, 278, 308,
  305,  30,  47,  68,  88, 104, 308, 143, 163, 181, 200, 220, 238, 258, 307, 294,
   11,  31,  48,  69,  89, 105, 306, 144, 164, 182, 201, 307, 239, 309, 279, 295,
   12, 306,  49,  70, 307, 305, 124, 145, 165, 183, 202, 308, 240, 259, 280, 296,
   13,  32,  50,  71, 309, 106, 125, 146, 166, 184, 203, 221, 241, 306, 305, 297,
   14,  33,  51,  72,  90, 107, 126, 147, 307, 306, 308, 222, 242, 260, 309, 298,
   15,  34,  52,  73,  91, 108, 127, 306, 167, 185, 204, 305, 243, 261, 308, 307,
   16,  35, 307, 309, 305, 109, 128, 148, 168, 186, 205, 223, 244, 262, 281, 299,
  308, 305,  53,  74,  92, 110, 129, 149, 169, 187, 309, 224, 245, 263, 282, 300,
  307,  36,  54,  75, 308, 111, 130, 150, 309, 188, 206, 225, 306, 264, 283, 301,
   17, 309,  55, 307,  93, 112, 131, 151, 170, 189, 207, 226, 246, 265, 306, 302,
   18,  37,  56, 308,  94, 113, 132, 305, 306, 190, 208, 227, 247, 266, 284, 303,
   19,  38,  57,  76,  95, 114, 133, 152, 171, 308, 209, 228, 307, 305, 285, 304
)), nrow = 24, byrow = TRUE)

# Bailey and Haines (2025) square array: t x t field, k controls, t(t - k)
# tests. Control i sits at cell (j + 1, (block[i] + j) mod t + 1) for
# j = 0, ..., t - 1, with code t(t - k) + i; the remaining cells take test
# codes 1, ..., t(t - k) in column-major order.
arc_bailey_haines <- function(block, t = 12) {
  n_tests <- t * (t - length(block))
  codes <- matrix(0L, t, t)
  j <- 0:(t - 1)
  for (i in seq_along(block)) {
    codes[cbind(j + 1, (block[i] + j) %% t + 1)] <- as.integer(n_tests + i)
  }
  codes[codes == 0L] <- seq_len(n_tests)
  codes
}

test_that("oracle reproduces Example 1 of Piepho and Williams (2026)", {
  # Biometrical Journal 68(5):e70182, Example 1; full-precision E_aug from
  # specification section 10.1, residual df ks - k - v - s + 2 = 3
  result <- arc_oracle(arc_example_1, check_codes = 73:75)
  expect_true(result$connected)
  expect_equal(result$E_aug, 0.388111888112, tolerance = 1e-10)
  expect_identical(result$residual_df_rank, 3L)
})

test_that("oracle reproduces Example 2 of Piepho and Williams (2026)", {
  # Biometrical Journal 68(5):e70182, Example 2; residual df 37. Checks are
  # inferred as the codes occurring more than once.
  result <- arc_oracle(arc_example_2)
  expect_true(result$connected)
  expect_equal(result$E_aug, 0.603141344828, tolerance = 1e-10)
  expect_identical(result$residual_df_rank, 37L)
})

test_that("oracle reproduces Bailey and Haines (2025) C(3,4,5)", {
  # JABES, Table 1: t = 12, k = 3, initial block {0, 3, 7}; A_tt = 4.0341 and
  # A_ct = 2.0910 printed to 4 decimals, A_cc = 1/6
  result <- arc_oracle(arc_bailey_haines(c(0, 3, 7)), check_codes = 109:111)
  expect_true(result$connected)
  expect_lt(abs(result$V_tt - 4.0341), 5e-5)
  expect_lt(abs(result$V_tc - 2.0910), 5e-5)
  expect_equal(result$V_cc, 1 / 6, tolerance = 1e-10)
})

test_that("oracle refuses objectives for Bailey and Haines (2025) C(4,4,4)", {
  # JABES, Table 1: initial block {0, 4, 8} is disconnected; the rank-based
  # residual df is 14 (specification section 5, refusal rule)
  result <- arc_oracle(arc_bailey_haines(c(0, 4, 8)), check_codes = 109:111)
  expect_false(result$connected)
  expect_identical(result$residual_df_rank, 14L)
  expect_null(result$E_aug)
  for (field in c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc", "pairwise")) {
    expect_false(field %in% names(result), info = field)
  }
})

test_that("weighted-variance identity holds on Example 2 (unequal replication)", {
  # Specification section 6: n (v* - 1) / E_aug equals the sum over i < j of
  # u_i u_j Var(tau_i - tau_j). Example 2 has tests once and checks 16 times.
  result <- arc_oracle(arc_example_2)
  pairwise <- result$pairwise
  u <- as.vector(table(arc_example_2)[rownames(pairwise)])
  n <- length(arc_example_2)
  weighted <- outer(u, u) * pairwise
  expect_equal(sum(weighted[upper.tri(weighted)]),
               n * (nrow(pairwise) - 1) / result$E_aug, tolerance = 1e-8)
})

test_that("transposing Example 1 leaves the objectives unchanged", {
  original <- arc_oracle(arc_example_1)
  transposed <- arc_oracle(t(arc_example_1))
  for (name in c("E_aug", "V_tt", "V_tc", "V_cc")) {
    expect_equal(transposed[[name]], original[[name]], tolerance = 1e-10, info = name)
  }
})

test_that("residual df uses singular values, not squared ones", {
  # False-QR design (M1 spike 2b): disconnected, connected-design df formula
  # -1. Rank identities give rank([X Z]) = (v + s - 1) + (v* - 2) = 756 = n,
  # so the rank-based residual df is 0. Its smallest genuine singular value
  # is 2.3e-5 of the largest; a tolerance on squared values would drop it
  # and report 1.
  contraction <- arc_fixture("false-qr.csv")
  v <- 54L
  k <- nrow(contraction)
  s <- ncol(contraction)
  n_tests <- (v - k) * s
  codes <- matrix(0L, v, s)
  for (i in seq_len(k)) codes[cbind(contraction[i, ], seq_len(s))] <- n_tests + i
  codes[codes == 0L] <- seq_len(n_tests)
  result <- arc_oracle(codes)
  expect_false(result$connected)
  expect_identical(result$residual_df_rank, 0L)
})
