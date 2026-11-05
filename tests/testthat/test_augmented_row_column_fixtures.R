# Augmented row-column designs: fixture manifest tests.
# Research M1 specification, sections 7 and 10. Reference values come from
# Piepho and Williams (2026), Biometrical Journal 68(5):e70182 (CC-BY), and
# Bailey and Haines (2025), JABES (CC-BY), as cited at each test.

manifest_columns <- c(
  "file", "kind", "v", "s", "k", "mode", "source", "licence",
  "pub_E_aug", "pub_E_aug_dec", "pub_Cbar_v", "pub_Cbar_v_dec",
  "pub_Cbar_s", "pub_Cbar_s_dec", "pub_A_tt", "pub_A_tt_dec",
  "pub_A_ct", "pub_A_ct_dec", "ref_connected", "ref_df_formula",
  "ref_residual_df_rank", "ref_E_aug", "ref_V_tt", "ref_V_tc", "ref_V_cc",
  "ref_max_tt", "ref_max_tc"
)

read_manifest <- function() {
  utils::read.csv(
    test_path("fixtures", "augmented-row-column", "manifest.csv"),
    stringsAsFactors = FALSE
  )
}

test_that("the manifest has exactly the specified columns", {
  manifest <- read_manifest()
  expect_identical(names(manifest), manifest_columns)
  expect_gt(nrow(manifest), 0L)
  expect_false(anyDuplicated(manifest$file) > 0L)
  expect_identical(manifest$file, sort(manifest$file, method = "radix"))
})

test_that("22 supplement-derived contraction rows are present", {
  manifest <- read_manifest()
  supplement <- grepl("^supplement", manifest$source) & manifest$kind == "contraction"
  expect_equal(sum(supplement), 22L)
  expect_equal(sum(grepl("^table1-", manifest$file)), 21L)
  expect_true("example2-contraction.csv" %in% manifest$file)
})

test_that("every manifest file exists, parses and has matching dimensions", {
  manifest <- read_manifest()
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, ]
    path <- test_path("fixtures", "augmented-row-column", row$file)
    expect_true(file.exists(path), info = row$file)
    design <- arc_fixture(row$file)
    expect_false(anyNA(design), info = row$file)
    if (row$kind == "contraction") {
      expect_identical(dim(design), c(row$k, row$s), info = row$file)
      expect_true(all(design >= 1L & design <= row$v), info = row$file)
      expect_true(row$mode %in% c("restricted", "column_binary"), info = row$file)
    } else {
      expect_identical(row$mode, "layout", info = row$file)
      expect_identical(dim(design), c(row$v, row$s), info = row$file)
    }
  }
})

test_that("published values lie within rounding distance of the references", {
  manifest <- read_manifest()
  within_print <- function(published, decimals, reference) {
    abs(published - reference) <= 0.5 * 10^(-decimals) + 1e-12
  }
  compared <- 0L
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, ]
    pairs <- list(
      c("pub_E_aug", "ref_E_aug"),
      c("pub_A_tt", "ref_V_tt"),
      c("pub_A_ct", "ref_V_tc")
    )
    for (pair in pairs) {
      published <- row[[pair[1]]]
      if (is.na(published)) next
      expect_false(is.na(row[[paste0(pair[1], "_dec")]]), info = row$file)
      expect_true(
        within_print(published, row[[paste0(pair[1], "_dec")]], row[[pair[2]]]),
        info = paste(row$file, pair[1])
      )
      compared <- compared + 1L
    }
    # Cbar_v and Cbar_s have no oracle reference: only presence is checked
    for (column in c("pub_Cbar_v", "pub_Cbar_s")) {
      if (!is.na(row[[column]])) {
        expect_false(is.na(row[[paste0(column, "_dec")]]), info = row$file)
      }
    }
  }
  expect_gt(compared, 20L)
})

test_that("Examples and Table 1 rows carry every published value", {
  manifest <- read_manifest()
  rows <- manifest[grepl("^table1-|^example[12]-contraction", manifest$file), ]
  expect_equal(nrow(rows), 23L)
  for (column in c("pub_E_aug", "pub_Cbar_v", "pub_Cbar_s")) {
    expect_false(anyNA(rows[[column]]), info = column)
  }
})

test_that("Bailey-Haines layouts carry A_tt and A_ct, and C(4,4,4) is disconnected", {
  manifest <- read_manifest()
  layouts <- manifest[manifest$kind == "layout" & grepl("^bailey-haines-", manifest$file), ]
  expect_equal(nrow(layouts), 5L)
  disconnected <- layouts[layouts$ref_connected == FALSE, ]
  expect_equal(nrow(disconnected), 1L)
  expect_match(disconnected$file, "4-4-4")
  expect_true(is.na(disconnected$ref_E_aug))
  connected <- layouts[layouts$ref_connected == TRUE, ]
  expect_false(anyNA(connected$pub_A_tt))
  expect_false(anyNA(connected$pub_A_ct))
})
