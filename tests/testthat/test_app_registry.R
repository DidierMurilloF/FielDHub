library(FielDHub)

test_that("the app registry describes every existing design module", {
  registry <- FielDHub:::fieldhub_app_registry()
  expect_length(registry, 20)
  ids <- vapply(registry, `[[`, character(1), "id")
  expect_identical(anyDuplicated(ids), 0L)
  expect_identical(unique(vapply(registry, `[[`, character(1), "group")),
                   c("Unreplicated Designs", "Partially Replicated Designs",
                     "Lattice Designs", "Other Designs"))
  namespace <- asNamespace("FielDHub")
  for (entry in registry) {
    expect_true(is.function(get(entry$ui, namespace)))
    expect_true(is.function(get(entry$server, namespace)))
    expect_true(entry$engine %in% getNamespaceExports("FielDHub"))
  }
  registered <- vapply(registry, `[[`, character(1), "server")
  expect_setequal(registered, ls(namespace, pattern = "^mod_.*_server$"))
})

test_that("the registry preserves the existing server registration order", {
  registry <- FielDHub:::fieldhub_app_registry()
  order <- order(vapply(registry, `[[`, integer(1), "server_order"))
  expect_identical(unname(vapply(registry[order], `[[`, character(1), "id")), c(
    "Diagonal_ui_1", "diagonal_multiple_ui_1", "Optim_ui_1", "RCBD_augmented_ui_1",
    "sparse_allocation_ui_1", "pREPS_ui_1", "multi_loc_preps_ui_1", "Square_Lattice_ui_1",
    "Rectangular_Lattice_ui_1", "Alpha_Lattice_ui_1", "CRD_ui_1", "RCBD_ui_1",
    "LSD_ui_1", "FD_ui_1", "SPD_ui_1", "SSPD_ui_1", "IBD_ui_1", "RowCol_ui_1", "STRIPD_ui_1",
    "Latin_Rectangle_ui_1"
  ))
})

test_that("the app title is derived from package metadata", {
  expect_identical(FielDHub:::fieldhub_app_title(),
                   paste0("FielDHub v", utils::packageVersion("FielDHub")))
})
