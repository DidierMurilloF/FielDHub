test_that("all app CSV downloads share the workflow archive writer", {
  classic <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD", "RowCol",
                "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
  spatial <- c("Diagonal", "diagonal_multiple", "sparse_allocation", "Optim",
                "RCBD_augmented", "pREPS", "multi_loc_preps")
  for (module in c(classic, spatial)) {
    code <- body(get(paste0("mod_", module, "_server"), asNamespace("FielDHub")))
    if (module %in% classic) {
      expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
      code <- body(app_classic_workflow)
    } else {
      expect_identical(sum(all.names(code) == "app_spatial_workflow"), 1L)
      code <- body(app_spatial_workflow)
    }
    expected <- if (module %in% classic) 2L else 1L
    expect_identical(sum(all.names(code) == "app_csv_archive"), expected)
    expect_identical(sum(all.names(code) == "write.csv"), 0L)
  }
})
