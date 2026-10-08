test_that("all app CSV downloads use the plain CSV writer without archives", {
  classic <- names(fieldhub_classic_workflows())
  spatial <- c("Diagonal", "diagonal_multiple", "sparse_allocation", "Optim",
                "RCBD_augmented", "pREPS", "multi_loc_preps")
  for (module in c(classic, spatial)) {
    code <- if (module %in% classic) design_server_body(module) else spatial_server_body(module)
    if (module %in% classic) {
      expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
      code <- body(app_classic_workflow)
    } else {
      expect_identical(sum(all.names(code) == "app_spatial_workflow"), 1L)
      code <- body(app_spatial_workflow)
    }
    expect_identical(sum(all.names(code) == "app_csv_download"), 1L)
    expect_identical(sum(all.names(code) == "app_plot_outputs"), 1L)
    expect_false("app_csv_archive" %in% all.names(code))
    expect_identical(sum(all.names(code) == "write.csv"), 0L)
  }
  expect_true("app_plot_outputs" %in% all.names(body(app_spatial_plot_outputs)))
  expect_true("app_csv_download" %in% all.names(body(app_plot_outputs)))
  expect_false("app_csv_archive" %in% all.names(body(app_plot_outputs)))
})
