test_that("spatial controls describe the coordinate axis and retain existing defaults", {
  for (suffix in c(".O", ".DIAG", ".PREP")) {
    fields <- spatial_correlation_spec(suffix)
    expect_named(fields, c("correlation_x", "correlation_y"))
    expect_identical(fields$correlation_x, list(
      inputId = paste0("ROX", suffix), label = "Adjacent columns (within a row):",
      choices = seq(0.1, 0.9, 0.1), selected = 0.5
    ))
    expect_identical(fields$correlation_y, list(
      inputId = paste0("ROY", suffix), label = "Adjacent rows (within a column):",
      choices = seq(0.1, 0.9, 0.1), selected = 0.5
    ))
  }
  for (suffix in list(NULL, NA_character_, "", 1, c(".O", ".PREP"))) {
    expect_error(spatial_correlation_spec(suffix), class = "fieldhub_input_error")
  }
})

test_that("all spatial workflows share the correlation controls with their existing IDs", {
  sources <- c(mod_Diagonal_server = ".DIAG", mod_diagonal_multiple_server = ".DIAG",
               mod_sparse_allocation_server = ".DIAG", mod_Optim_server = ".O",
               mod_RCBD_augmented_server = ".O", mod_pREPS_server = ".PREP",
               mod_multi_loc_preps_server = ".PREP")
  calls <- function(code) {
    if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(list())
    if (is.call(code) && identical(code[[1]], as.name("spatial_workflow_spec"))) {
      return(list(code))
    }
    unlist(lapply(as.list(code), calls), recursive = FALSE)
  }
  for (name in names(sources)) {
    controls <- calls(body(get(name, asNamespace("FielDHub"))))
    expect_length(controls, 1L)
    if (length(controls) == 1L) {
      spec <- spatial_workflow_spec(controls[[1]][[2]])
      expect_identical(spec$correlation_suffix, sources[[name]])
      expect_identical(spec$correlation_ids, c(x = paste0("ROX", sources[[name]]),
                                               y = paste0("ROY", sources[[name]])))
    }
  }
  expect_identical(sum(all.names(body(app_spatial_simulation_modal)) == "app_spatial_correlations"), 1L)
})
