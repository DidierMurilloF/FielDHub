test_that("every module connects its design to the shared reproduction component", {
  sources <- c(
    mod_CRD_server = "CRD_reactive", mod_RCBD_server = "RCBD_reactive",
    mod_LSD_server = "latinsquare_reactive", mod_FD_server = "fd_reactive",
    mod_SPD_server = "spd_reactive", mod_SSPD_server = "sspd_reactive",
    mod_STRIPD_server = "strip_reactive", mod_IBD_server = "IBD_reactive",
    mod_RowCol_server = "RowCol_reactive", mod_Alpha_Lattice_server = "ALPHA_reactive",
    mod_Rectangular_Lattice_server = "RECTANGULAR_reactive",
    mod_Square_Lattice_server = "SQUARE_reactive", mod_Diagonal_server = "diagonal_design",
    mod_diagonal_multiple_server = "diagonal_design", mod_Optim_server = "optimized_arrang",
    mod_RCBD_augmented_server = "rcbd_augmented_reactive",
    mod_sparse_allocation_server = "sparse_design", mod_pREPS_server = "pREPS_reactive",
    mod_multi_loc_preps_server = "pREPS_reactive"
  )
  calls <- function(code) {
    if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(list())
    if (is.call(code) && any(vapply(c("app_reproduction_outputs", "app_classic_workflow"),
                                    function(name) identical(code[[1]], as.name(name)), logical(1)))) {
      return(list(code))
    }
    unlist(lapply(as.list(code), calls), recursive = FALSE)
  }
  registry <- fieldhub_app_registry()
  expect_setequal(names(sources), vapply(registry, `[[`, character(1), "server"))
  for (entry in registry) {
    bindings <- calls(body(get(entry$server, asNamespace("FielDHub"))))
    expect_length(bindings, 1L)
    if (length(bindings) == 1L) {
      if (!is.null(entry$workflow)) {
        expect_identical(bindings[[1]][[3]], as.name("output"), info = entry$server)
        expect_identical(bindings[[1]][["design"]][[3]], call(sources[[entry$server]]), info = entry$server)
      } else {
        expect_identical(bindings[[1]][[2]], as.name("output"), info = entry$server)
        expect_identical(bindings[[1]][[3]], as.name(sources[[entry$server]]), info = entry$server)
      }
    }
  }
  expect_identical(sum(all.names(body(app_classic_workflow)) == "app_reproduction_outputs"), 1L)
  expect_true("app_reproduction_ui" %in% all.names(body(fieldhub_design_menus)))
})

test_that("CSV layout downloads are labeled as CSV", {
  namespace <- asNamespace("FielDHub")
  for (entry in fieldhub_app_registry()) {
    code <- body(get(entry$ui, namespace))
    expect_false('"Excel"' %in% trimws(deparse(code)), info = entry$ui)
    expect_false(grepl('label = *"Excel"', paste(deparse(code), collapse = " ")), info = entry$ui)
  }
})
