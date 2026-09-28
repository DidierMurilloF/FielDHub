test_that("every module connects its design to the shared reproduction component", {
  # The classic designs share the generic page server (mod_design_server()),
  # whose design reactive is `design`.
  # A spatial page of the generic module runs its results in
  # app_spatial_page(), whose design reactive is `design` too.
  sources <- c(
    mod_design_server = "design", app_spatial_page = "design",
    mod_sparse_allocation_server = "sparse_design",
    mod_multi_loc_preps_server = "pREPS_reactive"
  )
  calls <- function(code) {
    if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(list())
    if (is.call(code) && any(vapply(c("app_reproduction_outputs", "app_classic_workflow", "app_spatial_workflow"),
                                    function(name) identical(code[[1]], as.name(name)), logical(1)))) {
      return(list(code))
    }
    unlist(lapply(as.list(code), calls), recursive = FALSE)
  }
  registry <- fieldhub_app_registry()
  runs <- function(entry) {
    if (!is.null(entry$spec) && identical(entry$workflow_family, "spatial")) "app_spatial_page" else entry$server
  }
  expect_setequal(names(sources), unique(vapply(registry, runs, character(1))))
  for (entry in registry) {
    entry$server <- runs(entry)
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
  expect_identical(sum(all.names(body(app_spatial_workflow)) == "app_reproduction_outputs"), 1L)
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
