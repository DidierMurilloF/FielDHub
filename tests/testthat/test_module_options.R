library(FielDHub)

# Static architecture check: inspect code without starting a Shiny server.
writes_global_table_options <- function(code) {
  if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(FALSE)
  if (is.call(code) && identical(code[[1]], as.name("options")) &&
      "DT.options" %in% names(code)) return(TRUE)
  any(vapply(as.list(code), writes_global_table_options, logical(1)))
}

test_that("modules keep table options out of process-global state", {
  namespace <- asNamespace("FielDHub")
  modules <- ls(namespace, pattern = "^mod_.*_server$")
  offenders <- modules[vapply(modules, function(name) {
    writes_global_table_options(body(get(name, namespace)))
  }, logical(1))]
  expect_identical(offenders, character())
})
