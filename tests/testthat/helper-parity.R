# App/API parity helpers shared by test_design_args_parity.R and
# test_design_module.R.

#' `values` as Shiny can deliver them: every whole number as an integer
#'
#' Shiny decodes a whole-number numericInput value as an integer
#' (shiny:::decodeMessage() parses with simplifyVector = FALSE, so 270
#' arrives as 270L), and nrow() counts and automatic app seeds
#' (sample.int()) are integers too. Which fields take one of those paths
#' differs by module (the single diagonal module's raw `input$lines.d`, the
#' sparse module's nrow() count of an upload, the augmented RCBD module's
#' number of experiments, ...), so the tests send every whole number as an
#' integer -- the superset of what any module sends -- while the direct
#' API calls they compare with are written with doubles, as an R user
#' types them. Non-whole doubles (a percentage of checks such as 9.6),
#' characters, logicals and lists (a do_optim() allocation) are unchanged.
#' @noRd
shiny_shaped <- function(values) {
  lapply(values, function(v) {
    whole <- is.double(v) && is.null(dim(v)) && length(v) > 0L && all(is.finite(v)) &&
      all(v == trunc(v)) && all(abs(v) <= .Machine$integer.max)
    if (whole) storage.mode(v) <- "integer"
    v
  })
}

parity <- function(builder, engine, values, data = NULL, direct) {
  via_app <- do.call(engine, builder(shiny_shaped(values), data))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
}
