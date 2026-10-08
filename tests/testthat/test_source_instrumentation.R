test_that("structural checks unwrap only the known coverage-counter wrapper", {
  # These are syntax fixtures, never evaluated or loaded as dependencies.
  syntax <- function(text) parse(text = text)[[1L]]
  wrapped <- syntax('if (TRUE) { covr:::count("location"); force(args_reactive) }')
  expect_identical(fieldhub_uninstrument(wrapped), quote(force(args_reactive)))
  untouched <- list(
    syntax('if (FALSE) { covr:::count("location"); force(args_reactive) }'),
    syntax('if (TRUE) { other::count("location"); force(args_reactive) }'),
    syntax('if (TRUE) { covr:::count("location"); other(); force(args_reactive) }'),
    quote(app_check_dependencies())
  )
  for (expression in untouched) expect_identical(fieldhub_uninstrument(expression), expression)
})
