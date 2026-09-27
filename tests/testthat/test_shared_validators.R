library(FielDHub)

test_that("every planter check shares one classed error", {
  e <- expect_error(FielDHub:::validate_planter("zigzag"), class = "fieldhub_input_error")
  expect_identical(e$options, c("serpentine", "cartesian"))

  # Internal callers of the shared validator: each used to have its own
  # copy of the "serpentine" or "cartesian" check.
  internal_calls <- list(
    function() FielDHub:::ARCBD_name(planter = "zigzag"),
    function() FielDHub:::ARCBD_plot_number(planter = "zigzag"),
    function() FielDHub:::export_design(movement_planter = "zigzag"),
    function() FielDHub:::get_random(planter_mov = "zigzag")
  )
  for (f in internal_calls) {
    e <- expect_error(f(), class = "fieldhub_input_error")
    expect_identical(e$options, c("serpentine", "cartesian"))
  }
})
