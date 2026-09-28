test_that("simulation control identifiers are validated before building a dialog", {
  ids <- c(trait = "trailsCRD", other = "OtherCRD", minimum = "min.crd", maximum = "max.crd", submit = "ok.crd")
  expect_identical(simulation_control_ids(ids), ids)
  for (bad in list(ids[-1], unname(ids), rep("same", 5), replace(ids, 1, NA_character_),
                   replace(ids, 1, "bad'id"), replace(ids, 1, "bad-id"), replace(ids, 1, ids[2]))) {
    expect_error(simulation_control_ids(bad), class = "fieldhub_input_error")
  }
})

test_that("classic simulation dialogs share a component with namespaced controls", {
  modules <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD",
               "RowCol", "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
  for (module in modules) {
    code <- design_server_body(module)
    expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
  }
  expect_identical(sum(all.names(body(app_classic_workflow)) == "app_simulation_modal"), 1L)
  ids <- c(trait = "trait", other = "other", minimum = "min", maximum = "max", submit = "ok")
  html <- as.character(app_simulation_modal(shiny::NS("example"), ids))
  expect_match(html, 'id="example-trait"', fixed = TRUE)
  expect_match(html, 'id="example-other"', fixed = TRUE)
  expect_match(html, 'id="example-min"', fixed = TRUE)
  expect_match(html, 'id="example-max"', fixed = TRUE)
  expect_match(html, 'id="example-ok"', fixed = TRUE)
  expect_match(html, 'data-ns-prefix="example-"', fixed = TRUE)
  expect_match(html, "Trait name:", fixed = TRUE)
})
