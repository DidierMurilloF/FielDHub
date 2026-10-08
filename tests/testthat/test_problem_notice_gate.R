test_that("repeated unexpected notices do not queue identical dialogs", {
  gate <- problem_notice_gate()
  expect_true(gate("Unexpected problem: broken choice"))
  expect_false(gate("Unexpected problem: broken choice"))
  expect_false(gate("Unexpected problem: broken choice"))
  expect_true(gate("Unexpected problem: different failure"))
  expect_true(gate("Unexpected problem: broken choice"))
})

test_that("notice gates have independent state for each session", {
  first <- problem_notice_gate()
  second <- problem_notice_gate()
  expect_true(first("bug"))
  expect_true(second("bug"))
  expect_false(first("bug"))
  expect_false(second("bug"))
})
