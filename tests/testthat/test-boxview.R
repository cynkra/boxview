test_that("boxview() snapshots", {
  expect_snapshot(boxview(ave))
  expect_snapshot(boxview(ave, width = 40))
  expect_snapshot(boxview(ave, width = 40, optimization = "strong"))
})
