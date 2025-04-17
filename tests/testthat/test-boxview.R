test_that("boxview() snapshots", {
  expect_snapshot(boxview(ave))
  expect_snapshot(boxview(ave, width = 40))
  expect_snapshot(boxview(ave, width = 40, optimization = "strong"))
})

test_that("swapcall() snapshots", {
  expect_snapshot(
    swap_calls(quote({a <- if (this) {if (this) a else b} else b}))
  )
})
