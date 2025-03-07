test_that("findRenamedCols works", {
  settings1 <- c(c_change1 = 1, c_equal1 = 2, c_equal2 = 3, cm_change2 = 4)
  settings2 <- c(cm_change1 = 5, c_equal1 = 6, c_equal2 = 7, c_change2 = 8)
  expect_equal(findRenamedCols(settings1, settings2),
               c(c_change1 = "cm_change1", cm_change2 = "c_change2"))
})
