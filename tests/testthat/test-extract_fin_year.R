test_that("Date is assigned to the correct financial year", {
  expect_equal(extract_fin_year(as.Date("20120331", "%Y%m%d")), "2011/12")
  expect_equal(extract_fin_year(as.Date("20120401", "%Y%m%d")), "2012/13")
  expect_equal(extract_fin_year(as.POSIXct("20190104", format = "%Y%m%d")), "2018/19")
})

test_that("Non-date formats produce an error", {
  expect_error(extract_fin_year("28102019"))
  expect_error(extract_fin_year("28-Oct-2019"))
  expect_error(extract_fin_year(as.numeric("28102019")))
  expect_error(extract_fin_year(as.factor("28-Oct-2019")))
})
