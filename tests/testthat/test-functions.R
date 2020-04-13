test_that("creates filename", {
  expect_that(make_filename(2013), is_a("character"))
  expect_that(fars_read(filename = "accident_2013.csv"),is_a("data.frame"))
})

