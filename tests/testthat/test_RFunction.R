library('move2')

test_data <- test_data("input3_move2.rds") #file must be move2!
actual <- rFunction(data = test_data, days_prior = 14)

test_that("happy path", {
  expect_equal(test_data,actual)
})
