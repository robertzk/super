context("packages")

test_that("it can call super from packages in the correct way", {
  devtools::load_all('packages/package1')
  devtools::load_all('packages/package2')
  devtools::load_all('packages/package3')
  expect_output(foo(), "threetwoone")
})

test_that("loading in a different order gives different output", {
  devtools::load_all('packages/package1')
  devtools::load_all('packages/package3')
  devtools::load_all('packages/package2')
  expect_output(foo(), "twothreeone")
})
