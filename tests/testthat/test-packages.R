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

test_that("calling local function chains can ascend to packages", {
  devtools::load_all('packages/package1')
  devtools::load_all('packages/package2')
  devtools::load_all('packages/package3')
  foo <- function() {
    cat("four")
    super::super()
  }
  local({
    foo <- function() {
      cat("five")
      super::super()
    }
    expect_output(foo(), "fivefourthreetwoone")
  })
})

test_that("calling local function chains can ascend to packages *and* change calling arguments", {
  devtools::load_all('packages/package1')
  devtools::load_all('packages/package2')
  devtools::load_all('packages/package3')
  foo <- function(x) {
    cat(x)
    super::super("trois")
  }
  local({
    foo <- function() {
      cat("five")
      super::super("four")
    }
    expect_output(foo(), "fivefourtroisdeuxun")
  })
})

