context("super") 

test_that("it calls the parent method in a single example chain", {
  calls <- integer(0)
  function1 <- function() {
    calls <<- c(calls, 1L)
    invisible(NULL)
  }

  local({
    function2 <- function() {
      function1 <- function() {
        calls <<- c(calls, 2L)
        super()
      }

      function3 <- function() {
        function1 <- function() {
          calls <<- c(calls, 3L)
          super()
        }
        function1()
      }

      function3()
    }

    function2()
  })

  expect_equal(calls, 3L:1L)
})

test_that("it errors when no super method exists", {
  not_a_function <- function() { super() }
  expect_error(not_a_function(), "No parent function")
})

test_that("it can call with different super arguments", {
  calls <- integer(0)
  function1 <- function(x) {
    calls <<- c(calls, x)
  }

  function2 <- function() {
    function1 <- function(y) {
      calls <<- c(calls, y)
      super(2)
    }
    function1(1)
  }
  function2()

  expect_equal(calls, c(1L, 2L))
})

test_that("it can call without executing twice from a non-base call", {
  calls <- integer(0)
  function1 <- function(x) {
    calls <<- c(calls, x)
  }

  function2 <- function() {
    function1 <- function(y) {
      calls <<- c(calls, y)
      super(2)
    }
    local({
      function1(1)
    })
  }
  function2()

  expect_equal(calls, c(1L, 2L))
})


