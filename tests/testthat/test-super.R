context("super") 

test_that("it should call the parent method once", {
  calls <- integer(0)
  function1 <- function() { calls <<- c(calls, 1L) }
  local({
    function1 <- function() {
      calls <<- c(calls, 2L)
      super::super()
    }
    function1()
  })
  expect_equal(calls, c(2L, 1L))
})

test_that("it should call the parent method twice removed", {
  calls <- integer(0)
  function1 <- function() { calls <<- c(calls, 1L) }
  local({
    local({
      function1 <- function() {
        calls <<- c(calls, 2L)
        super::super()
      }
      function1()
    })
  })
  expect_equal(calls, c(2L, 1L))
})

test_that("it should call the parent method twice removed with another super call", {
  calls <- integer(0)
  local({
    level1 <- TRUE
    function1 <- function() {
      calls <<- c(calls, 1L)
    }
    local({
      level2 <- TRUE
      function1 <- function() {
        calls <<- c(calls, 2L)
        super::super()
      }
      local({
        level3 <- TRUE
        function1 <- function() {
          calls <<- c(calls, 3L)
          super::super()
        }
        function1()
      })
    })
  })
  expect_equal(calls, c(3L, 2L, 1L))
})

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

test_that("it can execute a simple local call", {
  expect_output({
    out <- function(x) print(x)
    local({
      out <- function(x) { super::super(x) }
      out("hi")
    })
  }, "hi")
})

test_that("it passes on non-standard evaluation", {
  expect_output({
    out <- function(x) deparse(substitute(x))
    local({
      out <- function(x) { super::super(x) }
      out(hi)
    })
  }, "hi")
})

test_that("it passes on non-standard evaluation and scope", {
  expect_equal({
    out <- function(x) list(x, deparse(substitute(x)))
    local({
      out <- function(x) { super::super(x) }
      val <- 1
      out(val)
    })
  }, list(1, "val"))
})



