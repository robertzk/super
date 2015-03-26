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

test_that("it passes on non-standard evaluation and scope with tweaks", {
  expect_equal({
    out <- function(x) list(x, deparse(substitute(x)))
    local({
      out <- function(x) { super::super(x + 1) }
      val <- 1
      out(val)
    })
  }, list(2, "val + 1"))
})

test_that("it is smart about translating NSE through name changes", {
  # TODO: (RK) Is this really the correct behavior?
  # options(super.debug=T);on.exit(options(super.debug=F))
  expect_equal({
    out <- function(x) list(x, deparse(substitute(x)))
    local({
      out <- function(y) { super::super(y) }
      val <- 1
      out(y = val)
    })
  }, list(1, "val"))
})

test_that("it is smart about translating NSE through name swaps", {
  expect_equal({
    out <- function(x, y) list(x, y, deparse(substitute(x)), deparse(substitute(y)))
    local({
      out <- function(y, x) { super::super(x, y) }
      val <- 1
      val2 <- 2
      out(y = val, x = val2)
    })
  }, list(2, 1, "val2", "val"))
})

test_that("it is smart about translating NSE through named name swaps", {
  expect_equal({
    out <- function(x, y) list(x, y, deparse(substitute(x)), deparse(substitute(y)))
    local({
      out <- function(y, x) { super::super(y = x,  x = y) }
      val <- 1
      val2 <- 2
      out(y = val, x = val2)
    })
  }, list(1, 2, "val", "val2"))
})

test_that("it cannot call `get` directly", {
  somefunc <- function() "hello!"
  expect_error(super::super(get("hello", envir = parent.frame(), inherits = FALSE)))
})
