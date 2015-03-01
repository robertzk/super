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


