#' Call the next available function by the same name.
#'
#' @param ... The arguments passed to the parent function.
#' @return the result of calling the next available function.
#' @export
#' @examples
#' function1 <- function() {
#'   print("Top-level")
#'   invisible(NULL)
#' }
#' 
#' local({
#'   function2 <- function() {
#'     function1 <- function() {
#'       print("Mid-level")
#'       super::super()
#'     }
#' 
#'     function3 <- function() {
#'       function1 <- function() {
#'         print("Low-level")
#'         super::super()
#'       }
#'       function1()
#'     }
#' 
#'     function3()
#'   }
#' 
#'   function2()
#' })
#' # Will print
#' # [1] "Low-level"
#' # [1] "Mid-level"
#' # [1] "Top-level"
super <- function(...) {
  # TODO: (RK) Error on anonymous function calls.
  fn <- as.character(sys.call(-1)[[1]])
  if (identical(fn[1], "get")) {
    if (length(fn) == 1) {
      stop("super::super does not support ", sQuote("get"), " calls ",
           "due to internal purposes.")
    } else {
      # From the magic get() call below, we extract the function name.
      fn <- fn[2] 
      env <- parent.frame()
    }
  } else {
    env <- parent.frame(2)
  }

  first <- TRUE
  parent_count <- -1
  while (!identical(env, emptyenv())) {
    parent_count <- parent_count + 1
    if (exists(fn, envir = env, inherits = FALSE)) {
      super_fn <- get(fn, envir = env, inherits = FALSE)
      if (is.function(super_fn)) {
        if (first) { first <- FALSE }
        else {
          # We have to find the equivalent of fn in the environment that is the
          # n-fold parent environment of the calling environment, parent.frame(),
          # where n = parent_count. Otherwise, non-standard evaluation will not work
          # correctly with super.
          return(eval.parent(substitute(
            # The magic number 5 comes from the fact eval.parent(...) creates its own
            # call chain with 4 steps inserted that we wish to skip over.
            {
              `*call*`      <- sys.call(-4)
              `*call*`[[1]] <- quote(
                get(fn, envir = super::multi_parent_env(parent.frame(7), parent_count)) # parent.frame(), parent_count))
              )
              eval(`*call*`)
            }
          )))
        }
      }
    }
    env <- parent.env(env)
  }

  stop("super: No parent function ", sQuote(fn))
}

#' @export
multi_parent_env <- function(env, times) {
  while (times > 0) {
    env <- parent.env(env)
    times <- times - 1
  }
  env
}

