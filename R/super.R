#' Call the next available function by the same name.
#'
#' @param ... The arguments passed to the parent function.
#' @return the result of calling the next available function.
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
  fn <- as.character(sys.call(-1)[[1]])

  env <- parent.env(parent.frame(2))

  while (!identical(env, emptyenv())) {
    if (exists(fn, envir = env, inherits = FALSE)) {
      super_fn <- get(fn, envir = env, inherits = FALSE)
      if (is.function(super_fn)) {
        return(do.call(fn, eval(substitute(alist(...))), envir = env))
      }
    }
    env <- parent.env(env)
  }

  stop("super: No parent function ", sQuote(fn))
}
