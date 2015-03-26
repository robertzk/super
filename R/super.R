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
    if (length(fn) == 1) { # TODO: (RK) Actually parse out super::multi_parent_env
      stop("super::super does not support ", sQuote("get"), " calls ",
           "due to internal purposes.")
    } else {
      # From the magic get() call below, we extract the function name.
      fn <- fn[2] 
      env <- parent.frame()
      level <- 2 # See comment below.
    }
  } else {
    env   <- parent.frame(2)
    level <- 7
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
            # The magic number level = 7 comes from the fact eval.parent(...) creates its own
            # call chain with 5 steps inserted that we wish to skip over. Otherwise,
            # if we are already in a super calling chain, we use level = 2.
            {
              `*call*`      <- sys.call(-4)
              `*call*`[[1]] <- quote(
                get(fn, envir = super::multi_parent_env(parent.frame(level), parent_count)) # parent.frame(), parent_count))
              )
              eval(`*call*`)
            }
          )))
        }
      }
    }
    env <- hop(env)
  }

  stop("super: No parent function ", sQuote(fn))
}

hop <- function(env) {
  if (isNamespace(env)) {
    # Shortcut! Since package namespaces traverse through imports back
    # to the base package and *then* the global environment and search path,
    # this will cause an infinite loop if we leave it in a super calling chain.
    # Instead, we must hop from the package namespace to the package environment,
    # instead of the parent environment of the package namespace (i.e., the package
    # imports).
    name <- getNamespaceName(env)
    if (name == "super" || name == "base") {
      # Traversing through the base or super namespace indicates we are in a
      # super calling chain that is making its way through package namespaces.
      parent.env(env)
    } else { parent.env(as.environment(paste0("package:", name))) }
  } else {
    parent.env(env)
  }
}

#' @export
multi_parent_env <- function(env, times) {
  while (times > 0) {
    env <- hop(env)
    times <- times - 1
  }
  env
}

