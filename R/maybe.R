# Implementation based on 
# https://kupac.gitlab.io/biofunctor/2019/05/25/maybe-monad-in-r/
maybe_types <- list(
  just = "Just",
  nothing = "Nothing"
)

just <- function(x) 
{
  if (is.maybe(x)) {
    return(x)
  } 

  result <- list(type = maybe_types$just, content = x)
  class(result) <- append(class(result), "maybe")
  
  result
}

nothing <- function(x = NULL) 
{
  if (is.maybe(x)) {
    # preserve previous error messages to track where
    # the failure was
    if (is_nothing(x))
      return(x)
    else
      return(nothing())
  }

  result <- list(type = maybe_types$nothing, content = x)
  class(result) <- "maybe"
  
  result
}

is.maybe <- function(x) 
{
  if (!inherits(x, "maybe")) {
    return(FALSE)
  }

  if (!is.null(x[["type"]])) {
    x[["type"]] %in% unlist(maybe_types)
  } else {
    FALSE
  }
}

is_nothing <- function(x)
{
  if (!is.maybe(x))
      return(FALSE)
  
  identical(x[["type"]], maybe_types$nothing)
}

print.maybe <- function(x, ...) 
{
  if (identical(x[["type"]], maybe_types$just)) {
    cat("Just:\n")
    print(x[["content"]], ...)
  } else {
    if (!is.null(x[["content"]]))
      cat("Nothing:", as.character(x[["content"]]), sep = "\n")
    else
      cat("Nothing\n")
  }
}

unwrap <- function(x, ...) 
{
  UseMethod("unwrap", x)
}

unwrap.default <- function(x, ...) 
{
  x
}

unwrap.maybe <- function(x, .default) 
{
  if (is_nothing(x)) {
    if (!missing(.default)) {
      .default 
    } else {
      stop("Cannot unwrap a value from Nothing (no default value provided with `.default`)", call. = FALSE)
    }
  } else {
    x[["content"]]
  }
}
