# Implementation based on 
# https://kupac.gitlab.io/biofunctor/2019/05/25/maybe-monad-in-r/
maybe_types <- list(
  just = "Just",
  nothing = "Nothing"
)

#' Just something
#' 
#' Wraps any object in a safe setting. Moreover, `just()` implements 
#' the join functionality of a monad, so `just(just(a))` will be `just(a)`.
#'
#' @param x Any R object
#' @return A Maybe object that contains `x`
#' @export
just <- function(x) 
{
  if (is.maybe(x)) {
    return(x)
  } 

  result <- list(type = maybe_types$just, content = x)
  class(result) <- append(class(result), "maybe")
  
  result
}

#' Nothing
#'
#' A Maybe object that contains nothing. If an error message is present,
#' it will carry that error message, but that is it. `nothing()` also 
#' implements the join property of a monad, so `nothing(nothing(message = err))`
#' will be `nothing(message = err)`.
#' 
#' Oftentimes programmers will use `NULL` to indicate some form of failure.
#' `nothing()` is *not* the same thing as `NULL`. `just()` can contain `NULL`,
#' but by design `nothing()` cannot hold anything. This is important to note 
#' when \code{\link{unwrap}}ping values from a maybe pipe chain.
#' 
#' @details `just(nothing())` will return `nothing()` as implemented in the 
#' MonadPlus typeclass. Similarly, `nothing(just(a))` will be `nothing()`. It's important
#' to note that the content of `just()` will *not* be treated as an error message.
#' 
#' @param x Any R object, usually a Maybe object.
#' @param message An error message. Purposefully a different parameter from x 
#'     so that `just(a) %?>% nothing()` is `nothing()`
#' @return A `nothing` object
#'
#' @export
nothing <- function(x = NULL, message = NULL) 
{
  if (is.maybe(x)) {
    # preserve previous error messages to track where
    # the failure was
    if (is.nothing(x))
      return(x)
    else
      return(nothing())
  }

  result <- list(type = maybe_types$nothing, content = message)
  
  # Multiclass to take advantage of is method dispatch
  class(result) <- c("maybe", "nothing")
  
  result
}

#' Is an object a Maybe object?
#'
#' An S3 method to check if an R object is a Maybe object
#'
#' @param x Any R object
#' @return logical
#' 
#' @export
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

#' Is an object Nothing?
#'
#' An S3 method to check if an R object is a Nothing instance
#' of a Maybe object
#'
#' @param x Any R object
#' @return logical
#' 
#' @export
is.nothing <- function(x)
{
  is.maybe(x) && inherits(x, "nothing")
}

#' @export
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

#' Unwrap values from Maybe context
#' 
#' After using a maybe pipe chain, it would be usefulto extract 
#' the resulting value from the chain. For `just()` Maybe objects,
#' this is straightforward: use a **regular** pipe (`%>%`)
#' to get the value. For `nothing()` objects, you need to define
#' a default value using the `.default` parameter. You cannot unwrap
#' a `nothing()` object without this defined, else an error will be thrown.
#' 
#' Unwrapping a non-Maybe object just returns that object since no Maybe
#' context is present.
#' 
#' @param x Any R object, usually a Maybe object used during a maybe pipe chain
#' @param .default The default value to use if `x` is `nothing()`. If
#'   `.default` is not defined and `x` is `nothing()`, an error will be thrown.
#' @param ... Currently unhandled, but present for expansion and S3 compatibility
#'
#' @return The unwrapped value or an error, depending on the function parameters   
#' 
#' @examples 
#' 1:5 %?>%
#'   sum() %>%
#'   unwrap()
#'   
#' "bad_file.csv" %?>%
#'   read.csv() %>%
#'   unwrap(.default = NULL)
#' 
#' @rdname unwrap
#' @export
unwrap <- function(x, ...) 
{
  UseMethod("unwrap", x)
}

#' @rdname unwrap
#' @export
unwrap.default <- function(x, ...) 
{
  x
}

#' @rdname unwrap
#' @export
unwrap.maybe <- function(x, .default, ...) 
{
  if (is.nothing(x)) {
    if (!missing(.default)) {
      .default
    } else {
      stop("Cannot unwrap a value from Nothing (no default value provided with `.default`)", call. = FALSE)
    }
  } else {
    x[["content"]]
  }
}
