# Wrap an expression in a function
# 
# This function takes the "body" part of a function and wraps it in
# a function. The return value depends on whether the function is created
# for its side effect with the tee operator. If the operator is \code{\%$\%}
# then the expression will be evaluated in a `with(., )` statement.
#
# @param body an expression which will serve as function body in single-argument
#    function with an argument names `.` (a dot)
# @param pipe a quoted magrittr pipe, which determines how the function is made.
# @param env The environment in which to contruct the function.
#
# @return a function of a single argument, named `.`.
wrap_function <- function(body, pipe, env)
{

  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  } else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  } else if (is_maybe_pipe(pipe)) {
    body <- wrap_maybe_body(body)
  }
  eval(call("function", as.pairlist(alist(.=)), body), env, env)
}

# Wrap an expression to safely evaluate via the maybe monad
# 
# This is a helper function to `wrap_function` that handles the maybe pipe.
# To ensure safe evaluation in a chain of pipes, this wraps the body of an
# expression in another expression that firstly checks if the placeholder
# is nothing, and if not it will unwrap the content of the placeholder and
# evaluate the expression within a `tryCatch` function. If the expression fails
# (i.e. if an error is thrown), the wrapper will return `nothing`. Otherwise,
# the wrapper will return `just(<return value of expression>)`.
#
# @param body an expression which will serve as function body in single-argument
#    function with an argument names `.` (a dot)
#
# @return a modified expression that wraps `body` in a maybe monad
wrap_maybe_body <- function(body) {
  bquote({
    if (is.nothing(.)) {
      return(.)
    }

    . <- unwrap(.)

    tryCatch(just(.(body)), error = function(e) nothing(message = e$message))
  })
}
