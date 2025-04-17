swap_calls <- function (expr) {
  if (!is.call(expr))
    return(expr)

  expr_is_if_assignment <-
    identical(expr[[1]], quote(`<-`)) &&
    is.call(expr[[3]]) &&
    identical(expr[[3]][[1]], quote(`if`))

  if (expr_is_if_assignment) {
    target <- expr[[2]]
    if_expr <- expr[[3]]
    yes <- if_expr[[3]]
    yes_has_braces <- is.call(yes) && identical(yes[[1]], quote(`{`))
    if (yes_has_braces) {
      # replace last expr of yes clause
      yes[[length(yes)]] <- bquote(.(target) <- .(yes[[length(yes)]]))
      # the new last expr and other calls in the clause must be checked too
      yes <- swap_calls(yes)
    } else {
      yes <- bquote(.(target) <- .(yes))
      yes <- swap_calls(yes)
    }
    if_expr[[3]] <- yes

    if_has_else <- length(if_expr) == 4

    if (if_has_else) {
      no <- if_expr[[4]]
      no_has_braces <- is.call(no) && identical(no[[1]], quote(`{`))
      if (no_has_braces) {
        # replace last expr of yes clause
        no[[length(no)]] <- bquote(.(target) <- .(no[[length(no)]]))
        # the new last expr and other calls in the clause must be checked too
        no <- swap_calls(no)
      } else {
        no <- bquote(.(target) <- .(no))
        no <- swap_calls(no)
      }
      if_expr[[4]] <- no
    }
    return(if_expr)
  }
  expr[] <- lapply(expr, swap_calls)
  expr
}
