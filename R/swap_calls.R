swap_calls <- function (expr) {
  if (!is.call(expr))
    return(expr)
  is_if_assignment <- identical(expr[[1]], quote(`<-`)) &&
    is.call(expr[[3]]) && identical(expr[[3]][[1]], quote(`if`))
  if (is_if_assignment) {
    var <- expr[[2]]
    expr <- expr[[3]]
    yes_surrounded_by_curly <- is.call(expr[[3]]) && identical(expr[[3]][[1]],
                                                               quote(`{`))
    if (yes_surrounded_by_curly)
      expr[[3]][[length(expr[[3]])]] <- call("<-", var,
                                             expr[[3]][[length(expr[[3]])]])
    else expr[[3]] <- call("<-", var, expr[[3]])
    if (length(expr) == 4) {
      no_surrounded_by_curly <- is.call(expr[[4]]) && identical(expr[[4]][[1]],
                                                                quote(`{`))
      if (no_surrounded_by_curly)
        expr[[4]][[length(expr[[4]])]] <- call("<-",
                                               var, expr[[4]][[length(expr[[4]])]])
      else expr[[4]] <- call("<-", var, expr[[4]])
    }
    return(expr)
  }
  expr[] <- lapply(expr, swap_calls)
  expr
}
