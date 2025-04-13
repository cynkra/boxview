#' Display code logic in nested boxes
#'
#' `boxview()` displays the code of a function in nested boxes, showing the
#' 'yes' and 'no' clauses of `if` calls side by side. It often makes code easier to
#' skim than the traditional way.
#'
#' @param fun A function
#' @param width The desired width, the output will be wider if the we can't
#'   make it narrow enough by wrapping code, and will be narrower if we don't
#'   need the width when not wrapping at all
#' @param optimization The level of space optimization, to wrap calls only
#'   when needed. `boxview()` is quite slow, and strong optimization can take
#'   a long time for big functions. You might not need higher levels most of
#'   the time, especially with sufficient width where the difference is often
#'   barely noticeable.
#'
#' @return a 'boxview' object that prints the boxed code to the console.
#' @export
#'
#' @examples
#' \dontrun{
#' boxview(ave)
#' boxview(ave, width = 10)
#' boxview(ave, width = 10, optimization = "medium")
#' boxview(interaction)
#' boxview(data.frame)
#' }
boxview <- function(fun, width = 200, optimization = c("weak", "medium", "strong")) {
  optimization <- rlang::arg_match(optimization)
  header_chr <- cli::code_highlight(deparse(args(fun)))
  header_chr <- header_chr[-length(header_chr)]
  code <- boxview_rec(swap_calls(body(fun)), width = width, optimization = optimization)
  code <- c(header_chr, code)
  structure(code, class = c("boxview", "character"))
}






