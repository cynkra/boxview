


ansi_if <- function() cli::code_highlight("if (x) x") |> cli::ansi_substring(1, 3)
ansi_for <- function() cli::code_highlight("for (x in x) x") |> cli::ansi_substring(1, 4)
ansi_in <- function() cli::code_highlight("for (x in x) x") |> cli::ansi_substring(8, 9)
ansi_while <- function() cli::code_highlight("while (x) x") |> cli::ansi_substring(1, 6)
ansi_repeat <- function() cli::code_highlight("repeat x") |> cli::ansi_substring(1, 7)

header_if <- function(call, width) {
  header <- call
  header <- header[1:2]
  header[[1]] <- quote(.if)
  header_chr <- cli::code_highlight(rlang::expr_deparse(header, width = width))
  header_chr <- paste(header_chr, collapse = "\n")
  header_chr <- paste0(ansi_if(), cli::ansi_substring(header_chr, 4))
  header_chr <- strsplit(header_chr, "\n")[[1]]
}

header_for <- function(call, width) {
  header <- call
  header <- header[1:2]
  header[[2]] <- call("?", call[[2]], call[[3]])
  header[[1]] <- quote(.for)
  n_sym_chr <- nchar(as.character(call[[2]]))
  header_chr <- cli::code_highlight(rlang::expr_deparse(header, width = width))
  header_chr <- paste(header_chr, collapse = "\n")
  header_chr <- paste0(
    ansi_for(),
    cli::ansi_substring(header_chr, 5, 5 + n_sym_chr + 1),
    ansi_in(),
    cli::ansi_substring(header_chr, 5 + n_sym_chr + 3)
    )
  header_chr <- strsplit(header_chr, "\n")[[1]]
}

header_while <- function(call, width) {
  header <- call
  header <- header[1:2]
  header[[1]] <- quote(.while)
  header_chr <- cli::code_highlight(rlang::expr_deparse(header, width = width))
  header_chr <- paste(header_chr, collapse = "\n")
  header_chr <- paste0(ansi_while(), cli::ansi_substring(header_chr, 7))
  header_chr <- strsplit(header_chr, "\n")[[1]]
}

header_repeat <- function() {
  ansi_repeat()
}

header_fun <- function(call, width) {
  # not great, hacky and width is ignored
  call[[3]][[3]] <- quote(expr=...)
  header_chr <- rlang::expr_deparse(call, width = width)
  header_chr <- cli::code_highlight(header_chr)
  header_chr
}

code_to_box <- function(code, col) {
  width_nested_content <- max(cli::ansi_nchar(code))
  width_content <- width_nested_content + 4
  paddings <- strrep(" ", width_nested_content - cli::ansi_nchar(code))
  box <- paste0(col("| "), code, paddings, col(" |"))
  top_line <- col(sprintf("\U{250C}%s\U{2510}", strrep("\U{2500}", width_content - 2)))
  bottom_line <- col(sprintf("\U{2514}%s\U{2518}", strrep("\U{2500}", width_content - 2)))
  box <- c(top_line, box, bottom_line)
}

optimal_content_width <- function(call, width, optimization) {
  if (optimization == "weak") return(width)
  if (optimization == "medium") optimization <- "weak"
  code_first_pass <- unlist(lapply(call[-1], boxview_rec, width = width, optimization = optimization))
  width_nested_content <- max(cli::ansi_nchar(code_first_pass))
  width_content <- width_nested_content + 4
  max(width_content, width)
}

is_function_definition <- function(call) {
  if (!rlang::is_call(call, "<-") && !rlang::is_call(call, "=")) return(FALSE)
  if (length(call) != 3) return(FALSE)
  rlang::is_call(call[[3]], "function")
}
