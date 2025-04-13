
boxview_rec <- function(call, col = identity, width = 200, optimization = "weak") {

  # A box of 40                                # width
  # ┌──────────────────────────────────────┐
  # | 36 for a regular call                |   # width - 4
  # | ┌───────────────┐ ┌───────────────┐  |
  # | | 17 outside    | | 13 inside     |  |   # floor((width - 5) / 2)
  # | └───────────────┘ └───────────────┘  |   # floor((width - 5) / 2) - 4
  # | ┌──────────────────────────────────┐ |
  # | | 36 outside, 32 inside            | |   # width - 4
  # | └──────────────────────────────────┘ |   # width - 8
  # └──────────────────────────────────────┘
  #
  # * When a call doesn't fit the provided width, its box is extended
  # * As a consequence the parent box is extended too and we take advantage of
  #   the new space there
  # * To take advantage of the extra space in this current box, we need
  #   to rerun with the new width specs, and since it's recursive, that's a
  #   lot of work and is very slow (though might be optimizationd with clever tricks).
  #   So we make it optional.

  # `width` is the width of the box
  # The call content `with - 4` : regular calls, or nested boxes
  # if/else boxes are (width- 4) / 2, or width/2 - 2, rounded

  # if we don't run twice, some calls will stretch the boxes and calls above won't benefit from it
  # but it'll be much faster

  width_content <- width - 4

  if (rlang::is_call(call, "{")) {
    width_content <- optimal_content_width(call, width_content, optimization)
    code <- unlist(lapply(call[-1], boxview_rec, width = width_content, optimization = optimization))
    box <- code_to_box(code, col)
    return(box)
  }


  # if -------------------------------------------------------------------------
  if (rlang::is_call(call, "if")) {
    if_has_no <- length(call) == 4

    yes <- call[[3]]
    # wrap yes in {}
    if (!rlang::is_call(yes, "{")) yes <- bquote({.(yes)})

    if (if_has_no) {
      width_nested_box <- floor((width_content - 1) / 2)

      no <- call[[4]]
      # wrap no in {}
      if (!rlang::is_call(no, "{")) no <- bquote({.(no)})

      # boxes
      yes <- boxview_rec(yes, cli::col_green, width = width_nested_box, optimization = optimization)
      no <- boxview_rec(no, cli::col_red, width = width_nested_box, optimization = optimization)

      # add empty space at the bottom of the shortest box
      if (length(yes) > length(no)) {
        length(no) <- length(yes)
      } else {
        length(yes) <- length(no)
      }
      yes[is.na(yes)] <- strrep(" ", cli::ansi_nchar(yes[[1]]))
      no[is.na(no)] <- strrep(" ", cli::ansi_nchar(no[[1]]))
      box <- paste(yes, no)
    } else {
      yes <- boxview_rec(yes, cli::col_green, width = width_content, optimization = optimization)
      yes[is.na(yes)] <- strrep(" ", cli::ansi_nchar(yes[[1]]))
      box <- yes
    }

    header_chr <- header_if(call, width_content)
    box <- c(header_chr, box)
    return(box)
  }

  # for ------------------------------------------------------------------------
  if (rlang::is_call(call, "for")) {
    body <- call[[4]]
    if (!rlang::is_call(body, "{")) body <- bquote({.(body)})
    box <- boxview_rec(body, cli::col_blue, width = width_content, optimization = optimization)
    header_chr <- header_for(call, width_content)
    box <- c(header_chr, box)
    return(box)
  }

  # while ------------------------------------------------------------------------
  if (rlang::is_call(call, "while")) {
    body <- call[[3]]
    if (!rlang::is_call(body, "{")) body <- bquote({.(body)})
    box <- boxview_rec(body, cli::col_blue, width = width_content, optimization = optimization)
    header_chr <- header_while(call, width_content)
    box <- c(header_chr, box)
    return(box)
  }

  # repeat ------------------------------------------------------------------------
  if (rlang::is_call(call, "repeat")) {
    body <- call[[2]]
    if (!rlang::is_call(body, "{")) body <- bquote({.(body)})
    box <- boxview_rec(body, cli::col_blue, width = width_content, optimization = optimization)
    header_chr <- header_repeat()
    box <- c(header_chr, box)
    return(box)
  }

  if (is_function_definition(call)) {
    header_chr <- header_fun(call, width_content)
    body <- call[[3]][[3]]
    box <- boxview_rec(body, width = width_content, optimization = optimization)
    box <- c(header_chr, box)
    return(box)
  }

  # regular calls or symbols
  repeat {
    code <- try(cli::code_highlight(styler::style_text(rlang::expr_deparse(call, width = width_content))), silent = TRUE)
    stripped <- cli::ansi_strip(code)
    if (!inherits(code, "try-error")) {
      # FIXME: we should just color the function
      if (any(startsWith(stripped[[1]], c("stop(", "abort(", "rlang::abort(", "stopifnot(")))) {
        fun <- c("stop", "abort", "rlang::abort", "stopifnot")[startsWith(stripped[[1]], c("stop(", "abort(", "rlang::abort(", "stopifnot("))]
        code[[1]] <- paste0(cli::bg_red(cli::col_black(fun)), cli::ansi_substring(code[[1]], cli::ansi_nchar(fun) + 1))
      } else if (any(startsWith(stripped[[1]], "return("))) {
        code[[1]] <- paste0(cli::bg_green(cli::col_black("return")), cli::ansi_substring(code[[1]], 7))
      } else if (any(startsWith(stripped[[1]], c("warning(", "warn(", "rlang::warn(")))) {
        fun <- c("warning", "warn", "rlang::warn")[startsWith(stripped[[1]], c("warning(", "warn(", "rlang::warn("))]
        code[[1]] <- paste0(cli::bg_yellow(cli::col_black(fun)), cli::ansi_substring(code[[1]], cli::ansi_nchar(fun) + 1))
      }
      return(code)
    }
    width <- width + 1
  }

}
