#' Parse one column
#'
#' @param col A list converted from the XML that corresponds to one column in a pzfx file.
#' @param strike_action One of c("exclude", "keep", "star") or c("e", "k", "s"). Should striked
#' values in the original pzfx be excluded, kept or labeled with a trailing "*". If a trailing
#' "*" is added, the column will be of type character.
#' @return a list with two elements, a string character col_name and a data.frame col_data.
#'
#' @examples
#' \dontrun{
#' read_col(col_list)
#' }
read_col <- function(col, strike_action="exclude") {
  col_name <- ""
  if ("Title" %in% names(col)) {
    col_name <- unlist(col[["Title"]])
  }
  for (i in seq_len(length(col))) {
    if (names(col)[i] == "Subcolumn") {
      this_subcol <- pzfx::read_subcol(col[[i]], strike_action=strike_action)
    }
  }
}
