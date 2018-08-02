#' Parse one column
#'
#' @param col A list converted from the XML that corresponds to one column in a pzfx file.
#' @param strike_action One of c("exclude", "keep", "star") or c("e", "k", "s"). Should striked
#'   values in the original pzfx be excluded, kept or labeled with a trailing "*". If a trailing
#'   "*" is added, the column will be of type character.
#' @param format A character string reflecting the XFormat or YFormat attribute of a pzfx table.
#' @param col_name A character string for default base column name. Will be disregarded if column
#'   has title.
#' @param tidify Logical. Should output data frame be tidified. Currently
#'   not implemented.
#'
#' @return  a data frame with parsed column data..
#'
#' @examples
#' \dontrun{
#' read_col(col_list)
#' }
read_col <- function(
  col,
  strike_action="exclude",
  format="",
  col_name="",
  tidify=FALSE) {
  if ("Title" %in% names(col)) {
    col_name <- unlist(col[["Title"]])
  }
  subcol_lst <- list()
  for (i in seq_len(length(col))) {
    if (names(col)[i] == "Subcolumn") {
      this_subcol <- pzfx:::read_subcol(col[[i]], strike_action=strike_action)
      subcol_lst[[length(subcol_lst) + 1]] <- this_subcol
    }
  }

  if (length(subcol_lst) == 1) {
    col_names <- col_name
  } else if (format == "error") {
    col_names <- paste0(col_name, c("_X", "_ERROR"))
  } else if (format == "replicates") {
    col_names <- paste(col_name, seq_len(length(subcol_lst)), sep="_")
  } else if (format == "SDN") {
    col_names <- paste0(col_name, c("_MEAN", "_SD", "_N"))
  } else if (format == "SEN") {
    col_names <- paste0(col_name, c("_MEAN", "_SEM", "_N"))
  } else if (format == "CVN") {
    col_names <- paste0(col_name, c("_MEAN", "_CV", "_N"))
  } else if (format == "SD") {
    col_names <- paste0(col_name, c("_MEAN", "_SD"))
  } else if (format == "SE") {
    col_names <- paste0(col_name, c("_MEAN", "_SE"))
  } else if (format == "CV") {
    col_names <- paste0(col_name, c("_MEAN", "_CV"))
  } else if (format == "SD") {
    col_names <- paste0(col_name, c("_MEAN", "_SD"))
  } else if (format == "low-high") {
    col_names <- paste0(col_name, c("_MEAN", "_PLUSERROR", "_MINUSERROR"))
  } else if (format == "upper-lower-limits") {
    col_names <- paste0(col_name, c("_MEAN", "_UPPERLIMIT", "_LOWERLIMIT"))
  } else {
    stop("Sorry, don't know how to parse column format.")
  }

  names(subcol_lst) <- col_names
  max_len <- max(sapply(subcol_lst, length))
  long_subcol_lst <- lapply(subcol_lst, function(s) {
    length(s) <- max_len
    s
  })

  ret <- as.data.frame(long_subcol_lst, stringsAsFactors=FALSE)
  names(ret) <- col_names
  return(ret)
}
