#' Read one table from a 'GraphPad Prism' '.pzfx' file
#'
#' Read one table from a 'GraphPad Prism' '.pzfx' file
#'
#' @param path Path to the '.pzfx' file.
#' @param table Table to read. Either a string (the name of a table), or an
#'   integer (the position of the table). If neither argument specifies the
#'   table, defaults to the first table.
#' @param strike_action One of c("exclude", "keep", "star") or c("e", "k", "s"). Should stricken
#'   values in the original .pzfx be excluded, kept or labeled with a trailing "*". If a trailing
#'   "*" is added, the column will be of type character.
#'
#' @return a data frame
#'
#' @export
#'
#' @examples
#' pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
#' read_pzfx(pzfx_file, table = 1, strike_action="exclude")
read_pzfx <- function(path, table=1, strike_action="exclude") {
  # sanity check
  table_names <- pzfx::pzfx_tables(path)
  if (is.numeric(table)) {
    if (table > length(table_names)) stop("Table index out of range")
    this_idx <- table
  } else {
    table <- as.character(table)
    if (!table %in% table_names) stop(sprintf("Can't find %s in prism file", table))
    this_idx <- which(table_names == table)
    if (length(this_idx) > 1) {
      warning(sprintf("Multiple tables named %s, returning the first one only", table))
      this_idx <- this_idx[1]
    }
  }

  xml <- xml2::read_xml(path)
  table_nodes <- xml2::xml_find_all(xml, ".//*[name()='Table']")
  this_table <- xml2::as_list(table_nodes[[this_idx]])
  if (!"Title" %in% names(this_table)) stop("Can't work with this pzfx file, is it later than v6.0?")
  if (is.character(table) && table != this_table[["Title"]]) stop("Can't work with this pzfx file, is it later than v6.0?")

  x_format <- ""
  if ("XFormat" %in% names(attributes(this_table))) {
    x_format <- attributes(this_table)$XFormat
  }
  y_format <- ""
  if ("YFormat" %in% names(attributes(this_table))) {
    y_format <- attributes(this_table)$YFormat
  }

  col_lst <- list()
  for (i in seq_len(length(this_table))) {
    if (names(this_table)[i] == "XColumn") {
      this_col <- read_col(
        this_table[[i]],
        strike_action=strike_action,
        col_name="X",
        format=x_format)
      if (nrow(this_col) > 0) {
        col_lst[[length(col_lst) + 1]] <- this_col
      }
    } else if (names(this_table)[i] == "RowTitlesColumn") {
      this_col <- read_col(
        this_table[[i]],
        strike_action=strike_action,
        col_name="ROWTITLE",
        format="")
      if (nrow(this_col) > 0) {
        col_lst[[length(col_lst) + 1]] <- this_col
      }
    } else if (names(this_table)[i] == "YColumn") {
      this_col <- read_col(
        this_table[[i]],
        strike_action=strike_action,
        format=y_format)
      col_lst[[length(col_lst) + 1]] <- this_col
    }
  }

  max_len <- max(sapply(col_lst, nrow))
  long_col_lst <- lapply(col_lst, function(c) {
    while (nrow(c) < max_len) {
      c <- rbind(c, NA)
    }
    c
  })

  ret <- Reduce("cbind", long_col_lst)
  return(ret)
}
