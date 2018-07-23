#' Read pzfx files
#'
#' @param path Path to the pzfx file.
#' @param table Table to read. Either a string (the name of a table), or an
#'   integer (the position of the table). If neither argument specifies the
#'   table, defaults to the first table.
#'
#' @return a data frame
#'
#' @examples
#' \dontrun{
#' read_pzfx(path, table = 1)
#' }
#'
#' @export
read_pzfx <- function(path, table = 1) {
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
  this_table <- as_list(table_nodes[[this_idx]])

}
