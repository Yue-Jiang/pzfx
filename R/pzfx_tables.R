#' List all tables in a prism graphpad pzfx file.
#'
#' List all tables in a prism graphpad pzfx file.
#'
#' @param path Path to the pzfx file
#'
#' @return a character string vector
#'
#' @examples
#' \dontrun{
#' pzfx_tables(path)
#' }
#'
#' @export
pzfx_tables <- function(path) {
  xml <- xml2::read_xml(path)
  table_nodes <- xml2::xml_find_all(xml, ".//*[name()='Table']")
  tables <- sapply(table_nodes, function(t) xml2::xml_text(xml2::xml_child(t, ".//*[name()='Title']")))
  return(tables)
}
