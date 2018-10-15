#' List all tables in a 'GraphPad Prism' '.pzfx' file.
#'
#' List all tables in a 'GraphPad Prism' '.pzfx' file.
#'
#' @param path Path to the '.pzfx' file
#'
#' @return a character string vector
#'
#' @export
#'
#' @examples
#' pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
#' pzfx_tables(pzfx_file)
pzfx_tables <- function(path) {
  xml <- xml2::read_xml(path)
  table_nodes <- xml2::xml_find_all(xml, ".//*[name()='Table' or name()='HugeTable']")
  tables <- sapply(table_nodes, function(t) xml2::xml_text(xml2::xml_child(t, ".//*[name()='Title']")))
  return(tables)
}
