#' Parse one sub-column from a column of a table in a .pzfx file
#'
#' @param subcol A list converted from the XML that corresponds to one sub-column in a .pzfx file.
#' @param strike_action One of c("exclude", "keep", "star") or c("e", "k", "s"). Should stricken
#' values in the original .pzfx be excluded, kept or labeled with a trailing "*". If a trailing
#' "*" is added, the column will be of type character.
#' @return a vector of values in the sub-column
#'
#' @examples
#' \dontrun{
#' read_subcol(subcol)
#' }
read_subcol <- function(subcol, strike_action="exclude") {
  strike_action <- tolower(strike_action)
  if (!strike_action %in% c("exclude", "keep", "star", "e", "k", "s")) {
    stop("strike_action must be one of c('exclude', 'keep', 'star', 'e', 'k', 's')")
  }
  vals <- rep(NA, length(subcol))
  for (i in seq_len(length(subcol))) {
    val <- unlist(subcol[[i]])
    if (is.null(val)) val <- NA
    if ("Excluded" %in% names(attributes(subcol[[i]]))) {
      if (attr(subcol[[i]], "Excluded") == "1") {
        if (strike_action %in% c("exclude", "e")) {
          val <- NA
        } else if (strike_action %in% c("keep", "k")) {
          val <- val
        } else if (strike_action %in% c("star", "s")) {
          val <- paste0(val, "*")
        }
      }
    }
    vals[i] <- val
  }
  if (!strike_action %in% c("star", "s")) {
    suppressWarnings(new_vals <- as.numeric(vals))
    if (all(is.na(new_vals) == is.na(vals))) vals <- new_vals
  }
  return(vals)
}
