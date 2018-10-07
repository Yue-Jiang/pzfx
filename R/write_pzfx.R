#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file
#'
#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file. A table can be a 'matrix',
#'   a 'data.frame', or a 'tibble'. All elements of the table be numeric.
#'
#' @param x Input table or named list of tables that will be 'Data Tables' in the '.pzfx' file
#' @param path Path to the output '.pzfx' file.
#' @param row_names Logical. If row names of the input table be preserved and become row titles in
#'   the output '.pzfx' file. If the length is greater than 1, it must match the length of list of
#'   input tables. Default: TRUE.
#' @param x_col 1-based column index or name of the column to be used as the 'X' column. If the
#'   length is greater than 1, it must match the length of list of input tables. All other columns
#'   in the input tables will be treated as "Y" columns in the output '.pzfx' file. Default: NA
#'
#' @return write_pzfx returns the input x invisibly.
#'
#' @export
#'
#' @examples
#' pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
#' df <- read_pzfx(pzfx_file, table = 1, strike_action = "exclude")
#' write_pzfx(df, path = tempfile(fileext = ".pzfx"), row_names = TRUE)
write_pzfx <- function(x, path, row_names=TRUE, x_col=NA) {
  # figure out if x is a single table or multiple of them
  if (inherits(x, c("data.frame", "matrix"))) {
    x_lst <- list("Data 1"=x)
  } else if (inherits(x, "list")) {
    x_lst <- x
    if (is.null(names(x_lst))) names(x_lst) <- paste("Data", seq_len(length(x_lst)))
    are_dfs <- sapply(x_lst, function(x) inherits(x, c("data.frame", "matrix")))
    if (any(!are_dfs)) stop(sprintf("These elements are not data frame: %s",
                                    paste(names(x_lst)[!are_dfs], collapse=", ")))
  } else {
    stop(sprintf("Cannot process x of class %s", paste(class(x), collapse=", ")))
  }
  # make sure all elements are numeric
  are_nums <- sapply(x_lst, function(x) all(sapply(x, is.numeric)))
  if (any(!are_nums)) stop(sprintf("These elements are not all numeric: %s",
                                   paste(names(x_lst)[!are_nums], collapse=", ")))
  # make sure row_names matches the length of x_lst
  if (length(row_names) == 1) row_names <- rep(row_names, length(x_lst))
  if (length(row_names) != length(x_lst)) {
    stop("Argument 'row_names' can only be of length 1 or the length of 'x'")
  }
  # convert other kinds of x_col specifications to a vector of integers
  if (length(x_col) == 1) x_col <- rep(x_col, length(x_lst))
  if (length(x_col) != length(x_lst)) {
    stop("Argument 'x_col' can only be of length 1 or the length of 'x'")
  }
  x_col[is.na(x_col)] <- 0
  if (is.numeric(x_col)) {
    x_col <- as.integer(x_col)
    if (any(x_col > sapply(x_lst, ncol))) {
      vio <- which(x_col > sapply(x_lst, ncol))
      stop(sprintf("Not enough columns for table %s", paste(names(x_lst)[vio], collapse=", ")))
    }
  }
  if (is.character(x_col)) {
    for (i in seq_len(length(x_col))) {
      idx <- which(colnames(x_lst[[i]]) == x_col[i])
      if (length(idx) == 0) {
        warning(sprintf(
          "Column %s is not in table %s, not used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- 0
      } else if (length(idx) > 1) {
        warning(sprintf(
          "Column %s has multiple occurance in table %s, only the first one used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- idx[1]
      } else {
        x_col[i] <- idx
      }
    }
  }

  lst <- base_lst()
  lst$GraphPadPrismFile$TableSequence <- table_seq_lst(x_lst)
  lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, table_lst(x_lst, row_names, x_col))
  attr(lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
  xml <- xml2::as_xml_document(lst)
  xml2::write_xml(xml, path)
  invisible(x)
}

# The basic list for a pzfx xml
base_lst <- function() {
  lst <- list(
    "GraphPadPrismFile"=list(
      "Created"=list(
        "OriginalVersion"=structure(
          list(),
          CreatedByProgram="GraphPad Prism",
          CreatedByVersion="6.0f.254",
          Login="",
          DateTime=strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
        )
      ),
      "InfoSequence"=list(
        "Ref"=structure(
          list(),
          "ID"="Info0",
          "Selected"="1"
        )
      ),
      "Info"=structure(
        list(
          "Title"=list("Project info 1"),
          "Notes"=list(""),
          "Constant"=list("Name"=list("Experiment Date"), "Value"=list("")),
          "Constant"=list("Name"=list("Experiment ID"), "Value"=list("")),
          "Constant"=list("Name"=list("Notebook ID"), "Value"=list("")),
          "Constant"=list("Name"=list("Project"), "Value"=list("")),
          "Constant"=list("Name"=list("Experimenter"), "Value"=list("")),
          "Constant"=list("Name"=list("Protocol"), "Value"=list(""))
        ),
        "ID"="Info0"
      )
      # Then, the "TableSequence" list goes here
      # Then, all "Table" lists go here
    )
  )
  return(lst)
}

# "TableSequence" element of the list for a pzfx xml
# Number of Refs corresponds to number of tables
table_seq_lst <- function(x_lst) {
  ret <- lapply(seq_len(length(x_lst)), function(i) {
    ref <- structure(
      list(),
      "ID"=sprintf("Table%d", i - 1)
    )
    if (i == 1) attr(ref, "Selected") <- "1"
    return(ref)
  })
  names(ret) <- rep("Ref", length(x_lst))
  return(ret)
}

# "Table" elements of the list for a pzfx xml
# As many tables as you have
# Currently only supports pzfx's "Column" type of tables
table_lst <- function(x_lst, row_names, x_col) {
  if (length(x_lst) != length(row_names)) {
    stop("Argument 'row_names' can only be of the same length as 'x_lst'")
  }
  if (length(x_lst) != length(x_col)) {
    stop("Argument 'x_col' can only be of the same length as 'x_lst'")
  }
  if (!is.integer(x_col)) {
    stop("Argument 'x_col' can only be of type 'integer'")
  }
  subcol_helper <- function(v) {
    v <- as.vector(v)
    lapply(v, function(e) list("d"=list(as.character(e))))
  }
  ret <- lapply(seq_len(length(x_lst)), function(i) {
    this_df <- x_lst[[i]]
    cols <- lapply(seq_len(ncol(this_df)), function(c) {
      structure(
        list(
          "Title"=list(colnames(this_df)[c]),
          "Subcolumn"=subcol_helper(this_df[, c, drop=TRUE])
        ),
        Width="89",
        Decimals="0",
        Subcolumns="1"
      )
    })
    names(cols) <- rep("YColumn", ncol(this_df))
    names(cols)[x_col[i]] <- "XColumn"
    one_table_lst <- list("Title"=list(names(x_lst)[i]))
    if (row_names[i]) {
      one_table_lst[["RowTitlesColumn"]] <- structure(
        list("Subcolumn"=subcol_helper(row.names(this_df))),
        Width="39"
      )
    }
    one_table <- structure(
      append(one_table_lst, cols),
      ID=sprintf("Table%d", i - 1),
      XFormat="none",
      #YFormat="replicates",
      #Replicates="1",
      TableType="OneWay",
      EVFormat="AsteriskAfterNumber"
    )
    if (x_col[i] > 0) {
      attr(one_table, "XFormat") <- "numbers"
      attr(one_table, "YFormat") <- "replicates"
      attr(one_table, "Replicates") <- "1"
      attr(one_table, "TableType") <- "XY"
    }
    return(one_table)
  })
  names(ret) <- rep("Table", length(x_lst))
  return(ret)
}
