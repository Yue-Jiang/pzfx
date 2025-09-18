#' Write one or more tables to a GraphPad Prism .pzfx file
#'
#' All parameters except path and notes can be of length 1, or the length of the list of input tables.
#'
#' @param x Data frame or named list of data frames to include as Prism tables.
#' @param path Path to output file.
#' @param row_names Logical or logical vector: include row names as row titles?
#' @param x_col Column index or name(s) for X column (0 for none).
#' @param x_err Column index or name(s) for X error (0 for none).
#' @param n_digits Number of decimal places to display for numeric data.
#' @param notes Notes table(s) with columns Name and Value.
#' @param subcolumns Number of subcolumns for Y data, or "SDN".
#' @param subcolumn_suffix Regex or string identifying grouped subcolumns.
#' @return Invisibly returns `x`.
#' @export
write_pzfx <- function(x, path,
                       row_names = TRUE,
                       x_col = NA,
                       x_err = NA,
                       n_digits = 2,
                       notes = NA,
                       subcolumns = 1,
                       subcolumn_suffix = "") {

  ## ------------------------
  ## Helper functions
  ## ------------------------

  utc_iso8601 <- function(t = Sys.time()) {
    format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
  }

  require_length <- function(arg, n, name) {
    if (length(arg) == 1) arg <- rep(arg, n)
    if (length(arg) != n)
      stop(sprintf("Argument '%s' must have length 1 or %d", name, n), call. = FALSE)
    arg
  }

  coerce_to_list_of_dfs <- function(obj, prefix) {
    if (inherits(obj, c("data.frame", "matrix"))) {
      setNames(list(obj), paste(prefix, 1))
    } else if (is.list(obj)) {
      if (is.null(names(obj))) names(obj) <- paste(prefix, seq_along(obj))
      bad <- !vapply(obj, inherits, logical(1), what = c("data.frame", "matrix"))
      if (any(bad)) stop(sprintf("These %s elements are not data.frames/matrices: %s",
                                 prefix, paste(names(obj)[bad], collapse = ", ")), call. = FALSE)
      obj
    } else if (length(obj) == 1 && is.na(obj)) {
      NULL
    } else {
      stop(sprintf("Cannot process %s of class %s", prefix, paste(class(obj), collapse = ", ")), call. = FALSE)
    }
  }

  normalise_col_arg <- function(arg, lst, label) {
    arg <- require_length(arg, length(lst), label)
    arg[is.na(arg)] <- 0
    if (is.character(arg)) {
      arg <- vapply(seq_along(arg), function(i) {
        if (identical(arg[i], "")) return(0L)
        idx <- which(colnames(lst[[i]]) == arg[i])
        if (!length(idx)) {
          warning(sprintf("Column '%s' not in table '%s'; ignored as %s",
                          arg[i], names(lst)[i], label), call. = FALSE)
          0L
        } else idx[1]
      }, integer(1))
    }
    as.integer(arg)
  }

  subcol_helper <- function(v) {
    lapply(as.vector(v), function(e) {
      e <- as.character(e)
      if (is.na(e)) return(list("d" = list(NA_character_)))
      if (grepl("\\*$", e)) {
        val <- sub("\\*$", "", e)
        return(list("d" = structure(list(val), Excluded = "1")))
      }
      list("d" = list(e))
    })
  }

  build_xcol_structure <- function(df, x_idx, x_err_idx, n_digits) {
    if (x_idx == 0) return(list())
    if (x_err_idx == 0) {
      list(XColumn = structure(
        list(Title = list(names(df)[x_idx]),
             Subcolumn = subcol_helper(df[[x_idx]])),
        Width = "89", Decimals = as.character(n_digits), Subcolumns = "1"
      ))
    } else {
      subs <- lapply(c(x_idx, x_err_idx), function(cidx) subcol_helper(df[[cidx]]))
      names(subs) <- rep("Subcolumn", length(subs))
      list(XColumn = structure(
        append(list(Title = list(names(df)[x_idx])), subs),
        Width = "120", Decimals = as.character(n_digits), Subcolumns = "2"
      ))
    }
  }

  generate_subcolumns <- function(df, expected_count, subcolumn_suffix, n_digits) {
    grouping_factor <- sub(subcolumn_suffix, "", names(df))
    grouped <- split.default(df, grouping_factor)
    grouped <- grouped[unique(grouping_factor)]
    final <- lapply(grouped, function(gdf) {
      if (ncol(gdf) < expected_count) {
        pad <- matrix(NA, nrow = nrow(gdf), ncol = expected_count - ncol(gdf))
        gdf <- cbind(gdf, pad)
      }
      subs <- lapply(seq_len(ncol(gdf)), function(c) subcol_helper(gdf[[c]]))
      names(subs) <- rep("Subcolumn", length(subs))
      structure(append(list(Title = list(names(grouped)[[1]])), subs),
                Width = as.character(89 * expected_count),
                Decimals = as.character(n_digits),
                Subcolumns = as.character(expected_count))
    })
    names(final) <- rep("YColumn", length(final))
    final
  }

  table_lst <- function(x_lst, row_names, x_col, x_err, n_digits, subcolumns, subcolumn_suffix) {
    lapply(seq_along(x_lst), function(i) {
      df <- x_lst[[i]]
      xi <- x_col[i]; xe <- x_err[i]; subc <- subcolumns[i]; suffix <- subcolumn_suffix[i]
      ndig <- n_digits[i]
      y_format <- "replicates"; table_type <- "OneWay"; x_format <- "none"
      if (identical(subc, "SDN")) { y_format <- "SDN"; subc <- 3L }
      xcols <- build_xcol_structure(df, xi, xe, ndig)
      if (length(xcols)) { x_format <- if (xe == 0) "numbers" else "error"; table_type <- "XY" }
      df_y <- df[, setdiff(seq_len(ncol(df)), c(xi, xe)), drop = FALSE]
      ycols <- generate_subcolumns(df_y, as.integer(subc), suffix, ndig)
      meta <- list(Title = list(names(x_lst)[i]))
      if (row_names[i]) {
        meta$RowTitlesColumn <- structure(list(Subcolumn = subcol_helper(row.names(df_y))), Width = "39")
      }
      structure(append(meta, append(xcols, ycols)),
                ID = sprintf("Table%d", i - 1),
                XFormat = x_format, YFormat = y_format,
                Replicates = as.character(subc),
                TableType = table_type, EVFormat = "AsteriskAfterNumber")
    }) |> `names<-`(rep("Table", length(x_lst)))
  }

  info_lst <- function(notes_lst) {
    lapply(seq_along(notes_lst), function(i) {
      df <- notes_lst[[i]]
      notes_rows <- df[df$Name == "Notes", , drop = FALSE]
      constants <- df[df$Name != "Notes", , drop = FALSE]
      notes_block <- list(Title = list(names(notes_lst)[i]),
                          Notes = list(Font = structure(list(), Color = "#000000", Face = "Helvetica")))
      if (nrow(notes_rows) > 0) {
        note_values <- notes_rows$Value
        notes_block$Notes <- list(Font = structure(
          lapply(note_values, function(x) list(x, list(BR = list()))),
          Color = "#000000", Face = "Helvetica"
        ))
      }
      if (nrow(constants) > 0) {
        const_blocks <- sapply(seq_len(nrow(constants)), function(j) {
          list(Constant = list(Name = list(constants$Name[j]), Value = list(constants$Value[j])))
        })
        notes_block <- append(notes_block, const_blocks)
      }
      structure(notes_block, ID = sprintf("Info%d", i - 1))
    }) |> `names<-`(rep("Info", length(notes_lst)))
  }

  ## ------------------------
  ## Main body
  ## ------------------------

  # Defaults & coercion
  if (length(notes) == 1 && is.na(notes)) {
    notes <- list(data.frame(Name = "Notes", Value = NA_character_))
  }
  n_lst <- coerce_to_list_of_dfs(notes, "Project Info")
  x_lst <- coerce_to_list_of_dfs(x, "Data")

  # Warnings for non-numeric
  for (nm in names(x_lst)) {
    bad_cols <- names(x_lst[[nm]])[!vapply(x_lst[[nm]], is.numeric, logical(1))]
    if (length(bad_cols)) {
      warning(sprintf(
        "Table '%s' has non‑numeric columns: %s. Prism will ignore them; trailing * marks exclusions.",
        nm, paste(bad_cols, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # Normalise vector arguments to correct length
  row_names        <- require_length(row_names, length(x_lst), "row_names")
  subcolumns       <- require_length(subcolumns, length(x_lst), "subcolumns")
  subcolumn_suffix <- require_length(subcolumn_suffix, length(x_lst), "subcolumn_suffix")
  n_digits         <- require_length(n_digits, length(x_lst), "n_digits")

  # Column index handling
  x_col <- normalise_col_arg(x_col, x_lst, "x_col")
  x_err <- normalise_col_arg(x_err, x_lst, "x_err")


  ## ------------------------
  ## Assemble XML structure
  ## ------------------------

  base_lst <- list(
    GraphPadPrismFile = structure(list(
      Created = list(
        OriginalVersion = structure(list(),
                                    CreatedByProgram = "GraphPad Prism",
                                    CreatedByVersion = "6.0f.254",
                                    Login = "",
                                    DateTime = utc_iso8601()
        )
      )
    ))
  )
  
  # Optional notes
  if (!is.null(n_lst)) {
    base_lst$GraphPadPrismFile$InfoSequence <- lapply(seq_along(n_lst), function(i) {
      ref <- structure(list(), ID = sprintf("Info%d", i - 1))
      if (i == 1) attr(ref, "Selected") <- "1"
      ref
    }) |> `names<-`(rep("Ref", length(n_lst)))

    base_lst$GraphPadPrismFile <- append(
      base_lst$GraphPadPrismFile,
      info_lst(n_lst)
    )
  }

  # Table sequence and table content
  base_lst$GraphPadPrismFile$TableSequence <- lapply(seq_along(x_lst), function(i) {
    ref <- structure(list(), ID = sprintf("Table%d", i - 1))
    if (i == 1) attr(ref, "Selected") <- "1"
    ref
  }) |> `names<-`(rep("Ref", length(x_lst)))

  base_lst$GraphPadPrismFile <- append(
    base_lst$GraphPadPrismFile,
    table_lst(x_lst, row_names, x_col, x_err, n_digits, subcolumns, subcolumn_suffix)
  )
  attr(base_lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
  ## ------------------------
  ## Write to file
  ## ------------------------

  xml <- xml2::as_xml_document(base_lst)
  xml2::write_xml(xml, path)
  invisible(x)
}
