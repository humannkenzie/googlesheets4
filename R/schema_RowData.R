# creates an array of instances of RowData
as_RowData <- function(df, col_names = TRUE) {
  df_cells <- purrr::modify(df, as_CellData)
  df_rows <- pmap(df_cells, list)
  if (col_names) {
    df_rows <- c(list(as_CellData(names(df))), df_rows)
  }
  map(df_rows, ~ list(values = unname(.x)))
}

as_RowDataAndFormat <- function(df, 
                                col_names = TRUE, 
                                cell_formats = NULL) {
  # Helper to apply formatting to a cell
  format_cell <- function(value, format_data) {
    cell <- list(userEnteredValue = list())

    # Handle NA values explicitly
    if (is.na(value)) {
      cell$userEnteredValue <- NULL  # Google Sheets will treat it as an empty cell
    } else if (is.numeric(value)) {
      cell$userEnteredValue$numberValue <- value
    } else if (inherits(value, "Date")) {
      cell$userEnteredValue$numberValue <- as.numeric(value)
    } else if (inherits(value, "POSIXt")) {
      cell$userEnteredValue$numberValue <- as.numeric(value) / 86400 + 25569
    } else if (is.logical(value)) {
      cell$userEnteredValue$boolValue <- value
    } else {
      cell$userEnteredValue$stringValue <- as.character(value)
    }

    # Apply formatting if available
    if (!is.null(format_data)) {
      cell$userEnteredFormat <- format_data
    }

    return(cell)
  }

  # Convert each cell in the data frame
  df_cells <- purrr::imap(df, function(column, col_index) {
    purrr::imap(column, function(value, row_index) {
      format_data <- NULL
      # Convert col_index to a numeric index if it’s a name.
      num_col_index <- if (is.character(col_index)) {
        match(col_index, names(df))
      } else {
        col_index
      }

    if (!is.null(cell_formats) && 
        row_index <= nrow(cell_formats) && 
        num_col_index <= ncol(cell_formats) && 
        !is.null(cell_formats[row_index, num_col_index])) {
        # Directly assign the formatting object stored in cell_formats.
        format_data <- cell_formats[row_index,num_col_index][[1]]
      } 
      format_cell(value, format_data)
    })
  })
  
  # Convert to rows
  df_rows <- purrr::transpose(df_cells)

  # Add column names as the first row if required
  if (col_names) {
    col_name_cells <- purrr::map(names(df), ~ list(
      userEnteredValue = list(stringValue = .x),
      userEnteredFormat = list(
        textFormat = list(bold = TRUE)  # Column headers are bold by default
      )
    ))
    df_rows <- c(list(col_name_cells), df_rows)
  }

  # Format for Google Sheets API
  formatted_rows <- purrr::map(df_rows, ~ list(values = unname(.x)))
  
  return(formatted_rows)
}