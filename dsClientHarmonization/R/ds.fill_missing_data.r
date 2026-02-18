#' @title Fill missing visit data on a server-side data frame
#' @description Invoke the server-side function \code{fill_missing_dataDS}
#'   to fill missing values in a specified column within each patient,
#'   ordered by visit time from diagnosis. The modified data frame is assigned
#'   to a new server-side object.
#'
#' Server function called: \code{fill_missing_dataDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   containing at least the patient identifier column, the visit time column,
#'   and the variable to be forward-filled.
#' @param pat_id_col A character string specifying the name of the patient
#'   identifier column.
#' @param visit_col A character string specifying the name of the visit time
#'   column.
#' @param value_col A character string specifying the name of the column
#'   whose missing values should be forward-filled.
#' @param filled_newobj A character string specifying the name of the new
#'   server-side object that will store the modified data frame. If not provided,
#'   the default name is \code{paste0(df, "_filled")}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If not specified, the default set of connections is used:
#'   see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisibly returns the name of the newly created server-side object.
#'   A message is printed indicating where the result has been saved.
#' @export
#'

ds.fill_missing_data <- function(df, pat_id_col, visit_col, value_col,
                                 filled_newobj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(filled_newobj)) {
    filled_newobj <- paste0(df, "_filled")
  }

  call <- call("fill_missing_dataDS", as.symbol(df), pat_id_col, visit_col, value_col)

  datashield.assign.expr(
    conns = datasources,
    symbol     = filled_newobj,
    expr       = call
  )
  message("Result saved as: '", filled_newobj, "'")

  invisible(filled_newobj)
}
