#' @title Cast specified columns to factor on a server-side data frame
#' @description Convert one or more columns of a server-side data frame to factor
#' using the \code{cast_to_factorDS} server-side function. The resulting data frame
#' is assigned as a new object on each server.
#'
#' Server function called: \code{cast_to_factorDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#' containing the columns to be converted.
#' @param columns A character vector specifying the names of the columns to convert
#' to factor.
#' @param modified_obj A character string specifying the name of the new server-side
#' object to store the modified data frame. If NULL, a name is generated automatically
#' by appending \code{"_modified"} to \code{df}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#' after login. If the \code{datasources} argument is not specified the default set of
#' connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Returns the name of the newly created server-side object
#' containing the modified data frame.
#' @export
#'

ds.cast_to_factor <- function(df, columns, modified_obj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(modified_obj)) {
    modified_obj <- paste0(df, "_modified")
  }

  collapsed_columns <- paste(columns , collapse = "$")
  call <- call("cast_to_factorDS", as.symbol(df), columns)

  DSI::datashield.assign.expr(
    conns = datasources,
    symbol = modified_obj,
    expr = call
  )

  message("Casted columns '", paste(columns, collapse = ", ") , "'. Result saved as: '", modified_obj, "'")

  invisible(modified_obj)

}
