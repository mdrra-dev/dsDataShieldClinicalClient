#' @title Cast "NA" string into NA
#' @description Executes \code{cast_NADS} server-side function to convert "NA" strings into NA values across a dataset,
#' and reports whether any replacements occurred.
#'
#' @param df A character string specifying the name of the server-side data frame to be converted.
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

ds.cast_NA <- function(df, modified_obj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(modified_obj)) {
    modified_obj <- paste0(df, "_cast_NA")
  }

  call <- call("cast_NADS", as.symbol(df))

  DSI::datashield.assign.expr(
    conns = datasources,
    symbol = modified_obj,
    expr = call
  )

  # check if changes are applied to the df
  expr_check <- call("get_cast_NA_changedDS", as.symbol(modified_obj))
  res <- DSI::datashield.aggregate(datasources, expr_check)
  changed <- any(unlist(res))

  if (changed) {
    message("Some 'NA' strings were converted to NA")
  } else {
    message("No 'NA' strings found (nothing changed)")
  }

  invisible(modified_obj)

}
