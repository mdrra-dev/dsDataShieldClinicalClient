#' @title Check variables on a server-side data frame
#' @description Verify that a server-side data frame contains exactly the expected
#'   variables. Uses the server-side function \code{check_variablesDS} to identify
#'   missing or extra variables. Messages are printed for each server indicating
#'   discrepancies.
#'
#' Server function called: \code{check_variablesDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   to check.
#' @param variables A character vector specifying the expected variable names.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If the \code{datasources} argument is not specified, the default set of
#'   connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisible list of results returned by the server-side function. Each element
#'   corresponds to a server and contains two components: \code{missing} (variables
#'   present in \code{variables} but missing in the data frame) and \code{extra}
#'   (variables present in the data frame but not in \code{variables}).
#' @export

ds.check_variables <- function(df, variables, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  collapsed_variables <- paste(variables, collapse = "$")

  call <- call("check_variablesDS",df = as.symbol(df),variables_string = collapsed_variables)

  results <- DSI::datashield.aggregate(conns = datasources,expr = call)

  for (server in names(results)) {

    missing <- results[[server]]$missing
    extra   <- results[[server]]$extra

    if (length(missing) == 0 && length(extra) == 0) {
      message("All variables in ", server, " match the variables list.")
    } else {
      msg <- paste0("Server ", server, ": ")
      if (length(missing) > 0) {
        msg <- paste0(msg,"Missing variables: ",paste(missing, collapse = ", "))
      }

      if (length(extra) > 0) {
        if (length(missing) > 0) {
          msg <- paste0(msg, "; ")
        }
        msg <- paste0(msg,"Extra variables: ",paste(extra, collapse = ", "))
      }
      message(msg)
    }
  }

  invisible(results)
}
