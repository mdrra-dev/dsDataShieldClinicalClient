#' @title Check patient-visit combinations on a server-side data frame
#' @description Verify all patient-visit combinations in a server-side data frame
#'   and report any duplicates using the \code{check_patient_visitDS} server-side function.
#'   Messages are printed for each server indicating whether duplicates are present.
#'
#' Server function called: \code{check_patient_visitDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   containing at least the columns \code{pat_ID} and \code{Visit_months_from_diagnosis}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If the \code{datasources} argument is not specified, the default set of
#'   connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisible \code{NULL}. Messages are printed to indicate whether
#'   duplicate patient-visit pairs were found on each server.
#' @export

ds.check_patient_visit <- function(df, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  call <- call("check_patient_visitDS", df = as.symbol(df))
  results <- DSI::datashield.aggregate(conns = datasources, expr = call)

  for (server in names(results)) {

    df_server <- results[[server]]
    # Filter out duplicated combinations
    duplicates <- df_server[df_server$n > 1, ]

    if (nrow(duplicates) > 0) {
      colnames(duplicates)[colnames(duplicates) == "n"] <- "n_of_duplicates"

      message(paste0("Server: ", server))
      message("Warning: the pair patient-visit contains duplicates\n")

      duplicates_df <- as.data.frame(duplicates)

      message(paste(capture.output(print(duplicates_df, row.names = FALSE)),
                    collapse = "\n"), "\n")

    } else {
      message(paste0("Server: ", server))
      message("All patient-visit pairs are unique")
    }
  }
  invisible(NULL)
}
