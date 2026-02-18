#' @title Filter columns of a server-side object based on amissingness threshold
#'
#' @description Filters the columns of a server-side data set by removing variables whose
#' proportion of missing values exceeds a user-defined threshold.
#' The function can operate in two modes:
#' \itemize{
#'   \item \code{"combined"}: computes a weighted global missingness across all
#'   servers and removes the same set of variables from every server.
#'   \item \code{"split"}: applies the threshold independently on each server and
#'   removes server-specific sets of variables.
#' }
#'
#' If no variables exceed the threshold, no filtering is applied and a message
#' is returned indicating that no columns need to be removed.
#'
#' Server function called: \code{preprocess_dataDS}, \code{remove_columnDS}
#'
#' @param df A character string providing the name of the input data frame object
#' stored on each server.
#' @param threshold A numeric value between 0 and 100 representing the maximum
#' acceptable percentage of missing values per column.
#' @param type A character string specifying the filtering strategy. Must be one
#' of \code{"combined"} or \code{"split"}.
#' @param preproc_newobj A character string specifying the name of the new
#' pre-processed object to be created on each server. If \code{NULL}, a name is
#' generated automatically by appending \code{"_preprocessed"} to \code{df}.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}}
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return A character string giving the name of the newly created server-side
#' object containing the pre-processed data set. If no columns are removed, the
#' original data are left unchanged and the name of the intended output object
#' is still returned invisibly.
#'
#' @export


ds.check_missing_data <- function(df,
                               threshold,
                               type = c("combined", "split"),
                               preproc_newobj = NULL,
                               datasources = NULL) {

  if (is.null(datasources))
    datasources <- datashield.connections_find()

  if (is.null(preproc_newobj))
    preproc_newobj <- paste0(df, "_preprocessed")

  # Compute % missing per server
  miss_list <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("check_missing_dataDS", as.symbol(df))
  )

  ref_cols <- names(miss_list[[1]])
  for (x in miss_list) {
    if (!identical(names(x), ref_cols))
      stop("Not all servers have the same columns.")
  }

  miss_tab <- as.data.frame(miss_list)
  colnames(miss_tab) <- names(datasources)


  # TYPE == COMBINED
  if (type == "combined") {

    dims <- dsBaseClient::ds.dim(df, type = "split", datasources = datasources)
    n_vec <- sapply(dims, function(x) x[1])
    names(n_vec) <- names(dims)

    miss_mat <- sapply(miss_tab[, names(datasources)], as.numeric)
    miss_tab$global <- (miss_mat %*% n_vec) / sum(n_vec)

    vars_to_remove <- rownames(miss_tab)[miss_tab$global > threshold]

    message("\n Missingness Table (%)")
    tbl_txt <- capture.output(round(miss_tab, 2))
    message(paste(tbl_txt, collapse = "\n"))

    message("\n Variables Removed (Global Missing > ", threshold, "%)")

    if (length(vars_to_remove) == 0) {
      message("None")
      message("\nNo columns need to be removed")
      message("\n Preprocessed data stored as: ", preproc_newobj)
      return(invisible(preproc_newobj))
    }

    removed_tab <- data.frame(
      variable = vars_to_remove,
      global_missing = round(miss_tab[vars_to_remove, "global"], 2),
      row.names = NULL
    )
    tbl_txt <- capture.output(removed_tab)
    message(paste(tbl_txt, collapse = "\n"))

    # build cols_string
    cols_string <- if (length(vars_to_remove) == 1) {
      paste0(vars_to_remove, "$")
    } else {
      paste(vars_to_remove, collapse = "$")
    }

    DSI::datashield.assign.expr(
      conns  = datasources,
      symbol = preproc_newobj,
      expr   = call("remove_columnsDS", as.symbol(df), cols_string)
    )
  }

  # TYPE == SPLIT
  if (type == "split") {

    message("\n Missingness Table (%)")
    tbl_txt <- capture.output(round(miss_tab, 2))
    message(paste(tbl_txt, collapse = "\n"))

    vars_to_remove_list <- lapply(colnames(miss_tab), function(srv) {
      rownames(miss_tab)[miss_tab[[srv]] > threshold]
    })
    names(vars_to_remove_list) <- colnames(miss_tab)

    message("\n Variables Removed per Server (Missing > ", threshold, "%)")
    for (srv in names(vars_to_remove_list)) {
      message("\n ", srv, ":")
      vars_srv <- vars_to_remove_list[[srv]]

      if (length(vars_srv) == 0) {
        message("None")
      } else {
        removed_tab <- data.frame(
          variable    = vars_srv,
          missing_pct = round(miss_tab[vars_srv, srv], 2),
          row.names   = NULL
        )
        tbl_txt <- capture.output(removed_tab)
        message(paste(tbl_txt, collapse = "\n"))
      }
    }

    tot_removed <- sum(lengths(vars_to_remove_list))

    if (tot_removed == 0) {
      message("\nNo columns need to be removed")
      message("\n Preprocessed data stored as: ", preproc_newobj)
      return(invisible(preproc_newobj))
    }

    for (srv in names(datasources)) {
      vars_srv <- vars_to_remove_list[[srv]]
      if (length(vars_srv) == 0)
        next
      cols_string <- if (length(vars_srv) == 1) {
        paste0(vars_srv, "$")
      } else {
        paste(vars_srv, collapse = "$")
      }

      DSI::datashield.assign.expr(
        conns  = datasources[srv],
        symbol = preproc_newobj,
        expr   = call("remove_columnsDS", as.symbol(df), cols_string)
      )
    }
  }

  message("\n Preprocessed data stored as: ", preproc_newobj)
  invisible(preproc_newobj)
}












