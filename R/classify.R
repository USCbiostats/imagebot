#' Check whether a path is an R package or not.
#' @param path Character scalar. Path to the package to dir.
#' @param fn Character vector. List of files to consider an R package.
#' @examples
#' is_rpackage()
is_rpackage <- function(path = ".", fn = c("R", "DESCRIPTION", "NAMESPACE", "man")) {

  # Listing the files so we can take a look at their names. Do these match an
  # ar package?
  all(fn %in% list.files(path))


}


