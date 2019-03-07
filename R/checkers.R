#' Functions to check coding standards
#'
#' @template sdb
#' @name checkers
#' @examples
#'
#' \dontrun{
#' # Looking at the files in the project
#' obj <- read_files()
#'
#' # Calling the checkers
#' check_comments(obj)
#' check_linewidth(obj)
#' }
#'
#'
NULL

#' @param n Threshold for max linewidth.
#' @rdname checkers
#' @export
check_linewidth <- function(obj, n = 90) {

  # Checking it is the right type of object
  is_sdb_files(obj)

  res <- lapply(obj, function(f) {

    nchars <- nchar(f$contents)
    passed <- nchars <= n

    data.frame(
      line   = ifelse(passed, f$contents, paste(substr(f$contents, 1, n), "[trunc]")),
      nchars = nchars,
      dif    = ifelse(passed, 0, nchars - n)
    )

  })

  names(res) <- names(obj)

  res

}

#' @export
#' @rdname checkers
check_comments <- function(obj) {

  # Baseline checking
  is_sdb_files(obj)


  # Collapsing comments
  res <- sapply(obj, function(f) {

    # First character
    first_char   <- substr(trimws(f$contents, "left"), 1L, 1L)
    are_comments <- first_char == "#"

    # Counting the number of comments
    are_comments <- which(are_comments)
    if (are_comments[length(are_comments)] == length(first_char))
      are_comments <- are_comments[-length(are_comments)]

    n_lines_comments <- length(are_comments)
    if (n_lines_comments < 2)
      return(n_lines_comments)

    sum(first_char[are_comments] != first_char[are_comments + 1L])



  })


  structure(res, names = names(obj))

}
