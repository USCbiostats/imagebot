# This function keeps track of the current file that is been checked. In lintr
# files are parsed expression by expression, so keeping track of whether the
# file has been checked or not allows us to make a single call of a particular
# function (like comments_linter for example) only once.
file_checked <- (function() {

  # Initializing environment
  env         <- new.env(parent = emptyenv())
  env$last    <- NULL
  env$current <- NULL

  function(filename) {

    env$current <- filename
    if (is.null(env$last)) {
      env$last <- env$current
    } else if (env$last == env$current) {
      return(TRUE)
    } else
      env$last <- env$current

    return(FALSE)
  }

})()


#' A lint to count proportion of code/comments
#' @param source_file See [lintr::linters]
#' @export
#' @importFrom lintr Lint
comments_linter <- function(source_file) {

  # Checking caller
  if (file_checked(source_file$filename))
    return(NULL)

  # Retrieving the raw contents
  f <- readLines(source_file$filename)

  # First character
  first_char   <- substr(trimws(f, "left"), 1L, 1L)
  are_comments <- first_char == "#"

  # Counting the number of comments
  are_comments   <- which(are_comments)
  arent_comments <- setdiff(1:length(f), are_comments)

  if (!length(are_comments))
    return(NULL)

  # Counting comments
  n_lines_comments <- length(are_comments)
  n_comments <- if (n_lines_comments < 2)
    n_lines_comments
  else
    sum(first_char[are_comments] != first_char[are_comments + 1L])

  # Looking at proportion, For every 20 lines of code, there should be at least
  # 1 comment
  prop_commented <- n_comments/length(arent_comments)

  if (prop_commented <= 1/5)
    lintr::Lint(
      filename      = source_file$filename,
      line_number   = 1L,
      column_number = 1L,
      type          = "style",
      message       = sprintf(
        "No enought comments: %i of every %i lines of code has a comment (%0.2f%%). ",
        n_comments, length(arent_comments), prop_commented*100),
      line          = "",
      linter        = "comments_linter"
    )

}
