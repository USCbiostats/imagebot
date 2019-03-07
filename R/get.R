#' List repositories of a organization or a user
#'
#' @param org,user Character scalar. Name of the organization or user from which
#' to pull the information.
#' @param fields List of fields to return. An available list of return parameters
#' is available here: https://developer.github.com/v3/repos/#response
#' @return An object of class data frame.
#' @export
list_repos <- function(
  org,
  user,
  fields = c("id", "full_name", "private", "html_url", "language", "size", "fork")
  ) {

  # API call to obtain the list of repositories
  dat <- if (missing(org) & missing(user))
    stop("Both `org` and `user` are missing. You should specify one of them.",
         call. = FALSE)
  else if (!missing(org) & missing(user))
    gh::gh("/orgs/:org/repos", org=org)
  else if (missing(org) & !missing(user))
    gh::gh("/users/:user/repos", user=user)
  else if (!missing(org) & !missing(user))
    stop("Only one of `org` and `user` can be used.", call. = FALSE)

  # Coercing the data to a data.frame
  dat <- lapply(dat, "[", fields)

  data.frame(do.call(rbind, dat), stringsAsFactors = FALSE)

}

