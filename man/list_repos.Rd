% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{list_repos}
\alias{list_repos}
\title{List repositories of a organization or a user}
\usage{
list_repos(org, user, fields = c("id", "full_name", "private",
  "html_url", "language", "size", "fork"))
}
\arguments{
\item{org, user}{Character scalar. Name of the organization or user from which
to pull the information.}

\item{fields}{List of fields to return. An available list of return parameters
is available here: https://developer.github.com/v3/repos/#response}
}
\value{
An object of class data frame.
}
\description{
List repositories of a organization or a user
}
