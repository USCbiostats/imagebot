
#' Supported file extensions with the comments
#' @noRd
extensions <- list(
  R = list(ext = "R|r", comments = "^\\s*[#]"),
  c = list(ext = "c",   comments = "[//]")
)

#' This function removes all comments
#' @noRd
strip_comments <- (function() {

  # This is the core function
  strip <- function(code, init, end = NULL) {

    if (length(end)) {
      code <- stringr::str_remove_all(
        paste(code, collapse = "[[LINESPLIT]]"),
        paste(init, end, sep = ".+")
        )
      strsplit(code, "[[LINESPLIT]]", fixed = TRUE)
    } else {
      stringr::str_remove_all(
        code,
        paste0(init, ".+")
      )
    }

  }

  # Listing the function that will be available by extension of the file
  ans <- list()
  ans[["R"]]   <- function(code) strip(code, "#")
  ans[["cpp"]] <- function(code) {
    strip(strip(code, "//"), "[/][*]", "[*][/]")
  }
  ans[["h"]] <- ans[["cpp"]]

})()


#' Read-in files to be processed by the checkers
#' @param path Path to the project to process data from
#' @param trim Logical scalar. When `TRUE`, will remove ending blanks.
#' @param ext Regular expression to capture in filenames.
#' @export
#' @aliases sdb_files
read_files <- function(
  path = ".",
  trim = TRUE,
  ext = c(R = "[.](R|r)$", Rmd = "[.](Rmd|rmd)$", cpp = "[.]cpp$", h = "[.]h(pp)?$")
  ) {

  # Listing relevant files to process
  fn <- list.files(path, full.names = TRUE, recursive = TRUE)
  fn <- fn[grepl(paste(ext, collapse="|"), fn)]

  files <- lapply(fn, readLines)

  if (trim)
    files <- lapply(files, trimws, which="right")

  # Adding the filetype
  f_exts <- gsub(".+[.](?=[a-zA-Z]+$)", "", fn, perl = TRUE)
  f_exts <- stringr::str_replace_all(f_exts, structure(names(ext), names = ext))
  files  <- lapply(seq_along(files), function(i) {

    list(
      name     = fn[i],
      contents = structure(
        files[[i]],
        names = seq_along(files[[i]]),
        class = "sdb_contents"
        ),
      ext      = f_exts[i] #,
      # comments = f_comments[i]
    )
  })

  structure(
    unname(files),
    class = "sdb_files"
  )

}

is_sdb_files <- function(obj) {
  if (!inherits(obj, "sdb_files"))
    stop("`obj` should be of class `sdb_files`. It is of class `", class(obj)[1],
         "`. See ?read_files.", call. = FALSE)
}

print.sdb_contents <- function(obj, ...) {

  cat(sprintf("% 4i. %s", as.integer(names(obj)), obj), sep="\n")

}
