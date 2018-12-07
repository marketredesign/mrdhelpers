#' Checks if file extension matches
#'
#' @param file The file of which the extension needs to be checked.
#' @param exp_ext The expected extension of the file to be checked. This
#' extension file can be both with or without leading "." For example,
#' inputing .csv or csv will give similar results.
#'
#' @return A TRUE value if the extension matches. And a FALSE value if
#' the extension does not match.
#' @export
#'
#' @examples checkExtension(file = "testdata/test1.csv", exp_ext = "csv")
checkExtension <- function(file, exp_ext) {

  # check if all arguments are filled
  if (methods::hasArg(file) == FALSE) {
    stop("The file parameter is mandatory.")
  }

  if (methods::hasArg(exp_ext) == FALSE) {
    stop("The exp_ext value is mandatory.")
  }

  # check if extension starts with a "."
  if (substring(exp_ext, 1, 1) == ".") {
    exp_ext <- substring(exp_ext, 2, nchar(exp_ext))
  }

  # check if the extensions match
  if (tools::file_ext(file) == exp_ext) {
    result <- TRUE
  } else if (tools::file_ext(file) != exp_ext){
    result <- FALSE
  } else {
    stop("Something went wrong in the function. Please contact package maintainer")
  }

  # return the result as TRUE or FALSE
  return(result)

}
