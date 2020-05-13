#' Launch an interactive Shiny app
#'
#' @param example A Shiny app
#' @examples
#' run("missing-data-1")
#' @export
run <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("apps", package = "hdat9700tutorials"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please specify a valid app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("apps", example, package = "hdat9700tutorials")
  shiny::runApp(appDir, display.mode = "normal")
}
