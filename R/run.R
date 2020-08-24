#' Launch an interactive Shiny app
#'
#' @param app A Shiny app
#' @examples
#' run("missing-data-1")
#' @export
run <- function(app) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("apps", package = "hdat9700tutorials"))

  validExamplesMsg <-
    paste0(
      "Available apps are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validExamples) {
    stop(
      'Please specify a valid app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("apps", app, package = "hdat9700tutorials")
  shiny::runApp(appDir, display.mode = "normal")
}
