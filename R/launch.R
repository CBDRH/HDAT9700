#' Launch an interactive learnr tutorial
#'
#' @param tute A learnr tutorial
#' @examples
#' launch("dags")
#' @export
launch <- function(tute) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("tutorials", package = "hdat9700tutorials"))

  validExamplesMsg <-
    paste0(
      "Available tutorials are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(tute) || !nzchar(tute) ||
      !tute %in% validExamples) {
    stop(
      'Please specify a valid learnr tutorial as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the tutorial
  learnr::run_tutorial(name = tute, package = 'hdat9700tutorials')
}
