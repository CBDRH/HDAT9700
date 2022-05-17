# Functions for HDAT9700 Statistical Modelling II


#' Show an index of available tutorials
#'
#' @return
#' @export
#'
#' @examples
#' index()
index <- function(){

  learnr::available_tutorials(package = 'hdat9700tutorials')

}
