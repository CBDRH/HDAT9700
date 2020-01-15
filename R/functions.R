# Functions for HDAT9700 Statistical Modelling II

# The blocks below can be used to map chapters to tutorials
# e.g. The chapter1() function is mapped to the tutorial "research-design-and-methods"
# If mapping a chapter to a new tutorial make sure to update all the information for
# that chapter in the relevant block below


#' Chapter 1. Research Design and Methods
#'
#' Launch the tutorial for chapter 1
#'
#' @export
chapter1 <- function() {
  learnr::run_tutorial("rdx", utils::packageName())
}


#' Chapter 2. Draw your assumptions before your conclusions
#'
#' Launch the tutorial for chapter 2
#'
#' @export
chapter2 <- function() {
  learnr::run_tutorial("test", utils::packageName())
}


#' Chapter 3. Propensity score matching
#'
#' Launch the tutorial for chapter 3
#'
#' @export
chapter3 <- function() {
  learnr::run_tutorial("psm", utils::packageName())
}


#' Chapter 4. Multilevel Modelling (Part I)
#'
#' Launch the tutorial for chapter 4
#'
#' @export
chapter4 <- function() {
  learnr::run_tutorial("mlm1", utils::packageName())
}


#' Chapter 5. Multilevel Modelling (Part II)
#'
#' Launch the tutorial for chapter 5
#'
#' @export
chapter5 <- function() {
  learnr::run_tutorial("mlm2", utils::packageName())
}


#' Chapter 6. Growth Curve Modelling
#'
#' Launch the tutorial for chapter 6
#'
#' @export
chapter6 <- function() {
  learnr::run_tutorial("mlm3", utils::packageName())
}


#' Chapter 7. Time Series Analysis
#'
#' Launch the tutorial for chapter 7
#'
#' @export
chapter7 <- function() {
  learnr::run_tutorial("tsa", utils::packageName())
}


#' Chapter 8. Interrupted Time Series Analysis
#'
#' Launch the tutorial for chapter 8
#'
#' @export
chapter8 <- function() {
  learnr::run_tutorial("its", utils::packageName())
}


#' Chapter 9. Missing data and multiple imputation
#'
#' Launch the tutorial for chapter 9
#'
#' @export
chapter9 <- function() {
  learnr::run_tutorial("mi", utils::packageName())
}

