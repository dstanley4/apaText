#' Create APA style text from analyses for use within R Markdown documents. Descriptive statistics, confidence intervals, and cell sizes are reported.
#' \tabular{ll}{
#' Package: \tab apaText\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1.7\cr
#' Date: \tab 2023-05-23\cr
#' License: \tab MIT\cr
#' }
#'
#' @name apaText
#' @aliases apaText
#' @docType package
#' @title Create R Markdown Text for Results in the Style of the American Psychological Association (APA)
#' @author
#' \tabular{ll}{
#' Author: \tab David J. Stanley \email{dstanley@@uoguelph.ca}\cr
#' Maintainer: \tab David J. Stanley \email{dstanley@@uoguelph.ca}
#' }
#' @importFrom "stats"  "na.omit" "qt" "sd" "cor" "cor.test"
#' @importFrom "dplyr" "filter" "select" "quos" "enquo"
utils::globalVariables(c("apa.default.options"))
NULL
