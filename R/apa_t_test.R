apa.t.output <- function(t_test_object, show.mean.difference = TRUE, show.statistic = NULL, show.conf.interval = NULL, number.decimals = NULL, number.decimals.p = NULL) {

  local_options <- set_local_options(list(show.mean.difference = show.mean.difference,
                                          show.conf.interval = show.conf.interval,
                                          number.decimals = number.decimals,
                                          number.decimals.p = number.decimals.p))

  local_options$show.mean.difference = show.mean.difference

  # make markdown output
  number_decimals <- local_options$number.decimals
  number_decimals_p <- local_options$number.decimals.p
  use_p_smaller_than_p001 <- local_options$use_p_smaller_than_p001
  output_txt <- ""

  t_details <- get_t_details(t_test_object)

  if (local_options$show.mean.difference == TRUE) {
    new_str <- sprintf("$\\Delta M$ = %%1.%df",number_decimals)
    new_str <- sprintf(new_str, t_details$M_difference)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.conf.interval == TRUE) {
    new_str <- sprintf("95%%%% CI[%%1.%df, %%1.%df]",number_decimals, number_decimals)
    new_str <- sprintf(new_str, t_details$LL, t_details$UL)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.statistic == TRUE) {
    if (t_details$is_var_equal == TRUE) {
      t_str <- sprintf("*t*(%g)", t_details$df)
    } else {
      t_str <- sprintf("*t*(%1.2f)", t_details$df)
    }
    new_str <- sprintf("%s = %%1.%df",t_str, number_decimals)
    new_str <- sprintf(new_str, abs(t_details$t_value))
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.p == TRUE) {
    new_str <- get_p_text(t_details$p_value, number_decimals_p, use_p_smaller_than_p001, t_details$is_one_side_test)
    output_txt <- add_to_output(output_txt, new_str)
  }

  return(output_txt)
}



get_t_details <- function(t_test_object) {

  is_welch <- "Welch Two Sample t-test"
  t_method <- t_test_object$method
  is_var_equal <- TRUE
  if (t_method == is_welch ) {
    is_var_equal <- FALSE
  }

  if (length(t_test_object$estimate)>1) {
    M_difference <- abs(t_test_object$estimate[1] - t_test_object$estimate[2])
  } else {
    M_difference <- t_test_object$estimate
  }

  t_value <- t_test_object$statistic
  df <- t_test_object$parameter
  p_value <- t_test_object$p.value
  LL <- t_test_object$conf.int[1]
  UL <- t_test_object$conf.int[2]

  test_side <- t_test_object$alternative
  is_one_side_test <- TRUE
  if (test_side == "two.sided") {
    is_one_side_test <- FALSE
  }

  output <- list(t_method = t_method,
                 is_var_equal = is_var_equal,
                 M_difference = M_difference,
                 LL = LL,
                 UL = UL,
                 t_value = t_value,
                 df = df,
                 p_value = p_value,
                 is_one_side_test = is_one_side_test)

  return(output)
}


#' Report descriptive statistics for a set of values
#' @param .data A data frame or data frame extension (e.g., tibble)
#' @param .iv Name of the independent variable column (only 2 levels)
#' @param .dv Name of the dependent variable column
#' @param var.equal (boolean) TRUE or FALSE for cell equal variances
#' @param one.sided  (boolean) TRUE or FALSE for conducting a one-sided test
#' @param bonferroni.multiplier Multiply the p-value by this number to make a bonferroni adjustment
#' @param show.mean.difference Show mean difference (Bool. Default TRUE)
#' @param show.conf.interval Show CI for mean difference (Bool. Default TRUE)
#' @param show.statistic Show t-value (Bool. Default TRUE)
#' @param number.decimals Number of decimals used in output (excluding p-value)
#' @param number.decimals.p Number of decimals used in p-value output
#' @return R Markdown text
#' @examples
#' if  (requireNamespace("apaTables", quietly = TRUE)){
#'     library(dplyr)
#'     goggles <- apaTables::goggles
#'
#'     # one-sided test
#'     goggles %>%
#'       filter(alcohol == "None") %>%
#'       filter(gender == "Female" | gender == "Male") %>%
#'       apa.ind.t.test(gender, attractiveness,
#'                      var.equal = TRUE, one.sided = TRUE)
#'     #two-sided test
#'     goggles %>%
#'       filter(alcohol == "None") %>%
#'       filter(gender == "Female" | gender == "Male") %>%
#'       apa.ind.t.test(gender, attractiveness,
#'                       var.equal = TRUE, one.sided = FALSE)
#'
#'     #two-sided test with Bonferroni correction (three exploratory tests)
#'     goggles %>%
#'       filter(alcohol == "None") %>%
#'       filter(gender == "Female" | gender == "Male") %>%
#'       apa.ind.t.test(gender, attractiveness,
#'                      var.equal = TRUE, one.sided = FALSE,
#'                       bonferroni.multiplier = 3)
#'  }
#' @export
apa.ind.t.test <- function(.data, .iv, .dv, bonferroni.multiplier = 1, show.mean.difference = TRUE, show.statistic = NULL, show.conf.interval = NULL, number.decimals = NULL, number.decimals.p = NULL, var.equal = TRUE, one.sided = FALSE) {
  local_options <- set_local_options(list(show.mean.difference = show.mean.difference,
                                          show.conf.interval = show.conf.interval,
                                          number.decimals = number.decimals,
                                          number.decimals.p = number.decimals.p))

  local_options$show.mean.difference = show.mean.difference
  number_decimals <- local_options$number.decimals
  number_decimals_p <- local_options$number.decimals.p
  use_p_smaller_than_p001 <- local_options$use_p_smaller_than_p001
  output_txt <- ""

  iv <- enquo(.iv)
  dv <- enquo(.dv)

  ivlevels = levels(dplyr::pull(.data, !!iv))

  grp1rows = dplyr::filter(.data, !!iv == ivlevels[1])
  xvalues = dplyr::pull(grp1rows, !!dv)

  grp2rows = dplyr::filter(.data, !!iv == ivlevels[2])
  yvalues = dplyr::pull(grp2rows, !!dv)

  t_test_object = stats::t.test(x= xvalues, y=yvalues, var.equal = var.equal, alternative = "two.sided")

  t_details <- get_t_details(t_test_object)

  if (one.sided == TRUE) {
    t_details$p_value <- t_details$p_value /2
    t_details$is_one_side_test = TRUE
  }

  is_bonferroni = FALSE
  if (bonferroni.multiplier > 1) {
    is_bonferroni = TRUE
  }


  t_details$p_value <- t_details$p_value * bonferroni.multiplier


  if (local_options$show.mean.difference == TRUE) {
    new_str <- sprintf("$\\Delta M$ = %%1.%df",number_decimals)
    new_str <- sprintf(new_str, t_details$M_difference)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.conf.interval == TRUE) {
    new_str <- sprintf("95%%%% CI[%%1.%df, %%1.%df]",number_decimals, number_decimals)
    new_str <- sprintf(new_str, t_details$LL, t_details$UL)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.statistic == TRUE) {
    if (t_details$is_var_equal == TRUE) {
      t_str <- sprintf("*t*(%g)", t_details$df)
    } else {
      t_str <- sprintf("*t*(%1.2f)", t_details$df)
    }
    new_str <- sprintf("%s = %%1.%df",t_str, number_decimals)
    new_str <- sprintf(new_str, abs(t_details$t_value))
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.p == TRUE) {
    new_str <- get_p_text(t_details$p_value, number_decimals_p, use_p_smaller_than_p001, t_details$is_one_side_test, is_bonferroni)
    output_txt <- add_to_output(output_txt, new_str)
  }


  return(output_txt)
}
