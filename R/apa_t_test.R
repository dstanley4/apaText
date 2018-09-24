#' Report descriptive statistics for a set of values
#' @param ... one or more dplyr commands (separated by commas) to select rows. Eg: alcohol=="2 Pints", gender=="Female"
#' @param dv Name of the dependent variable column
#' @param data Project data frame name
#' @param show.mean Show mean (Bool. Default TRUE)
#' @param show.sd Show standard deviation (Bool. Default TRUE)
#' @param show.se Show standard error (Bool. Default FALSE)
#' @param show.conf.interval Show confidence interval (Bool. Default TRUE)
#' @param show.N Show number of cases (Bool. Default TRUE)
#' @return R Markdown text
#' @examples
#' #2-way ANOVA Example
#' library(apaTables) #load apaTables to access goggles
#' @export
apa.t.test <- function(t_test_object, show.mean.difference = TRUE, show.statistic = NULL, show.conf.interval = NULL, number.decimals = NULL, number.decimals.p = NULL) {

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
    new_str <- get_p_text(t_details$p_value, number_decimals_p, use_p_smaller_than_p001)
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

  M_difference <- abs(t_test_object$estimate[1] - t_test_object$estimate[2])
  t_value <- t_test_object$statistic
  df <- t_test_object$parameter
  p_value <- t_test_object$p.value
  LL <- t_test_object$conf.int[1]
  UL <- t_test_object$conf.int[2]

  output <- list(t_method = t_method,
                 is_var_equal = is_var_equal,
                 M_difference = M_difference,
                 LL = LL,
                 UL = UL,
                 t_value = t_value,
                 df = df,
                 p_value = p_value)

  return(output)
}


