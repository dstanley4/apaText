#' Report descriptive statistics for a set of values
#' @param .data A data frame or data frame extension (e.g., tibble)
#' @param .dv Name of the dependent variable column
#' @param show.mean Show mean (Bool. Default TRUE)
#' @param show.sd Show standard deviation (Bool. Default TRUE)
#' @param show.se Show standard error (Bool. Default FALSE)
#' @param show.conf.interval Show confidence interval (Bool. Default TRUE)
#' @param show.N Show number of cases (Bool. Default TRUE)
#' @param number.decimals Number of decimals in output
#' @return R Markdown text
#' @examples
#' # 2-way ANOVA Example
#' if  (requireNamespace("apaTables", quietly = TRUE)){
#'     library(dplyr)
#'     goggles <- apaTables::goggles
#'
#'     #Main Effect Means: Gender
#'     goggles %>% filter(gender == "Female") %>% apa.desc(attractiveness)
#'     goggles %>% filter(gender == "Male") %>% apa.desc(attractiveness)
#'
#'     # Main Effect Means: Alcohol
#'     goggles %>% filter(alcohol == "None") %>% apa.desc(attractiveness)
#'     goggles %>% filter(alcohol == "2 Pints") %>% apa.desc(attractiveness)
#'     goggles %>% filter(alcohol == "4 Pints") %>% apa.desc(attractiveness)
#'
#'     # Single Cell Mean
#'     goggles %>% filter(alcohol == "4 Pints", gender == "Female") %>%
#'                 apa.desc(attractiveness)
#' }
#' @export
apa.desc <- function(.data, .dv = NULL, show.mean = NULL, show.sd = NULL, show.se = NULL, show.conf.interval = NULL, show.N = NULL, number.decimals = NULL) {

  local_options <-  set_local_options(list(show.mean = show.mean,
                                          show.sd = show.sd,
                                          show.se = show.se,
                                          show.conf.interval = show.conf.interval,
                                          show.N = show.N,
                                          number.decimals = number.decimals))



  dv <- enquo(.dv)
  group_data = dplyr::pull(.data, !!dv)

  desc <- get_desc(group_data)

  # make markdown output
  number_decimals <- local_options$number.decimals
  output_txt <- ""

  if (local_options$show.mean == TRUE) {
    new_str <- sprintf("*M* = %%1.%df",number_decimals)
    new_str <- sprintf(new_str, desc$group_m)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.conf.interval == TRUE) {
    new_str <- sprintf("95%%%% CI[%%1.%df, %%1.%df]",number_decimals, number_decimals)
    new_str <- sprintf(new_str, desc$LL, desc$UL)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.sd == TRUE) {
    new_str <- sprintf("*SD* = %%1.%df",number_decimals)
    new_str <- sprintf(new_str, desc$group_sd)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.se == TRUE) {
    new_str <- sprintf("*SE* = %%1.%df",number_decimals)
    new_str <- sprintf(new_str, desc$group_se)
    output_txt <- add_to_output(output_txt, new_str)
  }

  if (local_options$show.N == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("*N* = %g", desc$N) )
  }

  return(output_txt)
}






get_desc <- function(group_data, conf.level = .95) {

  N <- length(group_data)
  group_m <- mean(group_data, na.rm = TRUE)
  group_sd <- stats::sd(group_data, na.rm = TRUE)
  group_se <- group_sd / sqrt(N)
  group_df <- N - 1

  qtlookup <- conf.level + (1-conf.level)/2
  ci_t <- stats::qt(qtlookup, df = group_df)

  LL <- group_m - ci_t * group_se
  UL <- group_m + ci_t * group_se

  output <- list(N = N,
                 group_m = group_m,
                 group_sd = group_sd,
                 group_se = group_se,
                 group_df = group_df,
                 ci_t = ci_t,
                 LL = LL,
                 UL = UL)
 return(output)
}


