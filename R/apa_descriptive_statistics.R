#' Report descriptive statistics for a set of values
#' @param dv Name of the dependent variable column
#' @param data Project data frame name
#' @param show.conf.interval Show confidence interval (default behavior is TRUE)
#' @param show.N Show confidence interval (default behavior is TRUE)
#' @param ... Tidyverse selection of rows
#' @return R Markdown text
#' @examples
#' #2-way ANOVA Example
#' library(apaTables) #load apaTables to access goggles
#'
#' #Main Effect Means: Gender
#' apa.desc(goggles, gender=="Female", dv = attractiveness)
#' apa.desc(goggles, gender=="Male", dv = attractiveness)
#'
#' #Main Effect Means: Alcohol
#' apa.desc(goggles, alcohol=="None", dv = attractiveness)
#' apa.desc(goggles, alcohol=="2 Pints", dv = attractiveness)
#' apa.desc(goggles, alcohol=="4 Pints", dv = attractiveness)
#'
#' #Cell Mean: Female, 2 Pints
#' apa.desc(goggles, alcohol=="2 Pints", gender=="Female", dv = attractiveness)
#' @export
apa.desc <- function(..., dv = NULL, data = NULL, show.mean = TRUE, show.sd = TRUE, show.se = FALSE, show.conf.interval = NULL, show.N = NULL) {

  local_options <- set_local_options(list(show.mean = show.mean,
                                          show.sd = show.sd,
                                          show.se = show.se,
                                          show.conf.interval = show.conf.interval,
                                          show.N = show.N))
  if (!is.null(get_apa_data())) {
    message("Using apa.data data frame.")
    assign("data", get("apa.data", envir = get_apa_data()))
  }

  dv <- dplyr::enquo(dv)
  row_selection_criteria <- dplyr::quos(...)
  data_column_frame <- dplyr::filter(data, !!! row_selection_criteria)
  data_column_frame <- dplyr::select(data_column_frame, !!! dv)
  data_column_frame <- stats::na.omit(data_column_frame) # only one column
  group_data <- data_column_frame[, 1]

  desc <- get_desc(group_data)

  # make markdown output
  output_txt <- ""

  if (local_options$show.mean == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("*M* = %1.2f", desc$group_m) )
  }

  if (local_options$show.sd == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("*SD* = %1.2f", desc$group_sd) )
  }

  if (local_options$show.se == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("*SE* = %1.2f", desc$group_se) )
  }

  # make markdown output
  if (local_options$show.conf.interval == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("95%% CI[%1.2f, %1.2f]", desc$LL, desc$UL) )
  }

  if (local_options$show.N == TRUE) {
    output_txt <- add_to_output(output_txt, sprintf("*N* = %g", desc$N) )
  }

  return(output_txt)
}






get_desc <- function(group_data) {

  N <- length(group_data)
  group_m <- mean(group_data, na.rm = TRUE)
  group_sd <- stats::sd(group_data, na.rm = TRUE)
  group_se <- group_sd / sqrt(N)
  group_df <- N - 1
  ci_t <- stats::qt(.975, df = group_df) # 95% CI
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


add_to_output <- function(output_txt, new_str) {
  if (output_txt =="") {
    output <- new_str
  } else {
    output <- sprintf("%s, %s", output_txt, new_str)
  }
  return(output)
}