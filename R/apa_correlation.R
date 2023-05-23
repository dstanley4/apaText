# Correlation -------------------------------------------------------------

#' Report r(x,y) correlation in markdown APA style
#'
#' @param .data A data frame or data frame extension (e.g., tibble)
#' @param .x Name of column in data frame
#' @param .y Name of column in data frame
#' @param alternative Alternative hypothesis to pass to alternative argument of cor.test. Default is "two.sided"
#' @param method Calculation method to pass to alternative argument of cor.test. Default is "pearson"
#' @param show.r Show correlation or not (TRUE/FALSE)
#' @param show.conf.interval Show confidence interval or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.N Show sample size or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.p Show p-value or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.statistic Show test statistic or not (TRUE/FALSE). Default behavior is TRUE.
#' @param number.decimals Number of decimals used in output (excluding p-value)
#' @param number.decimals.p Number of decimals used in output for p-value
#' @return R Markdown text
#' @examples
#' library(dplyr)
#' attitude %>% apa.r(rating, advance)
#' @export
apa.r <- function(.data, .x, .y, alternative = "two.sided", method = "pearson", show.r = TRUE, show.conf.interval = NULL, show.N = NULL, show.p = NULL, show.statistic = NULL, number.decimals = NULL, number.decimals.p = NULL) {
  conf.level <- .95

  local_options <- set_local_options(list(
    show.conf.interval = show.conf.interval,
    show.N = show.N,
    show.p = show.p,
    show.statistic = show.statistic
  ))

  x <- enquo(.x)
  y <- enquo(.y)
  cor_data = select(.data, !!x, !!y)

  two_vars <- na.omit(cor_data)
  xvalues <- two_vars[, 1]
  yvalues <- two_vars[, 2]
  N <- length(xvalues)

  result_cor <- cor.test(x = xvalues, y = yvalues,
                         method = method,
                         alternative = alternative,
                         conf.level = conf.level)

  alternative_txt <- ""
  if (alternative != "two.sided") {
    alternative_txt <- " (one sided)"
  }

  rvalue <- result_cor$estimate
  r_txt <- get_r_txt(rvalue, decimals = 3)

  output_txt <- sprintf("*r* = %s", r_txt)

  if (local_options$show.p == TRUE) {
    pvalue <- result_cor$p.value
    p_apa_txt <- get_p_apa_txt(pvalue)
    if (local_options$show.statistic == TRUE) {
      is_pearson <- grep("Pearson", result_cor$method)
      if (length(is_pearson) > 0) {
        t_value <- round(result_cor$statistic, 2)
        df <- result_cor$parameter
        output_txt <- sprintf("%s, t(%g) = %1.2f, %s%s", output_txt, df, t_value, p_apa_txt, alternative_txt)
      } else {
        output_txt <- sprintf("%s, %s%s", output_txt, p_apa_txt, alternative_txt)
        local_options$show.conf.interval <- FALSE
      }
    } else {
      output_txt <- sprintf("%s, %s%s", output_txt, p_apa_txt, alternative_txt)
    }
  }

  if (local_options$show.conf.interval == TRUE) {
    conf_int <- result_cor$conf.int
    LL <- get_r_txt(conf_int[1])
    UL <- get_r_txt(conf_int[2])
    conf_level_percent <- round(conf.level * 100)
    output_txt <- sprintf("%s, %g%% CI[%s,%s]", output_txt, conf_level_percent, LL, UL)
  }

  if (local_options$show.N == TRUE) {
    output_txt <- sprintf("%s, *N* = %g", output_txt, N)
  }

  return(output_txt)
}

#' Report difference between correlations in markdown APA style from different samples
#'
#' @param formula Formula for comparing correlations
#' @param data1 Project data frame 1 name
#' @param data2 Project data frame 2 name
#' @param alternative Alternative hypothesis to pass to alternative argument of cocor.indep.groups. Default is "two.sided"
#' @param show.conf.interval Show confidence interval or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.N Show sample size or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.p Show p-value or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.statistic Show test statistic or not (TRUE/FALSE). Default behavior is TRUE.
#' @return R Markdown text
#' @examples
#'
#' # Test difference between r(rating, learning) from dataset: attitude
#' # and r(weight, height) from dataset: women
#'
#' apa.r.compare.across.samples(formula = ~ rating + learning | height + weight,
#'                              data1 = attitude,
#'                              data2 = women)
#' @export
apa.r.compare.across.samples <- function(formula, data1, data2,
                                         alternative = "two.sided",
                                         show.conf.interval = NULL, show.N = NULL,
                                         show.p = NULL, show.statistic = NULL) {
  conf.level <- .95

  local_options <- set_local_options(list(
    show.conf.interval = show.conf.interval,
    show.N = show.N,
    show.p = show.p,
    show.statistic = show.statistic
  ))


  if (is.null(data1) == TRUE) {
    is.apa.data <- any("apa.data" == ls(globalenv()))
    if (is.apa.data == TRUE) {
      assign("data1", get("apa.data", envir = globalenv()))
    } else {
      message("Data frame 1 not specified")
      return()
    }
  }

  if (is.null(data2) == TRUE) {
    message("Data frame 2 not specified")
    return()
  }

  cor_tests <- cocor::cocor(formula = formula, data = list(data1, data2), conf.level = conf.level)

  n1 <- cor_tests@n1
  n2 <- cor_tests@n2

  diff_r_txt <- get_r_txt(cor_tests@diff, decimals = 3)

  alternative_txt <- ""
  if (alternative != "two.sided") {
    alternative_txt <- " (one sided)"
  }
  output_txt <- sprintf("$\\Delta r =$ %s", diff_r_txt)

  if (local_options$show.conf.interval == TRUE) {
    LL <- round(cor_tests@zou2007$conf.int[1], 2)
    UL <- round(cor_tests@zou2007$conf.int[2], 2)
    conf_level_percent <- round(conf.level * 100)
    output_txt <- sprintf("%s, %g%% CI[%s,%s]", output_txt, conf_level_percent, LL, UL)
  }

  if (local_options$show.p == TRUE) {
    stat_details <- cor_tests@fisher1925
    p_apa_txt <- get_p_apa_txt(stat_details$p.value)
    test_dist <- stat_details$distribution
    test_value <- stat_details$statistic
    if (local_options$show.statistic == TRUE) {
      test_txt <- sprintf("%s = %1.2f, %s", test_dist, test_value, p_apa_txt)
      output_txt <- sprintf("%s, %s%s", output_txt, test_txt, alternative_txt)
    } else {
      output_txt <- sprintf("%s, %s%s", output_txt, p_apa_txt, alternative_txt)
    }
  }

  if (local_options$show.N == TRUE) {
    output_txt <- sprintf("%s, *N1* = %g, *N2* = %g", output_txt, n1, n2)
  }
  return(output_txt)
}


#' Report difference between correlations in markdown APA style from different samples
#'
#' @param r1 Correlation in sample 1
#' @param n1 Sample size for sample 1
#' @param r2 Correlation in sample 2
#' @param n2 Sample size for sample 2
#' @param alternative Alternative hypothesis to pass to alternative argument of cocor.indep.groups. Default is "two.sided"
#' @param show.conf.interval Show confidence interval or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.N Show sample size or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.p Show p-value or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.statistic Show test statistic or not (TRUE/FALSE). Default behavior is TRUE.
#' @return R Markdown text
#' @examples
#' apa.r.compare.across.samples.from.descriptive(r1 = .3, r2 =.6, n1 = 70, n2 =80)
#' @export
apa.r.compare.across.samples.from.descriptive <- function(r1,r2,n1,n2,
                                         alternative = "two.sided",
                                         show.conf.interval = NULL, show.N = NULL,
                                         show.p = NULL, show.statistic = NULL) {
  conf.level <- .95

  local_options <- set_local_options(list(
    show.conf.interval = show.conf.interval,
    show.N = show.N,
    show.p = show.p,
    show.statistic = show.statistic
  ))

  cor_tests <- cocor::cocor.indep.groups(r1.jk = r1, r2.hm = r2, n1 = n1, n2 = n2)

  diff_r_txt <- get_r_txt(cor_tests@diff, decimals = 3)

  alternative_txt <- ""
  if (alternative != "two.sided") {
    alternative_txt <- " (one sided)"
  }
  output_txt <- sprintf("$\\Delta r =$ %s", diff_r_txt)

  if (local_options$show.conf.interval == TRUE) {
    LL <- round(cor_tests@zou2007$conf.int[1], 2)
    UL <- round(cor_tests@zou2007$conf.int[2], 2)
    conf_level_percent <- round(conf.level * 100)
    output_txt <- sprintf("%s, %g%% CI[%s,%s]", output_txt, conf_level_percent, LL, UL)
  }

  if (local_options$show.p == TRUE) {
    stat_details <- cor_tests@fisher1925
    p_apa_txt <- get_p_apa_txt(stat_details$p.value)
    test_dist <- stat_details$distribution
    test_value <- stat_details$statistic
    if (local_options$show.statistic == TRUE) {
      test_txt <- sprintf("%s = %1.2f, %s", test_dist, test_value, p_apa_txt)
      output_txt <- sprintf("%s, %s%s", output_txt, test_txt, alternative_txt)
    } else {
      output_txt <- sprintf("%s, %s%s", output_txt, p_apa_txt, alternative_txt)
    }
  }

  if (local_options$show.N == TRUE) {
    output_txt <- sprintf("%s, *N1* = %g, *N2* = %g", output_txt, n1, n2)
  }
  return(output_txt)
}



#' Report difference in markdown APA style between between correlations within a sample
#'
#' @param formula Formula for comparing correlations
#' @param data Project data frame name
#' @param alternative Alternative hypothesis to pass to alternative argument of cor.test. Default is "two.sided"
#' @param show.conf.interval Show confidence interval or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.N Show sample size or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.p Show p-value or not (TRUE/FALSE). Default behavior is TRUE.
#' @param show.statistic Show test statistic or not (TRUE/FALSE). Default behavior is TRUE.
#' @param test Type of significance test. If non-overlapping variables use one of "pearson1898", "dunn1969", "steiger1980", "raghunathan1996", or "silver2004".  If overlapping variables use one of pearson1898, hotelling1940, hendrickson1970, williams1959, olkin1967, dunn1969, steiger1980, meng1992, hittner2003. Default is pearson1898.
#' @return R Markdown text
#' @examples
#'
#' # non-overlappling variables example
#' apa.r.compare.within.sample(data = attitude,
#'   formula = ~ rating + complaints | privileges + learning)
#'
#' # overlappling variables example
#' apa.r.compare.within.sample(data = attitude,
#'   formula = ~ rating + complaints | rating + learning)
#' @export
apa.r.compare.within.sample <- function(formula, data, test = "pearson1898", alternative = "two.sided", show.conf.interval = NULL, show.N = NULL, show.p = NULL, show.statistic = NULL) {
  conf.level <- .95

  local_options <- set_local_options(list(
    show.conf.interval = show.conf.interval,
    show.N = show.N,
    show.p = show.p,
    show.statistic = show.statistic
  ))

  is_data_frame <- FALSE
  if (!missing(data) == TRUE) {
    is_data_frame <- TRUE
  } else {
    is.apa.data <- any("apa.data" == ls(globalenv()))
    if (is.apa.data == TRUE) {
      message("Using apa.data data frame.")
      assign("data", get("apa.data", envir = globalenv()))
      is_data_frame <- TRUE
    }
  }

  cor_tests <- cocor::cocor(
    formula = formula, data = data,
    alternative = alternative,
    conf.level = conf.level
  )

  n <- cor_tests@n

  diff_r <- cor_tests@diff
  diff_r_txt <- get_r_txt(cor_tests@diff, decimals = 3)

  alternative_txt <- ""
  if (alternative != "two.sided") {
    alternative_txt <- " (one sided)"
  }
  output_txt <- sprintf("$\\Delta r =$ %s", diff_r_txt)

  if (local_options$show.conf.interval == TRUE) {
    LL <- round(cor_tests@zou2007$conf.int[1], 2)
    UL <- round(cor_tests@zou2007$conf.int[2], 2)
    conf_level_percent <- round(conf.level * 100)
    output_txt <- sprintf("%s, %g%% CI[%s,%s]", output_txt, conf_level_percent, LL, UL)
  }

  if (local_options$show.p == TRUE) {
    stat_details <- details_for_test_within(test, cor_tests)
    p_apa_txt <- get_p_apa_txt(stat_details$p.value)
    test_dist <- stat_details$distribution
    test_value <- stat_details$statistic
    if (local_options$show.statistic == TRUE) {
      test_txt <- sprintf("%s = %1.2f, %s", test_dist, test_value, p_apa_txt)
      output_txt <- sprintf("%s, %s%s", output_txt, test_txt, alternative_txt)
    } else {
      output_txt <- sprintf("%s, %s%s", output_txt, p_apa_txt, alternative_txt)
    }
  }

  if (local_options$show.N == TRUE) {
    output_txt <- sprintf("%s, *N* = %g", output_txt, n)
  }
  return(output_txt)
}


details_for_test_within <- function(test_type, test_results) {
  switch(test_type,
    pearson1898 = test_results@pearson1898,
    dunn1969 = test_results@dunn1969,
    hotelling1940 = test_results@hotelling1940,
    hendrickson1970 = test_results@hendrickson1970,
    williams1959 = test_results@williams1959,
    olkin1967 = test_results@olkin1967,
    dunn1969 = test_results@dunn1969,
    steiger1980 = test_results@steiger1980,
    meng1992 = test_results@meng1992,
    hittner2003 = test_results@hittner2003,
    raghunathan1996 = test_results@raghunathan1996,
    silver2004 = test_results@silver2004

  )
}
