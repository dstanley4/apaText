#' Report difference between correlations from different samples
#'
#' Report difference between correlations from different samples, using R Markdown, via cocor package.
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
#' apa.d.compare.across.samples.from.descriptive(formula = ~ rating + learning | height + weight,
#'                              data1 = attitude,
#'                              data2 = women)
#' @export
apa.r.compare.across.samples <- function(formula, data1, data2, alternative = "two.sided", show.conf.interval = NULL, show.N = NULL, show.p = NULL, show.statistic = NULL) {
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





delta_d_ci <- function(d1,n1.a, n1.b, d2, n2.a, n2.b) {
  d1_ci <- MBESS::ci.smd(smd=d1, n.1 = n1.a, n.2 = n1.b)
  d2_ci <- MBESS::ci.smd(smd=d2, n.1 = n2.a, n.2 = n2.b)

  L1 <- d1_ci$Lower.Conf.Limit.smd
  U1 <- d1_ci$Upper.Conf.Limit.smd

  L2 <- d2_ci$Lower.Conf.Limit.smd
  U2 <- d2_ci$Upper.Conf.Limit.smd

  # delta d CI
  LL <- d1 - d2 - sqrt((d1 - L1)^2 + (U2 - d2)^2)
  UL <- d1 - d2 + sqrt((U1 - d1)^2 + (d2 - L2)^2)

  return(c(LL,UL))
}
