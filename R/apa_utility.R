add_to_output <- function(output_txt, new_str) {
  if (output_txt =="") {
    output <- new_str
  } else {
    output <- sprintf("%s, %s", output_txt, new_str)
  }
  return(output)
}


get_r_txt <- function(r, decimals = 2) {
  if (decimals == 2) {
    r_value_txt <- sprintf("%1.2f", r)
  } else {
    r_value_txt <- sprintf("%1.3f", r)
  }
  r_txt <- strip_leading_zero(r_value_txt)
  r_txt
  return(r_txt)
}

strip_leading_zero <- function(string_in) {
  string_out <- string_in
  id_r_is_one <- string_in == "1.00" | string_in == "1.000"
  id_r_is_mone <- string_in == "-1.00" | string_in == "-1.000"
  string_out <- sub(pattern = "0.", replacement = ".", x = string_in)
  string_out[id_r_is_one] <- "1.00"
  string_out[id_r_is_mone] <- "-1.00"

  return(string_out)
}


get_p_apa_txt <- function(p_value) {
  if (p_value < .001) {
    p_txt <- sprintf("*p* < .001")
  } else {
    p_value_txt <- strip_leading_zero(sprintf("%1.3f", p_value))
    p_txt <- sprintf("*p* = %s", p_value_txt)
  }
  return(p_txt)
}

#' Create apaText default options for showing confidence intervals etc.. These options will be used unless overridden by local function arguments
#' @return A list with options object for apaText
#' @examples
#' # You must create an object called apa.default.options
#' # for options to be used, as per below.
#'
#' apa.options <- set.apa.default.options()
#' @export
set.apa.default.options <- function() {

  default_options <- list(
    show.mean = TRUE,
    show.sd = TRUE,
    show.se = FALSE,
    show.conf.interval = TRUE,
    show.N = TRUE,
    show.p = TRUE,
    show.statistic = TRUE,
    number.decimals = 2,
    number.decimals.p = 3,
    use_p_smaller_than_p001 = TRUE
    )
  #show mean, sd, se, and show.decimals need further implementation xyzzy

  return(default_options)
}


set_local_options <- function(arg_options) {
  is.apa.options <- any("apa.options" == ls(globalenv()))
  if (is.apa.options == TRUE) {
    assign("cur_options", get("apa.options", envir = globalenv()))
  } else {
    cur_options <- set.apa.default.options()
  }

  #set local arguments
  if (!is.null(arg_options$show.mean))  {
    cur_options$show.mean <- arg_options$show.mean
  }

  if (!is.null(arg_options$show.sd))  {
    cur_options$show.sd <- arg_options$show.sd
  }

  if (!is.null(arg_options$show.se))  {
    cur_options$show.se <- arg_options$show.se
  }

  if (!is.null(arg_options$show.conf.interval))  {
    cur_options$show.conf.interval <- arg_options$show.conf.interval
  }

  if (!is.null(arg_options$show.N))  {
    cur_options$show.N <- arg_options$show.N
  }

  if (!is.null(arg_options$show.p))  {
    cur_options$show.p <- arg_options$show.p
  }

  if (!is.null(arg_options$show.statistic))  {
    cur_options$show.statistic <- arg_options$show.statistic
  }

  if (!is.null(arg_options$number.decimals))  {
    cur_options$number.decimals <- arg_options$number.decimals
  }

  if (!is.null(arg_options$number.decimals.p))  {
    cur_options$number.decimals.p <- arg_options$number.decimals.p
  }

  if (!is.null(arg_options$use_p_smaller_than_p001))  {
    cur_options$use_p_smaller_than_p001 <- arg_options$use_p_smaller_than_p001
  }

  return(cur_options)
}


get_apa_data <- function (name = "apa.data", renv = parent.frame())
{
  if (identical(renv, emptyenv())) {
    return(NULL)
  }
  if (exists(name, renv, inherits = FALSE)) {
    renv
  }
  else {
    get_apa_data(name, parent.env(renv))
  }
}

get_p_text <- function(p_value, number_decimals_p, use_p_smaller_than_p001, is_one_side_test, is_bonferroni = FALSE) {
  new_str <- sprintf("*p* = %%1.%df",number_decimals_p)
  new_str <- sprintf(new_str, p_value)

  if (p_value < .001 & use_p_smaller_than_p001 == TRUE) {
    new_str <- "*p* < .001"
  }

  if (is_one_side_test == TRUE) {
    new_str <- paste(new_str, "(one-sided)")
  }

  if (is_bonferroni == TRUE) {
    new_str <- paste(new_str, "(Bonferroni adjusted)")
  }

  return(new_str)
}
