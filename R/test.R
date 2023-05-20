

apa.t.test <- function(.data, .iv, .dv, ...) {
  iv <- enquo(.iv)
  dv <- enquo(.dv)

  ivlevels = levels(pull(.data, !!iv))

  grp1rows = dplyr::filter(.data, !!iv == ivlevels[1])
  xvalues = pull(grp1rows, !!dv)

  grp2rows = dplyr::filter(.data, !!iv == ivlevels[2])
  yvalues = pull(grp2rows, !!dv)

  testoutput = t.test(x= xvalues, y=yvalues, ...)

  x = apa.t.output(testoutput)
  return(x)
}
