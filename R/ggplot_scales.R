
#' Get the colour values from the ggplot default discrete colour scale,
#' when using `n` distinct colours
#' @export
smv_ggplot_discrete_cols = function(n) {
  scales::hue_pal()(n)
}
