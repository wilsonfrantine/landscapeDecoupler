#' @name calc_lsm
#' @export
#' @title Calculate with LandscapeMetrics
#' @param x an output from decouple
#' @param level one of the levels considered by landscapemetrics
#' @param metric one of the metrics accepted by landscapemetrics
#' @param ... any o other parameter from calulate_lsm
#' @description this function return a long format data frame by reshaping a calling of calculate_lsm from landscapeMetrics
calc_lsm <- function(x, level=NULL, metric=NULL, ...) {
  Reduce(
    function(site.i,site.j){merge(site.i,site.j, all=T)},
    lapply(1:length(x), function(k) {
      data.frame(
        "site"= names(x)[k],
        landscapemetrics::calculate_lsm(x[[k]], level, metric, ...)
      )
    })
  )
}
