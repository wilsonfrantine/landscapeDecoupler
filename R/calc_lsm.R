#' @name calc_lsm
#' @export
#' @title Calculate with LandscapeMetrics
#' @param x an output from decouple() decouple.specific() or multiscales()
#' @param level Character: "landscape", "class" or "patch"
#' @param metric Character: one of the metrics in landscapemetrics. See list_lsm()
#' @param ... any o other parameter from calulate_lsm() in landscapemetrics
#' @description this function return a long format data frame by reshaping a calling of calculate_lsm from landscapeMetrics
#' @examples
#' #don't need to run this
#' r <- raster(system.file("extdata/raster.grd", package="landscapeDecoupler"))
#' p <- read_points(system.file("extdata/pnts.shp", package="landscapeDecoupler"), type="shp")
#' #setting buffers sizes
#' b = c(1000,2000,3000)
#' x <- decouple(r, p, b)
#' calc_lsm(x, level = "landscape", "shdi")
#' @seealso [extract_metrics() ], [decouple() ], [decouple.specific() ],  [multiscales() ] , [landscapemetrics::calculate_lsm() ]

calc_lsm <- function(x, level=NULL, metric=NULL, ...) {
  Reduce(
    function(site.i,site.j){merge(site.i,site.j, all=T)},
    lapply(1:length(x), function(k) {
      df<-data.frame(
        "site"  = names(x)[k],
        landscapemetrics::calculate_lsm(x[[k]], level, metric)
      )
      temp <- as.factor(df$layer)
      levels(temp)<-names(x[[k]])
      df$layer <- temp
      return(df)
    })
  )
}
