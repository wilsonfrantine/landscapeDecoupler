#' @name calc_lsm
#' @export
#' @title Calculate with LandscapeMetrics
#' @param x an output from [decouple()] [decouple.specific()] or [nestedscales()]
#' @param metric Character: one of the metrics in landscapemetrics. See list_lsm()
#' @param level Character: "landscape", "class" or "patch". Optional.
#' @param ... any other parameter from calulate_lsm() in landscapemetrics
#' @description This function calculates any metrics implemented in landscapemetrics package
#'  using objects returned from [decouple()], [decouple.specific()] or [nestedscales()].
#' @examples
#' \dontrun{
#' #You don't need to run the next two lines
#' r <- raster(system.file("extdata/raster.grd", package="landscapeDecoupler"))
#' p <- read_points(system.file("extdata/pnts.shp", package="landscapeDecoupler"), type="shp")
#' #setting buffers sizes
#' b = c(1000,2000,3000)
#' x <- decouple(r, p, b)
#' calc_lsm(x, level = "landscape", "shdi")
#' }
#' @seealso [extract_metrics()], [decouple()], [decouple.specific()],  [nestedscales()] , [landscapemetrics::calculate_lsm()]

calc_lsm <- function(x, metric=NULL, level=NULL, ...) {
 temp.df <- Reduce(
    function(site.i,site.j){merge(site.i,site.j, all=T)},
    future.apply::future_lapply(seq_along(x), function(k) {
      df<-data.frame(
        "site"  = names(x)[k],
        landscapemetrics::calculate_lsm(x[[k]], level, metric)
      )
      return(df)
    }, future.seed = 1234)
  )
 temp.df$layer <- as.factor(temp.df$layer)
 levels(temp.df$layer) <- names(x[[1]])
 df <- temp.df[,1:6] %>% lapply(as.factor) %>%
   as.data.frame() %>% cbind("value"=temp.df[,7]) %>%
   dplyr::as_tibble()
 return(df)
}
