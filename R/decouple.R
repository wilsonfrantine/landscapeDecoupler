#' @name decouple
#' @author Wilson Frantine-Silva
#' @export
#' @title Decouple landscape scales
#' @description Return a list of RasterBricks with decoupled scales from a raster file, given sampling points and radii sizes.
#' @param r a raster object. Raster must be projected to UTM system, otherwise, buffer sizes will be given in map units.
#' @param p a set of Spatial* or simple_features points from where buffers will be calculated
#' @param b a vector of buffer sizes (radius) to be calculated. If a single value is provided the function returns a simple buffer cut.
#' @usage decouple(r,p,b)
#' @examples
#' r <- raster(system.file("extdata/raster.grd", package = "landscapeDecoupler"))
#' p <- read_points(system.file("extdata/pnts.shp", package = "landscapeDecoupler"), type = "shp")
#' b <- c(750,1500,3000)
#' my.decoupled.landscape <- decouple(r,p,b)
#' @seealso [decouple.specific()], [nestedscales()]
#' @details this function was designed to sequentially perform both decoupling strategies: symetric (same size buffers) asymetric. Buffer sizes are meant to be higher than zero and increasing in size. Different settings will result in an unexpected behaviour.
#' The functions can also runs in parallel using the "Future" API. Users must only set the parallelization extrategy with [future::plan()] and run the function.

decouple <- function(r, p, b){
  l <- lapply(lbuffers(p, b), bintersect)
  res <- future_lapply( l, function(x) cropper(r,x) )
  return(res)
}
