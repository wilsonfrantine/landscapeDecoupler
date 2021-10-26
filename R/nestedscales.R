#' @name nestedscales
#' @author Wilson Frantine-Silva
#' @export
#' @title Nested Scales
#' @description This function crops out buffers from a landscape ginven a raster file ( _r_ ), one or more sampling sites ( _p_ ) and radii sizes ( _b_ ). The function returns a list with one raster files for each sampling site and one RasterBricks layer for each provided buffer size.
#' @param r a raster object. Raster must be projected to an UTM system, otherwise, buffer sizes will be given in map units.
#' @param p a set of sampling sites (SpatialPointsDataFrame, SpatialPoints simple_features) points from where buffers will be calculated.
#' @param b a vector of buffer sizes (radii in meters) to be calculated. If a single value is provided the function returns a simple buffer cut.
#' @seealso [decouple() ] [decouple.specific() ]
#' @usage nestedscales(r,p,b)
#' @examples
#' path.to.your.raster <- system.file("extdata/raster.grd", package="landscapeDecoupler")
#' path.to.your.sampling.points <- system.file("extdata/pnts.shp", package="landscapeDecoupler")
#' r <- raster(path.to.your.raster)
#' p <- read_points(path.to.your.sampling.points, type="shp")
#' b <- c(1000,2000,3000)
#' your.nested.scales <- nestedscales(r,p,b)
#' plot(your.nested.scales$p02)
nestedscales <- function(r, p, b){
  res <- future_lapply(lbuffers(p,b), function(x){
      ext     <- extent(do.call(rbind, x))
      cropped <- raster::crop(r, ext)
      rlist   <- lapply(x, function(i) raster::mask(cropped, i))
      rbrick  <- raster::brick(rlist)
      names(rbrick) <- unlist(lapply(x, function(i) i$id))
      return(rbrick)
    })
  return(res)
}
