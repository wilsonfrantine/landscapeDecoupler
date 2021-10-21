#' @name nestedscales
#' @export
#' @title Design Multiscale landscapes
#' @description The function crop out buffers from a landscape raster file, origin Spatial points and radii sizes.
#' @param r a raster object. Raster must be projected to an UTM system, otherwise, buffer sizes will be given in map units.
#' @param p a set of Spatial* or simple_features points from where buffers will be calculated
#' @param b a vector of buffer sizes (radii in meters) to be calculated. If a single value is provided the function returns a simple buffer cut.
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
