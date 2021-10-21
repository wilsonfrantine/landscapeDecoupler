#' @name decouple
#' @export
#' @title Decouple landscape scales
#' @description The function decouple different scales from a raster file, origin locations and radii sizes.
#' @param r a raster object. Raster must be projected to an UTM system, otherwise, buffer sizes will be given in map units. NULL by default
#' @param p a set of Spatial* or simple_features points from where buffers will be calculated
#' @param b a vector of buffer sizes (radius) to be calculated. If a single value is provided the function returns a simple buffer cut.
decouple <- function(r, p, b){
  l <- lapply(lbuffers(p, b), bintersect)
  res <- future_lapply( l, function(x) cropper(r,x) )
  return(res)
}
