#' @title function for decoupling outter rings
#' @name decoupler
#' @description This function is for internal use only
#' @param r a raster as impute.
#' @param p a spatialPoint to use as buffer center
#' @param radius a vector of radius to decoupling. If the subjected raster is not in ultrametric system, the radius will be then calculated in map units
#' @param id identifiers for each of p objects that will be passed to layers' name
#' @keywords internal

decoupler <- function (r, p, radius, id){

  output <- list()

  e <- raster::extent(raster::buffer(p, max(radius)))
  r1 <- raster::crop(r, e)

  buffer1 <- raster::buffer(p, radius[1])
  layer <- raster::mask(r1, buffer1)
  names(layer) <- paste0(id,"_",radius[1])
  output[[1]] <- layer

  for(i in  1:(length(radius)-1)){

    buffer1 <- raster::buffer(p, radius[i])
    buffer2 <- raster::buffer(p, radius[i+1])

    intersection <- buffer2 - buffer1

    layer <- raster::mask(r1,intersection)
    names(layer) <- paste0(id,"_",radius[i+1])
    output[[i+1]] <- layer

  }

  return(output)
}



