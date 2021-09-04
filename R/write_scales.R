#' @description  This function saves each uncoupled scale as file
#' @export write_scales
#' @name write_scales
#' @title write scales
#' @author Wilson Frantine-Silva
#' @param x an output of decouple function or a list of raster files
#' @param buffer.names a vector of names same length as buffer scales or number of raster files on the list to be saved. If null, rasters will be saved as layers names.
#' @param format one of the valids output in rgdal package or raster native defaults. For details, see raster::writeRaster help.
#' @param path a path to save raster. Working directory is the default
#' @param overwrite a boolean whether to overwrite files. TRUE by default
#' @seealso decouple, raster::writeRaster, raster::writeFormats

write_scales <- function(x=NULL, buffer.names=NULL, format='GTiff', path=NULL, overwrite=T){
  if(is.null(x)){
    stop("A list of raster has been not provided. Check if you have a list of raster as input")
  }

  if(is.null(path)){
    path<-getwd()
  }
  for (i in 1:length(x)){
    raster <- raster::stack(x[[i]])
    if(is.null(buffer.names)){
      buffer.names <- names(raster)
    }
     raster::writeRaster(raster,
                filename=file.path(path, paste0(as.character(names(raster)))),
                bylayer=T, sufix=buffer.names, format=format, overwrite=T)
  }
}
