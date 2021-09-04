#' @name checkraster
#' @param r an object of the class raster
#' @keywords internal
#' @noRd

checkraster <- function(r){
  if(is.null(r)){
    stop("a raster file was not provided")
  }else {
    if(class(r)[1] != "RasterLayer"){
      if(class(r)[1] != "RasterStack"){
        stop("the object provided is not a valid raster object")
      }
    }
  }
}
