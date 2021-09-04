#' @name decouple
#' @export
#' @title Decouple landscape scales
#' @description The function decouple different scales from a raster file from a set of a buffers calculated from provided points and radius sizes.
#' @param r a raster object. Raster must be projected to an UTM system, otherwise, buffer sizes will be given in map units. NULL by default
#' @param p a vector of points from where buffers will be calculated
#' @param b a vecotr of buffer sizes (radius) to be calculated. If a single value is provided the function returns a simple buffer cut.
#' @param dec.specific this parameter controls whether the function will evaluate specific scales sepparetly. If the parameter is seted as c(1,3) and buffers as c(100, 200, 300, 400), the first and third buffers (100, 300) will be decoupled and the second and fourth will be merged in the final list. By default the parameter is setted as null, so all buffers are analyzed as independent unites.
#' @param progress a logical whether to show progress or not.

decouple <- function(r=NULL, p=NULL, b=NULL, dec.specific=NULL, progress=TRUE){

  #parameters checking
  p1 <- checkpoints(p)
  checkraster(r)
  if(class(b) != "numeric"){ stop("Radius buffers are not numeric") }

  #parameters addaptation
  p1  <- methods::as(p1, "SpatialPoints")
  ids <- p$id

  #recursive decoupling
  r.list <- list()
  #progress monitoring
  if(progress == T) {
    pb <- utils::txtProgressBar(0, length(p1), 0, style=3)
  }

  for(i in 1:length(p1)){
    r.list[[i]] <- decoupler(r, p1[i], b, ids[i])
    utils::setTxtProgressBar(pb, i)
  }

  #specific decoupling
  if(!is.null(dec.specific)){
    if(is.numeric(dec.specific)){
      r.list <- return_specific(r.list, dec.specific)
    }else{
      stop("specific buffers muts be a numeric vector")
    }
  }

  #function returning
  return(r.list)
}
