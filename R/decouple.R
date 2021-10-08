#' @name decouple
#' @export
#' @title Decouple landscape scales
#' @description The function decouple different scales from a raster file from a set of a buffers calculated from provided points and radius sizes.
#' @param r a raster object. Raster must be projected to an UTM system, otherwise, buffer sizes will be given in map units. NULL by default
#' @param p a set of Spatial* or simple_features points from where buffers will be calculated
#' @param b a vecotr of buffer sizes (radius) to be calculated. If a single value is provided the function returns a simple buffer cut.
#' @param specific this parameter controls whether the function will return specific scales sepparetly. If the parameter is seted as c(1,3) and buffers as c(100, 200, 300, 400), the first and third buffers (100, 300) will be decoupled and the second and fourth will be merged in the final list. By default the parameter is setted as null, so all buffers are analyzed as independent unites.
#' @param parallel a logical whether to compute with more than a cluster. By default, the function is calculated with all available cores less one.
decouple <- function(r, p, b, specific=NULL, parallel=detectCores()-1 ){
  if(parallel!=F){
    cl  <- parallel::makeCluster(parallel)
    res <- parallel::mclapply(lapply(lbuffers(p, b), bintersect),
                              function(x) cropper(r,x) )
    if(!is.null(specific)){
      res.spec <- mclapply(1:length(res), function(x) {
        rstack <- raster::stack(
          res[[x]][[specific]], raster::merge(res[[x]][[-specific]])
        )
        names(rstack) <- c(names(rstack)[-length(names(rstack))],"merged_scales")
        return(rstack)
      })
      names(res.spec) <- names(res)
      res <- res.spec
    }
    parallel::stopCluster(cl)
  }else{
    res <- lapply(lapply(lbuffers(p, b), bintersect) ,
                  function(x) cropper(r, x))
    if(!is.null(specific)){
      res.spec <- lapply(1:length(res), function(x) {
        rstack <- raster::stack(
          res[[x]][[specific]], raster::merge(res[[x]][[-specific]])
        )
        names(rstack) <- c(names(rstack)[-length(names(rstack))],"merged_scales")
        return(rstack)
      })
      names(res.spec) <- names(res)
      res <- res.spec
    }
  }
  return(res)
}
