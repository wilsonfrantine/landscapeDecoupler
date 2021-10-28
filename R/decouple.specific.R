#' @name decouple.specific
#' @title Decouple specific scales
#' @description This function extract specific scales as  given a multilayer raster object
#' @export
#' @param ls an output of [decouple()] or [nestedscales()] functions
#' @param specific which scales will be kept independent of the others. If you have a three layer object from the decouple or nestedscale functions, then specific can be 1, 2, 3 or any combination of it as c(1,2) or c(2,3). Using all scales in the ls object will return the same object.
#' @examples
#' decoupled.ls <- decouple(r,p, c(500,1000,2000,3000))
#' spec  <- decouple.specific(decoupled.ls, c(1,3))
#' plot(decoupled.ls$p02)
#' plot(spec$p02)
#' @seealso [decouple()], [nestedscales()]

decouple.specific <- function(ls=NULL, specific=NULL){
  if(is.null(ls)) stop("ls is NULL and must be provided")
  if(is.null(specific)) {stop("`specific` is NULL, but must be provided")}
  else if(!is.numeric(specific)){stop("`specific` is not numeric. Please check if you entered it right")}
  else if(length(specific) >= raster::nlayers(ls[[1]])-1) stop("You are trying to keep so many scales as layers (or n_scales - 1) in the raster object. It will result in the same object you already have. \n Execution helted!")
  spec <- future_lapply(1:length(ls), function(x) {
    rstack <- raster::stack(
      ls[[x]][[specific]], raster::merge(ls[[x]][[-specific]])
    )
    names(rstack) <- c(names(rstack)[-length(names(rstack))],"merged_scales")
    return(rstack)
  })
  names(spec) <- names(ls)
  return(spec)
}
