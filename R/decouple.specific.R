#' @name decouple.specific
#' @title Decouple specific scales
#' @param ls an output of decouple or nestedscales functions
#' @param specific which scales decouple out. If you have decouple four scales, let's say, three (100, 200, 300) in the decouple or nestedscale functions, then specific can be 1, 2, 3 or any combination of it as c(1,2) or c(2,3). Using all scales in the ls object will return the an equal object.
#' @export
decouple.specific <- function(ls, specific){
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
