#' @name plot_decoupled
#' @export plot_decoupled
#' @keywords function
#' @title Plot decoupled landscapes
#' @param x a list of raster objects or a raster object
#' @param cols the number of coloumns for a composition. Default = 1
#' @param rows the number of rows for a composition. Default = 4
#' @param colors a vector with hexadecilma or R-colors to be used for plot. If NULL the function will calculate a pallet based on the number of classes from the raster(s)
#' @param pallete a hcl valid pallete (to see options use hcl.pals()). The defualt is "Terrain"
#' @param breaks the conventional breaks from the plot function. These breaks match the values in the raster object and the vector of colors gived by users or functions
#' @param reverse.pal a logical indicating whether to reverse the pallete
#' @param legend a logical whether to the pallete
#' @param ... other valid parameter for the plot function

plot_decoupled <- function (x, cols = 1, rows = 4, colors=NULL, breaks=NULL, pallete="Terrain", reverse.pal=T, legend=T, ...){
  if(is.null(breaks)){
    cuts <- compute_breaks(x)
  }else{
    cuts <- breaks
  }
  if(is.null(colors)) {
    colors<- grDevices::hcl.colors(cuts, pallete, rev = reverse.pal)
  }
  if(is.list(x)){
    if(list_depth(x)==1){
      graphics::par(mfrow=c(rows, cols))
      for (i in 1:length(x)){
        raster::plot(x[[i]], main=names(x[[i]]), legend=legend, col=colors, breaks=cuts, ...)
      }
    }else{
      graphics::frame()
      lapply(x,plot_decoupled, ...)
    }
  }else if (grepl("Raster", class(x))){
    graphics::par(mfrow=c(1,1))
    raster::plot(x, main=names(x), legend=legend, col=colors, breaks=cuts, ...)
  }else {
    stop("apparently x is not either a list or a Raster object class")
  }
}
