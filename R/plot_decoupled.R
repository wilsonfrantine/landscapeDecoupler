#' @name plot_decoupled
#' @export plot_decoupled
#' @keywords function
#' @title Plot decoupled landscapes
#' @param x a list of raster objects or a raster object
#' @param cols the number of coloumns for a composition. Default = 1
#' @param rows the number of rows for a composition. Default = 4
#' @param colors a vector with hexadecilma or R-colors to be used for plot. If NULL the function will calculate a pallet based on the number of classes from the raster(s)
#' @param pallete a hcl valid pallete (to see options use [hcl.pals() ]). The defualt is "Terrain"
#' @param breaks the conventional breaks from the plot function. These breaks match the values in the raster object and the vector of colors given by users or functions
#' @param reverse.pal a logical indicating whether to reverse the pallet
#' @param legend a logical whether to the pallet
#' @param leg.adj a adjustment multiplyer to legend x position. Usefull when changing cols number in plot area. Default is 1, but values between 0.9 and 1.2 should work fine.
#' @param ... other valid parameter for the plot function
#' @seealso [raster::plot() ], [hcl.pals() ]
#' @details this function was designed to facilitate the plotting of objects returned by the functions [decouple() ], [decouple.specific() ], and [nestedscales() ]. Users can control specificities about different aspects of plotting categorical raster and creating copositions of such plots.
#' @examples
#' r <- landscapeDecoupler::r
#' p <- landscaleDecoupler::p
#' b <- c(1000,2000)
#' ls <- decouple(r, p, b)
#' plot_decoupled(ls$p01)
#' plot_decoupled(ls[[1]])
#' plot_decoupled(ls[1:2], cols=2, rows=2)
#' plot_decoupled(ls[1:4], cols = 4, rows = 2)
#' plot_decoupled(ls$p01, pallete = "viridis", rows = 1)
#' class.colors <- c("green", "red", "yellow", "#AAAAFF", rgb(0.1,0.5,0.9,1))
#' plot_decoupled(ls$p01, colors=class.colors)
plot_decoupled <- function (x, cols = NULL, rows = NULL, colors=NULL, breaks=NULL, pallete="Terrain", reverse.pal=T, legend=T, leg.adj= 1.1, ...){
  if(is.null(breaks)){
    breaks <- compute_breaks(x)
  }
  if(is.null(colors)) {
    colors<- grDevices::hcl.colors(length(breaks), pallete, rev = reverse.pal)
  }
  if(is.null(cols)){
    cols=2
  }
  if(is.null(rows)){
    rows=1
  }
  adj.vec <- c(-0.11,-0.25,-0.25,-0.37,-0.50,-0.75,
               -1.05,-1.60,-2.70,-5.70,
               -0.0734 * 1.4969^(11:50))
  if(is.list(x)){
    if(list_depth(x)==1){
      graphics::par(mfrow=c(rows,cols), mar=c(2,2,2,3))
      for(i in 1:length(x)){
        for(l in 1:nlayers(x[[i]])){
          temp<-as.factor(x[[i]][[l]])
          r_levels <- levels(temp)
          r_colors <- colors[ breaks %in% unlist(r_levels) ]
          r_breaks <- c(min(breaks) - 1, breaks [breaks %in% unlist(r_levels)])
          raster::plot(temp, main=paste(names(x)[i],names(temp)), legend=F,
                       col=r_colors)
          if(legend==T){
            legend("right", inset = adj.vec[cols]*leg.adj,
                   xjust = 0.5, yjust = 0,
                   legend = unlist(r_levels), fill=r_colors,
                   xpd=NA, bty = "n", x.intersp = 0.2,
                   y.intersp = 0.8,
                   title = "class")
          }
        }
      }
    }
  }else if (grepl("Raster", class(x))){
    graphics::par(mfrow=c(rows,cols), mar=c(2,2,2,3))
    for(l in 1:nlayers(x)){
      temp  <- as.factor(x[[l]])
      r_levels <- levels(temp)
      r_colors <- colors[ breaks %in% unlist(r_levels) ]
      r_breaks <- c(min(breaks) - 1, breaks [breaks %in% unlist(r_levels)])
      raster::plot(temp, main=names(temp), legend=F,
                   col=r_colors)
      if(legend==T){
        legend("right", inset = adj.vec[cols]*leg.adj,
               xjust = 0, yjust = 0,
               legend = unlist(r_levels), fill=r_colors,
               xpd=NA, bty = "n", x.intersp = 0.2,
               y.intersp = 0.8,
               title = "class")
      }
    }
  }else {
    stop("apparently x is not either a list or a Raster object class")
  }
  par(mfrow=c(1,1), mar=c(4,4,4,4))
}
