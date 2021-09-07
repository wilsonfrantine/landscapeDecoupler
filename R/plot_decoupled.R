#' @name plot_decoupled
#' @export plot_decoupled
#' @keywords function
#' @title Plot decoupled landscapes
#' @param x a list of raster objects or a raster object
#' @param cols the number of coloumns for a composition. Default = 1
#' @param rows the number of rows for a composition. Default = 4
#' @param colors a vector with hexadecilma or R-colors to be used for plot. If NULL the function will calculate a pallet based on the number of classes from the raster(s)
#' @param pallete a hcl valid pallete (to see options use hcl.pals()). The defualt is "Terrain"
#' @param breaks the conventional breaks from the plot function. These breaks match the values in the raster object and the vector of colors given by users or functions
#' @param reverse.pal a logical indicating whether to reverse the pallet
#' @param legend a logical whether to the pallet
#' @param ... other valid parameter for the plot function

plot_decoupled <- function (x, cols = NULL, rows = NULL, colors=NULL, breaks=NULL, pallete="Terrain", reverse.pal=T, legend=T, ...){
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
    rows=2
  }
  if(is.list(x)){
    if(list_depth(x)==1){
      graphics::par(mfrow=c(rows,cols), mar=c(2,2,2,2))
      for( i in 1:length(x)){
        x[[i]] <- as.factor(x[[i]])
        r_levels <- levels(x[[i]])[[1]]
        r_colors <- colors[ breaks %in% unlist(r_levels) ]
        r_breaks <- c(min(breaks) - 1,
                      breaks [breaks %in% unlist(r_levels)])
        raster::plot(x[[i]], main=names(x[[i]]), legend=F,
                     col=r_colors, legend.width=1.75)
        if(legend==T){
          legend("right", inset = -(0.15*log(cols+0.8,2)),
                 xjust = 0, yjust = 0.5,
                 legend = unlist(r_levels), fill=r_colors,
                 xpd=NA, bty = "n", x.intersp = 0.5,
                 y.intersp = 0.8,
                 title = "class")
        }
      }
    }else{
      graphics::par(mfrow=c(rows,cols), mar=c(2,2,2,2))
      for(j in 1:length(x)){
        for(i in 1:length(x[[j]])){
          x[[j]][[i]] <- as.factor(x[[j]][[i]])
          r_levels <- levels(x[[j]][[i]])[[1]]
          r_colors <- colors[ breaks %in% unlist(r_levels) ]
          r_breaks <- c(min(breaks) - 1,
                        breaks [ breaks %in% unlist(r_levels) ])
          raster::plot(x[[j]][[i]], main=names(x[[j]][[i]]),
                       legend=F, col=r_colors )
          if(legend==T){
            legend("right", inset = -(0.15*log(cols+0.8, 2.3)),
                   xjust = 0, yjust = 0.5,
                   legend = unlist(r_levels), fill=r_colors,
                   xpd=NA, bty = "n", x.intersp = 0.5,
                   y.intersp = 0.8,
                   title = "class")
          }
        }
      }
    }
  }else if (grepl("Raster", class(x))){
    graphics::par(mfrow=c(1,1))
    x<-as.factor(x)
    r_levels <- levels(x)
    r_colors <- colors[ breaks %in% unlist(r_levels) ]
    r_breaks <- c(min(breaks) - 1, breaks [breaks %in% unlist(r_levels)])
    raster::plot(x, main=names(x), legend=F,
                 col=r_colors)
    if(legend==T){
      legend("right", inset = -(0.15*log(cols+1, 3)),
             xjust = 0, yjust = 0.5,
             legend = unlist(r_levels), fill=r_colors,
             xpd=NA, bty = "n", x.intersp = 0.5,
             y.intersp = 0.8,
             title = "classes")
    }
  }else {
    stop("apparently x is not either a list or a Raster object class")
  }
}
