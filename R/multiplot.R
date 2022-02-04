#' @name multiplot
#' @export multiplot
#' @keywords function
#' @title Plot multiscale landscape objects
#' @param x a list of multilayer raster objects
#' @param colors a vector with hexadecimal or R-colors to be used for plot. If NULL the function will calculate a pallet based on the number of classes from the raster(s)
#' @param palette a hcl valid palette (to see all the options use [hcl.pals]). The defualt is "Terrain". Other popular are:  "Heat","Heat 2","Terrain 2","Viridis","Plasma","Inferno" and 112 other paletts.
#' @param breaks the conventional breaks from the plot function. These breaks match the values in the raster object and the vector of colors given by users or functions
#' @param reverse.pal a logical indicating whether to reverse the pallet
#' @param extra.arg other valid parameter to pass to the rasterVis::levelplot function. See details.
#' @param ... arguments to be passed to further functions
#' @seealso [rasterVis::levelplot]
#' @details this function was designed to facilitate the plotting of objects returned by the functions [decouple], [decouple.specific], and [nestedscales]. Users can control specificities about different aspects of plotting categorical raster and creating copositions of such plots.
#' This function was based on [rasterVis::levelplot] function and users might find more details
#' in this function help.
#' @examples
#' \dontrun{
#' r <- landscapeDecoupler::r
#' p <- landscapeDecoupler::p
#' b <- c(1000,2000)
#' ls <- decouple(r, p, b)
#' multiplot(ls) #plot every element in the object
#' multiplot(ls[1]) #return the scale name
#' multiplot(ls[[1]]) #also plot, but with no names
#' multiplot(ls$p01)  #same above, but different calling
#' multiplot(ls[1:2]) #a subset of the object
#' multiplot(ls[1:4], extra.arg=list( layout=c(1,4), scales=list(tick=F, mark=F) )  )
#' multiplot(ls$p01, palette = "viridis")
#' #more paletts with hcl.pals()
#' class.colors <- c("green", "red", "yellow", "#AAAAFF", rgb(0.1,0.5,0.9,1))
#' multiplot(ls$p01, colors=class.colors)
#' }
multiplot <- function (x, colors=NULL, breaks=NULL, palette="Terrain", reverse.pal=F, extra.arg=NULL, ...){

  plotlayer <- function(x, scale.name=NULL, extra.arg=NULL, ...){
    arg <- extra.arg
    rat <- raster::merge(x); rat <- rat[] %>% raster::as.factor() %>% levels %>% as.data.frame() %>% list(); colnames(rat[[1]]) <- "ID"
    y <- lapply(seq(nlayers(x)), function(i){
      temp <- raster::ratify(x[[i]])
      suppressWarnings(
        levels(temp) <- rat
      )
      return(temp)
    })
    x <- raster::brick(y)

    if(is.null(arg$layout)) arg$layout <- grDevices::n2mfrow(raster::nlayers(x))
    if(is.null(arg$scales)) arg$scales <- list()
    if(is.null(breaks)) breaks <- raster::levels(x)[[1]]$ID
    if(is.null(colors)) colors <- grDevices::hcl.colors(length(breaks), palette, rev = reverse.pal)

      r_levels <- raster::levels(x) %>% unlist
      r_colors <- colors[ breaks %in% r_levels ]
      r_breaks <- breaks [breaks %in% unlist(r_levels)]

      return(
        rasterVis::levelplot(x,layers=seq(raster::nlayers(x)), att="ID", main=scale.name,
                             col.regions=colors, layout=arg$layout, scales=arg$scales, ...)
      )
  }
  plotlist <- function(x, ...){
    return(lapply(seq_along(x), function(i) plotlayer(x[[i]], names(x[i]), ...)))
  }
  if(is.list(x)){
    return(plotlist(x, ...))
  }else if (grepl("Raster", class(x))){
    return(plotlayer(x, ...))
  }else {
    stop("apparently x is not either a list or a Raster object class")
  }
}
