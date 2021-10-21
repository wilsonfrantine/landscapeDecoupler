# Imports ####

#' @import methods raster rgeos sf sp graphics
#' @importFrom parallel makeCluster mclapply detectCores stopCluster
#' @importFrom future.apply future_lapply
#' @importFrom landscapemetrics calculate_lsm
#' @importFrom stats reshape

# Functions ####
## Deprecated Functions ####
#' @name return_specifics
#' @noRd
#' @keywords internal
return_specific <- function(r.list , b=NULL ){
      name <- paste(unlist(lapply(r.list[-c(b)],names)), collapse="_")
      temp <- raster::stack(r.list[-c(b)])
      temp <- raster::merge(temp)
      names(temp) <- name
      temp.list <- base::append(r.list[b],temp)
  return(temp.list)
}

#' @name list_depth
#' @noRd
#' @keywords internal
list_depth <- function(x,x.depth=0){
  if(!is.list(x)){
    return(x.depth)
  }else{
    return(max(unlist(lapply(x,list_depth,x.depth=x.depth+1))))
  }
}

## In use functions ####
### Plotting related functions ####
#' @name compute_breaks
#' @noRd
#' @keywords internal
compute_breaks <- function(x){
  if(is.list(x)){
    temp<-lapply(unlist(x),raster::getValues)
    temp<-as.numeric(levels(as.factor(unlist(temp))))
  }else if(grepl("Raster",class(x))){
    temp <- as.numeric(levels(as.factor(raster::getValues(x))))
  }
  return(temp)
}

### Decoupling-related functions
#' @name sbuffers
#' @title Creates a list of Buffers from SpatialPoints and a buffer list
#' @param p a single SpatialPoint object
#' @param b a vector of buffers to be calculated
#' @description this function was designed to be used in parallelal
#' computation receiving data from lbuffers
#' @seealso lbuffers
sbuffers <- function(p, b){
  blist <- lapply(as.list(b), function(x) buffer(p, x))
  names(blist) <- paste0("X",b)
  for(i in 1:length(blist)){blist[[i]]$id <- b[i]}
  return(blist)
}
#' @name bintersect
#' @title Estimates intersection between buffers
#' @param b an SpatialPoligon list with at least two buffers to intersect
bintersect <- function(b){
  append(b[[1]], lapply(1:I(length(b)-1), function(x) b[[x+1]]-b[[x]] ))
}
#' @name cropper
#' @title Crop rasters at maximum buffer size
#' @param r a raster as input
#' @param b a Spatial* or Raster* object with extent properties
#'
cropper <- function(r, b){
  ext     <- do.call(raster::merge, lapply(b, function(x) raster::extent(x)))
  cropped <- raster::crop(r, ext)
  rlist   <- lapply(b, function(x) raster::mask(cropped, x))
  rbrick  <- raster::brick(rlist)
  names(rbrick) <- unlist(lapply(b, function(x) x$id))
  return(rbrick)
}
#' @name lbuffers
#' @title Creates a list of Buffers from a list of SpatialPoints
#' @param p a SpatialPointsDataFrame
#' @param b a vector of buffer radii in metters to be calculated
#' @description this function was designed to create a list of SpatialPolygons
lbuffers <- function(p, b) {
  list <- sp_list(p)
  plist <- lapply(list, function(x) sbuffers(x,b))
  names(plist) <- unlist(lapply(list, function(x) x$id))
  return(plist)
}
#' @name sp_list
#' @title Convert SpatialPoints to a list
#' @param p a set of SpatialPoints
#' @keywords internal
sp_list <- function(p){
  res.list <- list()
  if( TRUE %in% grepl("sf", class(p)) ) {
    p <- as_Spatial(p)
  }
  for(i in 1:length(p)){
    res.list[[i]] <- p[i,]
  }
  return(res.list)
}
### Other utility functions ####
#' @name sum.classes
#' @title summarize classes
#' @description Several times we would like aggregate two or more classes.
#' This function takes the output from extract_metrics or calc_lsm and
#' summarize any given classes by user provide function
#' @param x an output from extract metrics
#' @param vals the values of classes that should be summarized
#' @param var the metric to be summed
#' @param fun a function to summarize the metric. Popular functions are "sum", "mean"...
#' Users can also use custom functions as: function(x){log(x+1)}
#' @seealso extract_metrics calc_lsm sum.scales
#' @noRd
sum.classes <- function(x, vals, var="percent", fun="sum"){
  sites   = unique(x$site)
  scales  = unique(x$scale)
  classes = unique(x$class)

  all.comb <- expand.grid("class"= sort(classes), "scale" = scales, "site"=sites)

  td <- merge(x, all.comb, all=TRUE)
  td[is.na(td)] <- 0

  exp <- td$class %in% vals
  fd <- td[exp,]
  fd <- fd[fd$metrics == var,]

  sum.data <- aggregate(value ~ site+scale, data=fd, FUN=fun)
  sum.data <- sum.data[order(sum.data$site),]
  names(sum.data) <- c(names(sum.data)[1:2], var)

  return(sum.data)
}

#' @name sum.scales
#' @title summarize scales
#' @description This function takes the output from extract_metrics or calc_lsm and
#' summarize scales by any provided metric and function.
#' Popular functions ara "mean","sum"...
#' @param x an output from extract metrics
#' @param vals the values that should be summed
#' @param var column name indicating which metrics might be summed
#' @param fun a function to sum the metrics. Popular functions are "sum", "mean".
#' Users can also use custom functions as: function(x){log(x+1)}
#' @seealso extract_metrics calc_lsm sum.classes
#' @noRd
sum.scales <- function(x, metric, fun="mean"){
  sites   = unique(x$site)
  scales  = unique(x$scale)
  classes = unique(x$class)

  all.comb <- expand.grid("class"= sort(classes), "scale" = scales, "site"=sites)

  td <- merge(x, all.comb, all=TRUE)
  td[is.na(td)] <- 0

  fd <- td[td$metrics == metric,]

  sum.data <- aggregate(value ~ site+scale, data=fd, FUN=fun)
  sum.data <- sum.data[order(sum.data$site),]
  names(sum.data) <- c(names(sum.data)[1:2], metric)

  return(sum.data)
}

