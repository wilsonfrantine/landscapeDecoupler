#Internal functions #################################################
## Check-related functions ##########################################
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
#' @name checkpoints
#' @title check sampling site data
#' @keywords internal
#' @noRd
#' @concept Sampling sites can be either lat long coordinates, spatialPoints, spatialPointsDataFrame or simple features objects. This function makes an interpretation of these data and deliver a package-readable input
#'@param p a set of georreferenced dataPoints
checkpoints <- function(p){
  pattern <- c("sf", "SpatialPoints", "SpatialPointsDataFrame","character")
  result <- switch( match( class(p), pattern )[1] ,
                    sf::as_Spatial(p),
                    methods::as(p, "SpatialPointsDataFrame"),
                    result <- p,
                    if(file.exists(p)){
                      if(grepl(".txt",p)){result <- as_Spatial(read_points(p, type = "txt"))}
                      else if(grepl(".shp", p)){result <- as_Spatial(read_points(p, type="shp"))}
                    }
  )
  return(result)
}
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

## Plotting-related Functions ###########################################
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

## Decoupling-related functions ################################################

#' @title function for decoupling outter rings
#' @name decoupler
#' @description This function is for internal use only
#' @param r a raster as impute.
#' @param p a spatialPoint to use as buffer center
#' @param radius a vector of radius to decoupling. If the subjected raster is not in ultrametric system, the radius will be then calculated in map units
#' @param id identifiers for each of p objects that will be passed to layers' name
#' @keywords internal
#' @noRd

decoupler <- function (r, p, radius, id){
  output <- list()
  e <- raster::extent(raster::buffer(p, max(radius)))
  r1 <- raster::crop(r, e)

  buffer1 <- raster::buffer(p, radius[1])
  layer <- raster::mask(r1, buffer1)
  names(layer) <- paste0(id,"_",radius[1])
  output[[1]] <- layer

  for(i in  1:(length(radius)-1)){

    buffer1 <- raster::buffer(p, radius[i])
    buffer2 <- raster::buffer(p, radius[i+1])

    intersection <- buffer2 - buffer1

    layer <- raster::mask(r1,intersection)
    names(layer) <- paste0(id,"_",radius[i+1])
    output[[i+1]] <- layer
  }
  return(output)
}
#' @name sbuffers
#' @title Creates a list of Buffers from SpatialPoints and a buffer list
#' @param p a single SpatialPoint object
#' @param b a vector of buffers to be calculated
#' @description this function was designed to be used in parallelal
#' computation receiving data from lbuffers
#' @seealso lbuffers
#' @noRd
#' @keywords internal
sbuffers <- function(p, b){
  blist <- lapply(as.list(b), function(x) buffer(p, x))
  names(blist) <- paste0("X",b)
  for(i in 1:length(blist)){blist[[i]]$id <- b[i]}
  return(blist)
}
#' @name bintersect
#' @title Estimates intersection between buffers
#' @param b an SpatialPoligon list with at least two buffers to intersect
#' @noRd
#' @keywords internal
bintersect <- function(b){
  append(b[[1]], lapply(1:I(length(b)-1), function(x) b[[x+1]]-b[[x]] ))
}
#' @name cropper
#' @title Crop rasters at maximum buffer size
#' @param r a raster as input
#' @param b a Spatial* or Raster* object with extent properties
#' @noRd
#' @keywords internal
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
#' @keywords internal
#' @noRd
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
#' @noRd
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
