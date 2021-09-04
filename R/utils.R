#Declaring some imports to link to the name space

#' @import methods raster rgeos sf sp

#' @name return_specifics
#' @noRd
#' @keywords internal
return_specific <- function(r.list , b=NULL ){
  s <- list()
  for (i in 1:length(r.list)) {
      name <- paste(unlist(lapply(r.list[[i]][-c(b)],names)), collapse="_")
      temp <- raster::stack(r.list[[i]][-c(b)])
      temp <- raster::merge(temp)
      names(temp) <- name
      temp.list <- base::append(r.list[[i]][b],temp)
      s[[i]] <- temp.list
  }
  return(s)
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

