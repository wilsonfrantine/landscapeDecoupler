#' @title check sampling site data
#' @name checkpoints
#' @keywords internal
#' @concept Sampling sites can be either lat long coordinates, spatialPoints, spatialPointsDataFrame or simple features objects. This function makes an interpretation of these data and deliver a package-readable input
#'@param p a set of georreferenced dataPoints

checkpoints <- function (p){
  if(is.null(p)){
    stop("You did not provide a valid point parameter.")
  }else if(class(p)[1] == "SpatialPoints"){
    warning("You provided SpatiaPoints which have no id assigned.
            They will be identified by input order")
    result <- methods::as(p, "SpatialPointsDataFrame")
    result@data <- base::data.frame("id"=1:length(p))
  }else if(class(p)[1] == "SpatialPointsDataFrame"){
    result <- p
  }else if(class(p)[1] == "sf"){
    result <- sf::as_Spatial(p)
  }
  if(is.null(result@data$id)){
    warning("Your points have no id assigned. They will be identified by input order")
    result@data$id <- 1:length(result)
  }
  return(result)
}
