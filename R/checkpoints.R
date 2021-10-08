#' @name checkpoints
#' @title check sampling site data
#' @keywords internal
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
