#' @name read_points
#' @title read georeferenced points
#' @export
#' @param x a valid pathfile to the points
#' @param type the specific filetype to be readed. Accepted types:
#' Shape files: "shp"
#' txt files: "txt"
#' @param id Only aplicable if the type is csv or txt.
#' The name of the field containing sites id
#' @param lat Only aplicable if the type is csv or txt.
#' The name of the field containing sites latitude coordinates
#' @param long Only aplicable if the type is csv or txt.
#' The name of the field containing sites longitude coordinates
#' @param delimiter a character to delimite the columns of the text file with the coordinates
#' @usage read_points(x,type, delimiter,id, lat, long)
#' @examples
#' points.file <- system.file("extdata/pnts.shp", package="landscapeDecoupler")
#' p <- read_points(points.file,type="shp")
#' plot(landscapeDecoupler::r)
#' plot(p, add=TRUE)
#' text(p, labels=p$id)


read_points <- function(x, type="txt", delimiter="\t", id="id",
                        lat="x", long="y"){
  points <-
    switch(type,
           shp = sf::read_sf(x),
           txt = read_txt(x, delimiter, id, lat, long)
           )
  return(checkpoints(points))
}
############Mini functions to switche
#' @name read_txt
#' @keywords internal
#' @noRd
read_txt <- function (x, delimiter, id, lat, long){
  points <- utils::read.table(x, header = T, sep = delimiter)
  out <- sp::SpatialPointsDataFrame(data=points,
                                    coords = points[,c(lat,long)])
  return(out)
}
