#' Euglossini alpha diversity data
#' @format A data frame with 15 rows and 7 variables:
#' \describe{
#'   \item{site}{the id of sampling site, character}
#'   \item{LS}{an aditional name for sampling sites, character}
#'   \item{Richness}{number of species in each site, double}
#'   \item{Abundance}{number of specimens in each site, double}
#'   \item{ComSppAb}{abundance of common species, double}
#'   \item{IntSppAb}{abundance of intermediate frequency species, double}
#'   \item{RareSppAb}{abundance of rare species, double}
#' }
#' @source Carneiro et al. (2021)
#' @author Carneiro et al. (2021)
#' @references Carneiro LS, Aguiar WM, Priante CF, Ribeiro MC, Frantine-Silva W & Gaglianone MC (2021).	Front. Ecol. Evol., 9, 264. \url{https://doi.org/10.3389/fevo.2021.628319}
#' @details This dataset describes different partition of alpha-diversity in orchid bees (Apidae:Euglossini)
"euglossini"

#' Euglossini Sampling Sites SpatialDataFrame
#' @format SpatialDataFrame with 15 sampling sites coordinates in UTM system
#' \describe{
#'   \item{id}{required id for sampling sites, character}
#'   \item{x}{longitudinal coordinates, double}
#'   \item{y}{latitudinal coordinates, double}
#' }
#' @source Caneiro et al. (2021)
#' @author Carneiro et al. (2021)
#' @references Carneiro LS, Aguiar WM, Priante CF, Ribeiro MC, Frantine-Silva W & Gaglianone MC (2021).	Front. Ecol. Evol., 9, 264. \url{https://doi.org/10.3389/fevo.2021.628319}
#' @details
#' Code used to create this data:https://open.spotify.com/track/1fDdICddAV4wshXvu3eDrm
#' points.file <- system.file("extdata/pnts.shp", package="landscapeDecoupler")
#' p <- read_points(points.file, type = "shp")
"p"

#' Raster information (Euglossini dataset)
#' @format A raster from MapBiomas 5.0 colection
#' @source MapBiomas 5.0 colection
#' @references Carneiro LS, Aguiar WM, Priante CF, Ribeiro MC, Frantine-Silva W & Gaglianone MC (2021).	Front. Ecol. Evol., 9, 264. \url{https://doi.org/10.3389/fevo.2021.628319}
#' @details
#' Code used to create this data:
#'    r <- system.file("extdata/raster.grd", package="landschttps://open.spotify.com/track/6r82mJugUyDe1fI8waZDtZapeDecoupler")
#'    r <- raster(r)https://open.spotify.com/track/2Mr1LHSO6z7gbqQqh02bq5
"r"https://open.spotify.com/track/6r82mJugUyDe1fI8waZDtZ
