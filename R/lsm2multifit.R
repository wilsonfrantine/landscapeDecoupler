#' @name lsm2multifit
#' @author Wilson Frantine-Silva
#' @title Convert landscapeMetrics to Multifit format
#' @export
#' @description this function was designed to shape the data as expected by [multifit()], merging the outputs from [calc_lsm()] and some biological data to be used as reponse variables.
#' @param lsm a data frame from [calc_lsm()]
#' @param biodata a dataframe with response variables; see euglossini example data frame
#' @param level The level to extract the metric; must be "landscape" or "class".
#' @param metrics The metric to be extracted from [calc_lsm()] output table
#' @param class The class to extract the value if level is "class"
#' @param id.col the column of the biodata that has the site id as the [calc_lsm()] output; "site" is default value
#' @return a data frame in wide format with a single value of a given metrics for each scale
#' @examples
#' \dontrun{
#' #For "landscape" level metrics
#' lsmdata <- calc_lsm(decouple(r,p,c(1000,2000,3000)), metric = "shdi")
#' d2multi <- lsm2multifit(lsm = lsmdata, biodata=euglossini,
#' level="landscape", metrics ="shdi")
#' head(d2multi)
#' scales <- colnames(d2multi)[grepl("X", colnames(d2multi))]
#' multifit(mod = "lm", multief = scales,
#'        formula = Abundance ~ multief, data = d2multi,
#'        criterion = "R2", plot_est = FALSE)
#'
#' #For "class" level metrics
#' lsmdata <- calc_lsm(decouple(r,p,c(1000,2000,3000)), level = "class", metric = "pland")
#' biodata <- euglossini
#' d2multi <- lsm2multifit(lsm = lsmdata, biodata=euglossini, level="class", class=3, metrics ="shdi")
#' head(d2multi)
#' #' scales <- colnames(d2multi)[grepl("X", colnames(d2multi))]
#' multifit(mod = "lm", multief = colnames(d2multi)[grepl("X", colnames(d2multi))],
#'          formula = Abundance ~ multief, data = d2multi,
#'          criterion = "R2", plot_est = FALSE)
#'}

lsm2multifit <- function(lsm, biodata=NULL, level=NULL, metrics=NULL, class=NULL, id.col="site"){
  wide.ls.level <- function(lsm, metrics, id.col=id.col){
    temp <- lsm %>% dplyr::filter(level == "landscape")
    if(is.null(metrics)){
      metrics <- unique(lsm$metric)[1]
      warning(paste0("No metric given. First taken: '",metrics,"'"))
    }
    temp <- temp %>% dplyr::filter( .data$metric == metrics) %>%
      tidyr::pivot_wider(id_cols = dplyr::all_of(id.col),
                         names_from = "layer",
                         values_from = "value")
    if(!is.null(biodata)) {temp <- cbind(biodata, metrics) %>% merge(temp) }
    else {temp <- cbind(temp, metrics)}
    return(temp)
  }
  wide.class.level <- function(lsm, metrics, selected.class = class, id.col=id.col){
    temp <- lsm %>% dplyr::filter(level=="class")
    if(is.null(metrics)){
      metrics <- unique(lsm$metric)[1]
      warning(paste0("No metric given. First taken: '",metrics,"'"))
    }
    temp <- temp %>% dplyr::filter(class == selected.class) %>%
      tidyr::pivot_wider(id_cols = dplyr::all_of(id.col),
                         names_from = "layer",
                         values_from = "value")
    if(!is.null(biodata)) {temp <- cbind(biodata, metrics) %>% merge(temp) }
    else {temp <- cbind(temp, metrics)}
    return(temp)
  }
  if(is.null(level)){
    if(length(unique(lsm$level)) == 1) level <- unique(lsm$level) else stop("You don't provided a level variable and it is not unique in the table. Please provide a level.")
  }
  if(level == "class"){
    wide.class.level(lsm, metrics, class, dplyr::all_of(id.col))
  }else if(level == "landscape"){
    wide.ls.level(lsm, metrics, id.col)
  }else{
    stop(paste("For while, the 'level' must be class or landscape"))
  }
}

