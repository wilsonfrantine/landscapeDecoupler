#' @name lsm2multifit
#' @author Wilson Frantine-Silva
#' @title Convert landscapeMetrics to Multifit format
#' @export
#' @description this function was designed to merge outputs from [calc_lsm()] and biological data to be used as reponse variables in [multifit()] function. The function expect a single metric for each site which must have the same
#' @param biodata a dataframe with response variables
#' @param lsmdata a dataframe with landscape metrics as [calc_lsm()] output
#' @param level a character indicating the metrics level is "landscape" or "class"
#' @param bio.id the column of the biodata that has the site id as the [calc_lsm()] output
#' @param which.class A character. If level is equal to "class", then the user must indicate which class will be extracted. Otherwise, the function will fail.
#' @examples
#' #For "landscape" level metrics
#' lsmdata <- calc_lsm(decouple(r,p,c(1000,2000,3000)), level = "landscape", metric = "shdi")
#' biodata <- euglossini
#' d2multi <- lsm2multifit(biodata, lsmdata, "landscape", bio.id="site")
#' head(d2multi)
#' multifit(mod = "lm", multief = colnames(d2multi)[grepl("X", colnames(d2multi))],
#'        formula = Abundance ~ multief, data = d2multi,
#'        criterion = "R2", plot_est = FALSE)
#'
#' #For "class" level metrics
#' lsmdata <- calc_lsm(decouple(r,p,c(1000,2000,3000)), level = "class", metric = "pland")
#' biodata <- euglossini
#' d2multi <- lsm2multifit(biodata, lsmdata, "class", which.class = 3)
#' head(d2multi)
#' multifit(mod = "lm", multief = colnames(d2multi)[grepl("X", colnames(d2multi))],
#'          formula = Abundance ~ multief, data = d2multi,
#'          criterion = "R2", plot_est = FALSE)

lsm2multifit <- function(lsm=NULL, biodata=NULL, response=NULL, id.col="site"){
  if(is.null(response)) stop("you must give some response variable to retain")
  if(is.null(lsm)) stop("you must provide a landscape metrics dataframe. \n See call_lsm for details")
  if(is.null(biodata)) stop("you must provide a dataframe with biological data. \n See euglossini object for an example")
  if(!is.null(id.col)) {
    if(!id.col %in% names(lsm)) stop(paste0("the id.col: '",id.col,"' was not found in lsm column names:"))
    if(!id.col %in% names(biodata)) stop(paste0("the id.col: '", id.col, "' was not found in biodata column names"))
  }
    out <- biodata[,c(all_of(id.col),response)] %>% full_join(lsm, by = id.col)
    return(out)
}
