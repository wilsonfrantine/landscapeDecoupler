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

lsm2multifit <- function(biodata=NULL, lsmdata=NULL, level=NULL, bio.id = NULL, which.class=NULL){
  # verifications
  if(is.null(biodata)) stop("biodata is NULL, but it is required")
  if(is.null(lsmdata)) stop("lsmdata is NULL, but it is required")
  if(is.null(level)){
    level <- "landscape"
    warning("`level` is NULL, it will be taken as 'landscape'")
  }
  if(is.null(bio.id)){
    bio.id <- names(biodata)[1]
    warning("`bio.id` is NULL: `biodata` first column was taken")
  }
  if(level=="landscape"){
    if("landscape" %in% lsmdata$level){
      temp <- reshape(lsmdata, idvar = bio.id, v.names = "value",
                      timevar = "layer", direction = "wide",
                      times = unique(lsmdata$layer),
                      drop = c("class", "level", "metric","id"))
      names(temp) <- gsub("value.","",names(temp))
      merged <- merge(biodata, temp)
      return(merged)
    }else{stop("it seems that the level in your lsmdata table is not `landscape` as you entered. Please, check it out.")}
  }else if(level=="class"){
    if(is.null(which.class)){
      stop(" `which.class` is needed but not provided")
    }else if(which.class %in% lsmdata$class){
      lsmdata <- subset(lsmdata, subset = lsmdata$class %in% which.class)
      temp <- reshape(lsmdata, idvar = bio.id, v.names = "value",
                      timevar = "layer", direction = "wide",
                      times = unique(lsmdata$layer),
                      drop = c("class", "level", "metric","id"))
      names(temp) <- gsub("value.","",names(temp))
      merged <- merge(biodata, temp)
      return(merged)
    }
  }else {stop("`level` must be one of `landscape` or `class`")}
}
