#' @name wide.lsm
#' @title Landscape Metrics to wide format
#' @description this function is designed to convert [calc_lsm()] output to a wide format, in which each scale (e.g. "X1000", "X1500", ...) will be a column with a single metric value associated with.
#' @param x a data frame from [calc_lsm()]
#' @param level The level to extract the metric; must be "landscape" or "class".
#' @param metric The metric to be extracted. Must be present in the calc_lsm output table
#' @param class The class to extract the value if level is "class"
#' @return a data frame in wide format with a single value for each scale
#' @export
#' @examples
#' multi.ls <- decouple(r, p, c(1000, 2000, 3000))
#' lsmdata <- calc_lsm(multi.ls, metric="shdi")
#' head(lsmdata)
#' wide.lsm(x = lsmdata, level="landscape", metric="shdi")
wide.lsm <- function(x, level=NULL, metric=NULL, class=NULL){
 wide.ls.level <- function(x, metric){
   temp <- x %>% filter(level == "landscape")
   if(is.null(metric)){
     metric <- unique(x$metric)[1]
     warning(paste0("No metric given. First taken: '",metric,"'"))
   }
   temp <- temp %>% filter(metric==metric) %>%
     pivot_wider(id_cols = site,
                 names_from = layer,
                 values_from = value)
   return(temp)
  }
  wide.class.level <- function(x, metric, selected.class = class){
    temp <- x %>% filter(level=="class")
    if(is.null(metric)){
      metric <- unique(x$metric)[1]
      warning(paste0("No metric given. First taken: '",metric,"'"))
    }
    temp <- temp %>% filter(class == selected.class) %>%
      pivot_wider(id_cols = site,
                  names_from = layer,
                  values_from = value)
    return(temp)
  }
  if(is.null(level)){
    if(unique(x$level == 1)) level <- unique(x$level) else stop("You don't provided a level variable and it is not unique in the table. Please provide a level.")
  }
  if(level == "class"){
    wide.class.level(x, metric, class)
  }else if(level == "landscape"){
    wide.ls.level(x, metric)
  }else{

  }
}
