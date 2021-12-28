#' @title Extract metrics
#' @name extract_metrics
#' @export extract_metrics
#' @param x a list from the decouple function
#' @param countNA whether to count NA values. If it is "no" (the default) the proportion of each class in the raster layer will be calculated by the sum of valid values. Otherwise, it will be proportional to the whole raster sizes. The allowed values correspond to never count ("no"), only if the count is positive ("ifany") and even for zero counts ("always"). There are some "phatological" cases in which two different kinds of NAs will be treated differently. See the rbase table function for details.
extract_metrics <- function(x=NULL, countNA=NULL){
  if(is.null(countNA)){countNA="no"}
  r.val <- lapply(x, raster::getValues)
  counts <- lapply(1:length(r.val), function(j) {
    lapply(1:ncol(r.val[[j]]), function(i){
      temp    <- table(r.val[[j]][,i], useNA = "no")
      percent <- as.numeric(temp)/sum(as.numeric(temp))
      data    <- data.frame("site"    = names(r.val[j]),
                            "inter"   = i,
                            "scale"   = colnames(r.val[[j]])[i],
                            "class"   = as.numeric(names(temp)),
                            "counts"  = as.numeric(temp),
                            "percent" = percent,
                            "n_class" = length(as.numeric(temp)),
                            "heterog" = -sum(percent * log(percent)))
    })
  })
  results <- Reduce(
    function(site.i, site.j, ...) merge(site.i, site.j, all = TRUE, ...),
  lapply(counts, function(each_site) Reduce(
    function(scale.i, scale.j, ...) merge(scale.i, scale.j, all = TRUE, ...),
    each_site
  )))
  results <- results[,-2]
  results <- reshape(results,
                     varying = c("counts","heterog","percent","n_class"),
                     v.names = "value", direction = "long",
                     times = c("counts","heterog","percent","n_class"),
                     timevar = "metrics",
                     idvar = NULL,
                     new.row.names = 1:I(nrow(results)*4))
  return(results)
}
