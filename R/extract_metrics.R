#'@title Extract metrics
#'@name extract_metrics
#'@export extract_metrics
#'@param x a list from the decouple function
#'@param countNA whether to count NA values. If it is "no" (the default) the proportion of each class in the raster layer will be calculated by the sum of valid values. Otherwise, it will be proportional to the whole raster sizes. The allowed values correspond to never count ("no"), only if the count is positive ("ifany") and even for zero counts ("always"). There are some "phatological" cases in which two different kinds of NAs will be treated differently. See the rbase table function for details.

extract_metrics <- function(x=NULL, countNA=NULL){

  if(is.null(countNA)){countNA="no"}

  results.list <- list()
  scales.list <- list()
  for(i in 1:length(x)){
    for(j in 1:length(x[[1]])){
      vals <- raster::getValues(x[[i]][[j]])
      counts <- table(matrix(vals), useNA = countNA)
      proportions <- counts/sum(counts)
      scales <- names(x[[i]][[j]])[1]
      data <- data.frame("sample"        = i,
                         "scale"         = scales,
                         "class"         = counts,
                         "proportions"   = as.numeric(proportions),
                         "n_classes"     = length(counts),
                         "heterogeneity" = -sum( as.numeric(proportions) * log(proportions))

      )
      names(data) <- c("sample", "scale", "class", "count", "percent", "n_classes", "heterog")
      scales.list[[j]] <- data
    }
    results.list[[i]] <- scales.list
  }
  return(results.list)
}
