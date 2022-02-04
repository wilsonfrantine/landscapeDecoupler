#' @name model.apply
#' @title Apply any function over models
#' @export
#' @description  this function allow users to apply any function
#' over models nested in tibbles as the output of the [fitscales()]
#' function. Users may use this function to retrieve model info,
#' as using summary, or apply their own personalized functions
#' over the models. The function will return a similar tibble
#' to the input table, but with an extra column with the result
#' of the function applied.
#' @param data a tibble. An output from [fitscales] function
#' @param fun vector. The name of a function to be applied. Any function that can handle
#' model objects are allowed, specially tidy(), glance(), augment() from
#' broom, broom.mixed and broomExtra packages. Custom functions are also
#' allowed, but they have to be loaded in the environment space.
#' @param result_to the new collumn name. It is optional. If null,
#' the new collumn name will be 'model.'+fun
#' @details This function was designed to help users expanding the
#' possibility in handle data from model objects, specially for
#' those not covered by broom API. The package broom.mixed can
#' be used to handle mixed models. Users only need to install and
#' library the package, so the functions tidy, glance, and augment
#' will be also applied for packages like 'lme4' and others.
#' @examples
#' \dontrun{
#' ls <- decouple(r,p,c(1000,2000,3000))
#' lsm <- calc_lsm(ls, c("pland","shdi"))
#' models <- fitscales(lsm, bio=euglossini, model="lm")
#' model.apply("summary",models, result_to="mod.summ")
#' }
#' @seealso [fitscales()] [calc_lsm()] [decouple()] [nestedscales()]

model.apply <- function(fun, data, result_to=NULL){
  if(!is.character(fun)) stop("You must provide the function name as a string, i.e. between quotes")
  if(is.null(result_to)) result_to <- paste0("model.",fun)
  data %>% dplyr::mutate(
    !!result_to := purrr::map(models, get(fun))
  )
}
