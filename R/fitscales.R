#' @name fitscales
#' @author Wilson Frantine-Silva
#' @title Fit regression models through spatial scales
#' @export
#' @description This function is an alternative to multifit with the
#' advantage of not being restricted to one response variable per time.
#' The function manage the aplication of regression  models (model) with
#' different formulas (formula) using both landscape metrics (lsm) and
#' biological variables (bio) in interchangeable  roles as predictor
#' and response variables. The function will return a tibble with
#' response and predictor pair applyed to n scales (layer) in many
#' levels, class and id as returned in the lsm object calculated with [calc_lsm()].
#'
#' @param lsm an output of the [calc_lsm] or [extract_metrics] functions.
#' Default is NULL
#' @param bio a table with other information to be used in the models.
#' See the 'euglossini' data, accessible through typing euglossini in
#' the console, for more details. Default is NULL.
#' @param model an valid model name function, as lm or glm, to be
#' called and calcula the model objects. Default is NULL
#' @param formula any valid formula to be used in the models. Default is
#' "responses ~ predictors" which returns pairwise combinations of predictors
#' as lsm metrics and bio vars as responses. Users can take advantage of
#'  the wildcards "responses" and "predictors". See details for more information.
#' @param responses a vector specifying which responses variables must be
#' considered while building models. If null, all response variables are
#' returned. Default is null.
#' @param model.arg a vector or a list with any other arguments required for
#' the models functins. It's often used to inform the family of regression,
#' e.g. c(family="poisson") or list(family="poisson", "model" = FALSE, ...)
#' @param quiet a logical to inform whether or not return low concerning warnings.
#' Critical warnings are always returned.
#'
#' @usage fitscales(lsm=NULL,bio=NULL,model=NULL, formula="responses ~ predictors",
#'  responses=NULL, model.arg=NULL, quiet=FALSE)
#'
#' @seealso [modelplot()], [calc_lsm()], [extract_metrics()], [multifit()]
#'
#' @details This functions was designed to work with any model package and object
#' supported by the broom framework, which is used to manage the output. Therefore,
#' users must be sure to load the model's package before to run the analysis,
#' as well as that the formula is suitable for the dataset, and that the
#' broom family (broom, broommixed, and broomExtra) can handle the model output.
#'
#' Fomula wildcards:
#' To make the process more smooth, we implemented two wildcards: "responses" and
#' "predictors". By combining these wildcards users might have some facilities as:
#' "responses ~ predictors" : will return all pairwise combinations of the lsm
#'  metrics as predictors and 'bio table' as responses for a given model and formula;
#' "predictors": by using predictors as the seccond term of the function,
#' e.g. "A ~ predictors", users will have models for each predictor over the
#' response variable "A".
#' "responses": using responses as the first term of the function
#' (e.g "responses ~ a+b") will return models for all numeric responses
#' variables in the "bio table". Users can have models for specific sets
#' of response variables by specifying the parameter `response` of the function,
#' which will not requires variables to be numeric.
#'
#' The output tibble will present several columns but a single line for each
#' calculated model. For each single model, the output will always have the following
#' columns:
#' response: the response variable used to that model calculation
#' predictor: same as response, but for predictors variable used
#' layer: the scale for which the model was calculated
#' class: same as layer when the metrics considering classes
#' id: same as layer and classes when both are required by the metric calculation
#' data: the data passed to the model calculation
#' models: the model object itselves.
#' tidy: the summary of the model estimation
#' glance: the adjusment estimatiors for each model, as AIC, BIC, r_squared...
#' augment: the model residuals and other information for each record on data
#'
#' For other information about tidy, glance and augment,
#' users may look at the broom package family.
#'
#' @examples
#' \dontrun{
#' ls  <- decouple(r,p,b=c(1000,2000,3000))
#' lsm <- calc_lsm(ls, c("pland","shdi"))
#' bio <- euglossini ##see documentantion for details
#' models <- fitscales(lsm, bio, "glm", "responses ~ predictors",
#' responses=c("Abundance", "Richness"), model.arg=c(family="gaussian"), quiet=F)
#' head(models)
#'
#' ## Other usages:
#' mods.all.pairs            <- fitscales(lsm,bio,"lm","responses ~ predictors")
#' mods.some.resp.all.pred   <- fitscales(lsm,bio,"lm","responses ~ predictors",
#'  responses = c("Abundance","Richness"))
#' mods.all.resp.one.pred    <- fitscales(lsm,bio,"lm","responses ~ shdi+pland_3")
#' mods.one.resp.all.pred    <- fitscales(lsm,bio,"lm","Abundance ~ predictors")
#' mods.one.formule.for.all  <- fitscales(lsm,bio,"lm","Abundance ~ shdi+pland_3")
#' mods.resp.operations      <- fitscales(lsm,bio,"lm","log(Abundance)+Richness ~ shdi+pland_3")
#'
#' }

fitscales<-function(lsm=NULL,bio=NULL,model=NULL, formula="responses ~ predictors", responses=NULL, model.arg=NULL, quiet=FALSE){
  ##************Check block**************************####
  stop.vars <- list(lsm, bio, model)
  stop.msgs <- list(
    "lsm.error"   = "\n You did not provided a lsm output, run 'calc_lsm()' or 'extract_metrics()' first",
    "bio.error"   = "\n You did not provided a bio table, import some data to use as response variable. See 'read.table()'.",
    "model.error" = "\n You did not provided a model variable. Tell which model you want, e.g.: 'lm' or 'glm'"
  )
  warn.msgs  <- list(
    "formula.warn"   = "You've specify no formula! 'responses ~ predictors' was used",
    "ds.dif.length"  = "Some datasets have different numbers of rows. Look at the data column",
    "only.numeric"   = "Some response variables are not numeric and was excluded as response variables. You can force any response varible by setting the 'responses' function parameter in fitscale()"
  )
  stop.check <- lapply(stop.vars, is.null) %>% unlist()
  if(TRUE %in% stop.check) stop(stop.msgs[which(stop.check)], call. = F)

  ##Functions Block *********************************#####
  generic.formula <- function(formula){
    if(formula == "responses ~ predictors") return(TRUE) else return(FALSE)
  }
  formula.processer <- function(x){
    aformula  <- as.formula(x)
    terms     <- terms.formula(aformula)
    fhands    <-gsub(" ","", x) %>% strsplit("~") %>% unlist()
    return(list("formula" = aformula, "terms"=terms, lefthand=fhands[1], righthand=fhands[2], original.call=x))
  }
  check.metrics.missmatch <- function(x,f.list){
    is.in.metric <- attr(f.list$terms, "term.labels") %in% x$metric
    if(TRUE %in% is.in.metric){
      stop(paste0("formula!", "\n You gave '", attr(f.list$terms, "term.labels")[is.in.metric],
                  "' in your formula, but these terms are expected: \n",
                  paste0(attr(f.list$terms, "term.labels")[is.in.metric], "_",
                         levels(lsm$class), collapse = ", "),
                  "\n That is because each class has its own value, and will be treated as single predictor \n")  )
    }
  }

  model.extr.info <- function(x){
    t    <- lapply(x, broomExtra::tidy)
    g    <- lapply(x, broomExtra::glance)
    a    <- lapply(x, broomExtra::augment)
    temp <- list("models"= x, "tidy"=t,"glance"=g,"augment"=a)
    return(temp)
  }

  model.fitter.generic <- function(x, model, model.arg=NULL, ...){
    model.data        <- data.frame(x$response.v, x$value, row.names = x$site)
    model.formula     <- as.formula(paste0(x$response.var[1],"~",x$predictors[1]))
    responses         <- all.vars(model.formula)[!all.vars(model.formula) %in% labels(terms(model.formula))]
    predictors        <- labels(terms(model.formula))
    names(model.data) <- c(responses, predictors)
    call.args         <- list("formula"  = model.formula,
                              "data"     = as.name("model.data"))
    if(!is.null(model.arg)) call.args <- append(call.args, model.arg)
    return(do.call(model, call.args))
  }
  data.shaper.generic  <- function(lsm, bio, responses=NULL){
    #this function return a nested tibble grouped by response_var, scale and metric
    if(is.null(responses) && generic.formula(formula)){
      temp <- names(bio)[!names(bio) %in% "site"]
      responses <- temp[lapply(bio[,temp], is.numeric) %>% unlist()]
    }
    temp <- bio %>%
      tidyr::pivot_longer(cols      = all_of(responses),
                          names_to  = "response",
                          values_to = "response.v") %>%
      dplyr::full_join(lsm, by = "site") %>%
      mutate(response.var = .data$response, predictors = .data$metric) %>%
      dplyr::group_by(.data$class, .data$metric, .data$response, .data$layer) %>%
      tidyr::nest() %>% dplyr::arrange(.data$metric, .data$response, .data$class) %>%
      dplyr::rename("predictor" = .data$metric)

    check.length <- function(ds){
      data.length  <- ds$data %>% lapply(nrow) %>% unlist()
      comparison   <- data.length < max(data.length)
      if(TRUE %in% comparison){warning(warn.msgs$ds.dif.length, call. = F)}
    }
    check.length(temp)
    return(temp)
  }

  data.shaper.specific  <- function(lsm, bio, formula, responses=NULL){
    f.list <- formula.processer(formula)
    if(f.list$lefthand == "responses"){
      if(is.null(responses)) responses <- colnames(bio)[!colnames(bio) %in% "site"]
      predictors  <- f.list$righthand
      numeric.res <- lapply(bio[,responses], is.numeric) %>% unlist
      if(TRUE %in% !numeric.res){
        cols <- names(numeric.res[!numeric.res])
        warning(paste0("Non-numeric variable dected: \n", names(bio[cols]),
                       "\n Numeric and non-numeric variables cannot be handled together.",
                       "\n Models returned only for numeric variables.",
                       "\n Please, use the parameter 'responses' in fitscales() to select one or",
                       " more vars of the same type.") )
        responses   <- names(numeric.res[numeric.res])
      }
      if("class" %in% lsm$level){
        cltemp <- lsm %>% filter(.data$level == "class")

        check.metrics.missmatch(cltemp, f.list)

        temp <- cltemp %>% group_by(.data$layer) %>% nest %>%
          mutate(data = .data$data %>%
                   lapply( function(x) pivot_wider(x, "site", c("metric","class"),
                                                   values_from = "value")))
        lstemp <- lsm %>% filter(.data$level == "landscape") %>%
          group_by(.data$layer) %>% nest %>% mutate(data = .data$data %>%
                                                      lapply( function(x) pivot_wider(x, "site", c("metric"),values_from = "value")))
        temp$data <- lapply(seq_along(temp$data), function(x){
          full_join(lstemp$data[[x]],temp$data[[x]],by="site")
        })

        temp.bio <- bio[, c("site", responses)] %>%
          pivot_longer(responses, names_to = "response", values_to = "responses") %>%
          full_join(cltemp, by="site") %>% mutate(resp_name = .data$response)
        temp <- temp.bio[,c("site", "resp_name", "response", "responses", "layer")] %>%
          unique %>% group_by(.data$layer, .data$response) %>% nest %>% full_join(temp, by="layer")

        temp$data <- lapply(seq_along(temp$response), function(x){
          full_join(temp$data.x[[x]], temp$data.y[[x]], by="site")
        })
        temp <- temp[,c("response","layer","data")]
        temp$predictor <- predictors
        temp$level     <- as.factor(paste(levels(lsm$level), collapse = "_"))
        temp$class     <- as.factor(paste(levels(lsm$class), collapse = "_"))
        temp$id        <- as.factor(if(length(levels(lsm$id)) > 0) paste(levels(.data$id), collapse = "_") else NA)
      }
      else if("landscape" %in% lsm$level){

        lstemp <- lsm %>% filter(.data$level == "landscape") %>%
          group_by(.data$layer) %>% nest %>%
          mutate(data = .data$data %>%
                   lapply( function(x){
                     pivot_wider(x, "site", "metric", values_from = "value")%>%
                       full_join(bio, by="site")
                   })
          )
        temp <- lsm %>% filter(.data$level=="landscape") %>%
          group_by(.data$layer) %>%
          select(-.data$metric,-.data$value)
        temp <- bio %>% select(.data$site, responses) %>%
          pivot_longer(responses, names_to = "response") %>%
          select(.data$site, .data$response) %>%
          full_join(temp, by="site") %>% select(-.data$site) %>%
          mutate("predictor" = predictors) %>% unique %>%
          full_join(lstemp, by="layer")
      }
    }
    else if(f.list$righthand == "predictors"){
      predictors <- attr(f.list$terms, "term.labels")
      responses  <- f.list$lefthand
      temp <- lsm %>% full_join(bio, by="site") %>%
        rename("predictor" = .data$metric, "predictors" = .data$value) %>%
        mutate(temp="temp") %>% group_by(temp) %>% nest %>%
        cbind("response" = responses)
      temp <- temp[,c("response", "data")] %>% unnest(.data$data) %>%
        group_by(.data$response, .data$layer, .data$level, .data$class, .data$id, .data$predictor) %>% nest
    }
    else if(!generic.formula(formula)){
      if(is.null(responses)){responses <- f.list$lefthand}
      lsm %>% filter(.data$level == "class") %>% check.metrics.missmatch(f.list)
      temp.cl <- lsm %>% group_by(.data$layer, .data$level) %>% nest %>% filter(.data$level == "class")
      temp.cl$data <- temp.cl$data %>% lapply(function(x) pivot_wider(x, "site", c("metric", "class")))
      temp.ls <- lsm %>% group_by(.data$layer, .data$level) %>% nest %>% filter(.data$level == "landscape")
      temp.ls$data <- temp.ls$data %>% lapply(function(x) pivot_wider(x, "site", "metric"))
      temp.data <- lapply(seq_along(temp.ls$data), function(x){
        full_join(temp.ls$data[[x]], temp.cl$data[[x]], by="site") %>%
          full_join(bio, by="site")
      })
      temp <- lsm %>% full_join(bio, by= "site") %>% group_by(.data$layer) %>% nest %>%
        cbind(response=responses, predictor = f.list$righthand,
              level=paste0(levels(lsm$level), collapse = "_"),
              class=paste0(levels(lsm$class), collapse = "_"),
              id   = if(length(levels(lsm$id)) > 0) paste0(levels(lsm$id), collapse = "_") else NA)
      temp$data <- temp.data
    }
    return(temp)
  }
  model.fitter.specific <- function(x=NULL, model=NULL, model.arg=NULL, formula=NULL){
    model.data        <- x
    model.formula     <- as.formula(formula)
    call.args         <- list("formula"  = model.formula,
                              "data"     = as.name("model.data"))
    if(!is.null(model.arg)) call.args <- append(call.args, model.arg)
    return(do.call(model, call.args))
  }

  ##**Execution Block ***************************************************####
  if(generic.formula(formula)){
    if(quiet == F && is.null(formula)) warning(warn.msgs["formula.warn"], call. = F)
    df            <- data.shaper.generic(lsm, bio, responses = responses)
    models        <- lapply(df$data, model.fitter.generic, model, model.arg)
    df$mod.id     <- seq_along(df$data)
    temp          <- model.extr.info(models) %>% as_tibble
    temp$mod.id   <- seq_along(temp$models)
    out           <- full_join(df[,c("response","layer", "class", "data","mod.id")],temp, by="mod.id")
    out           <-       out %>% unnest(.data$data) %>% group_by(.data$response, .data$predictors, .data$layer, .data$level,
                                                                   .data$class, .data$id, .data$mod.id, .data$models, .data$tidy,
                                                                   .data$glance, .data$augment)%>% nest
    colnames(out)[colnames(out) == 'predictors'] <- 'predictor'
  }
  else{
    df            <- data.shaper.specific(lsm, bio, formula, NULL)
    models        <- lapply(df$data, model.fitter.specific, model, model.arg, formula)
    df$mod.id     <- seq_along(df$data)
    temp          <- model.extr.info(models) %>% as_tibble
    temp$mod.id   <- seq_along(temp$models)
    out           <- full_join(df,temp, by="mod.id")
  }
  out <- out[,c("mod.id", "response", "predictor", "layer", "level", "class", "id", "data", "models", "tidy", "glance", "augment")]
  return(out)
}
