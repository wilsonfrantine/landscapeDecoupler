#' @name modelplot
#' @title Plot good of fitness multi-scale models
#' @author Wilson Frantine-Silva
#' @export
#' @description this function takes the output from fitscales
#' and plot the good of fitness acording to a given criterion.
#' The criterion vary among model types, but any column from
#' broom::glance() function will works. The output is a tibble
#' with two new columns "plot.id" and "ggplot". The former holds
#' the ggplot output which can be edited with ggplot2 API.
#' @param x the output of fitscale function
#' @examples
#' \dontrun{
#' ls <- decouple(r,p,c(1000,2000,3000))
#' lsm <- calc_lsm(ls, c("pland","shdi"))
#' bio <- euglossini
#' mod <- fitscales(lsm, bio, "lm")
#' m.plot <- modelplot(mod, "r.squared", plot=F)
#' m.plot
#' }
#' @details
#' By defaut the function plots the criterion by response and predictors.
#' When one has too many predictors, the plot might looks unreadable and filtering is
#' recomended. In addition, the function always highlights with balck dots
#' the p.values lower than 0.05 if such a column is summarized by the "broom"'s API.
#'
#' This 'p.value' means that an specific model is different than NULL,
#' not different each other. For comparing models, users must perform further analysis.
#' Besides NA values are ploted but filled with red color.
#'
#' Users are free to change any of theses features directlly in ggplot objecs from
#' ggplot column. It can be accessed by `myobject$ggplot[plot.id]` with 'plot.id' equal
#' to the desired plot id.
#'
#' It's important to highlight that the ouputed tibble has the same data as inputed,
#' which is stored into the collumn "data" and can be unnested with the command:
#' `myobject %>% unnest(data)`
#'
#' @param criterion a collumn name from the x$glance tibble.
#' By default the function looks for 'r.squared', 'AIC', 'BIC', and 'logLik', respectively.
#' @param plot a logical (defaut is TRUE), indicating whether to print the plots.
#' @details This function returns a ggplot2 object that might be fully customizable.
#' Users might find more details with ?broom::glance.
#' As an alternative, users may also extract any information
#' from models using [model.apply()] function.
#' @seealso [model.apply()] [fitscales()]

modelplot <- function(x, criterion=NULL, plot=T){
  if(is.null(x)) stop ("You must provide a tibble as the output of fitscale()")
  if(is.null(criterion)){
    preferred.criterions <-c("r.squared","AIC","BIC","logLik")
    available.criterions <- preferred.criterions %in% names(x$glance[[1]])
    criterion <- preferred.criterions[which(available.criterions)[1]]
    warning(paste0("No criterion provided."," The: '", criterion, "' was used. \n", "Other available: ", paste(names(x$glance[[1]]), collapse = ", "), "\n See more details in broomExtra::glance documentation" ))
  }

  base_plot <- function(x, ...){
    x %>% ggplot(aes(y = .data[[criterion]], .data$layer))+
      facet_grid(predictor ~ response, scales = "free_y")+
      geom_line(aes(group=.data$response), color="grey10")
  }

  geom_pvalue <- function(x, ...){
    if("p.value" %in% names(x)){
      out <- list(
        geom_point(size=2, aes(fill=.data$p.value), color="black", pch=21),
        scale_fill_stepsn(colours = c("black", "white","white"), breaks=c(0,0.05,1),
                          na.value = "red")
      )
    }else{ out <- geom_point(size=2,pch=21, fill="white") }
    return(out)
  }

  combine_plot <- function(x, ...){
    x %>% base_plot() + geom_pvalue(x) +
      labs(x="scales", y= criterion)
  }

  plot_cl <- function(x, ...){
    temp.cl <- x %>%
      group_by(.data$class) %>% nest
    out <- lapply(seq_along(temp.cl$data), function(y){
        combine_plot(temp.cl$data[[y]])+
        labs(title = paste("Class",temp.cl$class[[y]]))
      })
    out
  }

  run <- function(x, ...){
    temp <- x %>% unnest("glance") %>%
      group_by(.data$level) %>% nest
    if("class" %in% temp$level) class.plots <- plot_cl(temp[temp$level == "class","data"][[1]][[1]]) else class.plots <- NULL
    if("landscape" %in% temp$level) lands.plots <- combine_plot(temp[temp$level == "landscape","data"][[1]][[1]])
    if("class_landscape" %in% temp$level) lands.plots <- combine_plot(temp[temp$level == "class_landscape","data"][[1]][[1]])
    temp           <- x %>% group_by(.data$level, .data$class) %>% nest
    temp$ggplot    <- if(is.null(class.plots)) list(lands.plots) else append(class.plots, list(lands.plots))
    temp$plot.id   <- seq_along(temp$ggplot)
    temp           <- temp %>% unnest(.data$data)
    temp <- temp %>% group_by(.data$plot.id,.data$ggplot,.data$level,.data$class) %>% nest
    return(temp)
  }
  temp <- run(x)
  if(plot){
    for (i in seq_along(temp$ggplot)) {
      skip_to_next <- FALSE
      tryCatch(print(temp$ggplot[[i]]), error = function(e) {
        skip_to_next <<- TRUE
        cat("ERROR: ",conditionMessage(e), " in $ggplot[[",i,"]]", "\n", sep = "")
        })
      if(skip_to_next) { next }
    }
  }
  return(temp)
}
