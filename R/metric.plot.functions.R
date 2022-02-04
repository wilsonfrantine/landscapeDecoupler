#' @name densplot
#' @title Plot metric density through scales
#' @export
#' @description A simple density plot function for mertrics through scales
#' @param x a table output from [calc_lsm] or [extract_metrics]
#' @param n the number of classes to be included in the plot
#' @param level the level from which metrics must be retrived.
#' For the moment, only "landscape" or "class" are accepted.
#' Users can filter the dataset or change properties of the ggplot
#' output object
#' @usage densplot(x=NULL, n=3, level = NULL)
#' @examples
#' \dontrun{
#' ls.nest <- nestedscales(r,p,b=c(1000,2000,3000))
#' ls.lsm  <- calc_lsm(ls.nest, metric=c("shdi","sidi","pland"))
#' densplot(ls.lsm, level="class")
#' densplot(ls.lsm, level="landscape")
#' }
densplot <- function(x=NULL, n=3, level=NULL){
  if(is.null(x)) stop("You must provide a dataset. Run calc_lsm or extract_metrics first")
  if(is.null(level)){
    plotlevel <- level <- "landscape"
  }else {plotlevel <- level}
  xf <- x %>% filter(level==plotlevel)
  tab  <- sort(table(x$class), decreasing = T)[1:n]
  if(level=="class"){ ggx <- xf %>% filter(class %in% names(tab)); what="class"}
  if(level=="landscape") {ggx <- xf; what = "metric";}

  Okabe.Ito.colors<-c("#E69F00", "#56B4E9","#009E73",
                      "#F0E442", "#0072B2","#D55E00",
                      "#CC79A7", "#FFF000", "#000000")

  ggplot(ggx, aes(y=.data$value),group=get(what))+
    geom_density(aes(fill=get(what), group=get(what)), color="grey10", alpha=.65)+
    facet_grid(layer~metric)+
    coord_flip()+
    theme_minimal()+
    theme(panel.grid = element_blank())+
    scale_fill_manual(
      values = Okabe.Ito.colors
    )+
    labs(y="metric value", fill = "")
}
#' @name stackplot
#' @title Stack density plot through scales
#' @export
#' @description A simple stack density plot function for mertrics through scales
#' @param x a table output from [calc_lsm] or [extract_metrics]
#' @param metric a metric of choice to be ploted
#' @param level the level from which metrics must be retrived.
#' For the moment, only "landscape" or "class" are accepted.
#' Users can filter the dataset or change properties of the ggplot
#' output object
#' @usage stackplot(x=NULL, level="class", metric=NULL)
#' @examples
#' \dontrun{
#' ls.nest <- nestedscales(r,p,b=c(1000,2000,3000))
#' ls.lsm  <- calc_lsm(ls.nest, metric=c("shdi","sidi","pland"))
#' stackplot(ls.lsm, level="class", "pland")
#' stackplot(ls.lsm, level="landscape", "shdi")
#' }
stackplot <- function (x=NULL, level="class", metric=NULL){
  if(is.null(metric)){ metric <- x$metric[[1]]; warning("No metric provided. First was taken")}
  if(level == "class")x <- x%>%filter(.data$level==level) else if(level == "landscape") x <- x%>% filter(.data$level=="landscape") else stop("Levels must be class or landscape. ID is not supported for while.");
  Okabe.Ito.colors<-c("#E69F00", "#56B4E9","#009E73",
            "#F0E442", "#0072B2","#D55E00",
            "#CC79A7", "#FFF000", "#000000")
  ggplot(x%>%filter(.data$level=="class"),
         aes(x=.data$value, group=.data$class,fill=.data$class,color=.data$class))+
      geom_density(position="fill", adjust=1.5, aes(group=.data$class), alpha=0.75)+
      facet_wrap(~layer)+
      scale_fill_manual(values = Okabe.Ito.colors)+
    scale_color_manual(values=Okabe.Ito.colors)+
    theme_minimal()+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          strip.placement = "none")+
    labs(x=metric)
}
screenscales <- function(x=NULL, metric=NULL,  sum_fun = "mean_se",
                         split_by="class",color_by="metric", facet_labels=NULL){
  if(is.null(metric)) metric <- x$metric[1]; warning("A metric was not provided. First was taken.")
  if(is.null(facet_labels)){
    facet_labels <- x %>% pull(var = split_by) %>% unique %>% as.vector
    facet_labels[is.na(facet_labels)] <- "NA"
    names(facet_labels) <- facet_labels
  }
  ggplot(
    data    = x  %>% group_by(.data$layer),
    mapping = aes(x=.data$layer, y=.data$ value,
                  color=.data[[color_by]], fill=.data[[color_by]])  )+
    stat_summary(fun.data = sum_fun, size=0.5)+
    labs(title="Metrics accros scales", x= "scales", y=paste(sum_fun,"of metric values"))+
    facet_wrap(facets = split_by, scales = "free",
              labeller = as_labeller(facet_labels))+
    theme(
      axis.text.x = element_text(angle=90),
      axis.line = element_line(color = "grey10")
    )+
    scale_color_brewer(palette = "Dark2")
}
