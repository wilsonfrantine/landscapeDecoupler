#' @name predicor
#' @title Predictor correllation throught scales
#' @description This function analyze the correlation of a predictor across different scales.
#' @return A data frame containing the correlation values between scales
#' @usage predicor(cor.data)
#' @param x a dataframe generated with the functions [calc_lsm] and tranformed with [lsm2wide()] function
#' @examples
#' #Coparing correlation behaviour in nested and decouple multiscales:
#'
#' b <- c(500, 1000, 1500, 2000, 2500, 3000)
#'
#' nest.ls <- nestedscales(r, p, b)
#' dec.ls  <- decouple(r, p, b)
#'
#' #extracting percentage for each class and each scale
#' nest.lsm <- calc_lsm(nest.ls, metric = "pland")
#' dec.lsm  <- calc_lsm(dec.ls,  metric = "pland")
#'
#' #Comparing correlation for the class = 3
#' nestcor <- predicor(lsm2wide(nest.lsm, 3))
#' deccor  <- predicor(lsm2wide(dec.lsm,  3))
#'
#' corcomp    <- data.frame(
#'   strategy    = factor(c(rep("nested", nrow(nestcor)), rep("decoupled", nrow(deccor))), levels = c("nested","decoupled")),
#'   scales      = factor(c(nestcor$scales, deccor$scales), levels=nestcor$scales),
#'   correlation = c(nestcor$correlation, deccor$correlation)
#' )
#' library(ggplot2)
#' ggplot(corcomp, aes(x=scales, y=correlation, fill=strategy))+
#'   geom_bar(stat="identity", position="dodge")+
#'   theme(axis.text = element_text(angle=90))
#'
#' #Now for a metric at the landscape level
#' nest.lsm <- calc_lsm(nest.ls, metric = "shdi")
#' dec.lsm  <- calc_lsm(dec.ls,  metric = "shdi")
#'
#' nestcor <- predicor(lsm2wide(nest.lsm))
#' deccor  <- predicor(lsm2wide(dec.lsm ))
#'
#' corcomp    <- data.frame(
#'   strategy    = factor(c(rep("nested", nrow(nestcor)), rep("decoupled", nrow(deccor))), levels = c("nested","decoupled")),
#'   scales      = factor(c(nestcor$scales, deccor$scales), levels=nestcor$scales),
#'   correlation = c(nestcor$correlation, deccor$correlation)
#' )
#' library(ggplot2)
#' ggplot(corcomp, aes(x=scales, y=correlation, fill=strategy))+
#'   geom_bar(stat="identity", position="dodge")+
#'   theme(axis.text = element_text(angle=90))

predicor <- function(x, ...){
 cor.v <- cor(x[ , grepl(pattern ="X\\d+", colnames(x)) ], ...)
 scales <- t(combn(colnames(cor.v), 2))
 comb <- paste(scales[,1], scales[,2], sep=".")
 data.frame(scales=factor(comb, levels=comb), correlation=cor.v[scales])
}


