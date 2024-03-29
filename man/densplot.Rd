% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric.plot.functions.R
\name{densplot}
\alias{densplot}
\title{Plot metric density through scales}
\usage{
densplot(x=NULL, n=3, level = NULL)
}
\arguments{
\item{x}{a table output from \link{calc_lsm} or \link{extract_metrics}}

\item{n}{the number of classes to be included in the plot}

\item{level}{the level from which metrics must be retrived.
For the moment, only "landscape" or "class" are accepted.
Users can filter the dataset or change properties of the ggplot
output object}
}
\description{
A simple density plot function for mertrics through scales
}
\examples{
\dontrun{
ls.nest <- nestedscales(r,p,b=c(1000,2000,3000))
ls.lsm  <- calc_lsm(ls.nest, metric=c("shdi","sidi","pland"))
densplot(ls.lsm, level="class")
densplot(ls.lsm, level="landscape")
}
}
