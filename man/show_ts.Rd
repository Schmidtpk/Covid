% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time series plots.R
\name{show_ts}
\alias{show_ts}
\title{Show time series of region/province}
\usage{
show_ts(
  df = it,
  regions = c("Lombardia", "Lombardy"),
  outcome = "growth",
  measures = it.measures
)
}
\arguments{
\item{df}{data.frame}

\item{regions}{regions to be considered}

\item{outcome}{outcome shown}

\item{measures}{measures that are plotted}
}
\value{
ggplot
}
\description{
Show time series of region/province
}
\examples{
show_ts()
}
