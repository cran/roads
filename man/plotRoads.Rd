% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRoads.R
\name{plotRoads}
\alias{plotRoads}
\title{Plot projected roads}
\usage{
plotRoads(sim, mainTitle, subTitle = paste0("Method: ", sim$roadMethod), ...)
}
\arguments{
\item{sim}{sim list result from \code{projectRoads}}

\item{mainTitle}{A title for the plot}

\item{subTitle}{A sub title for the plot, by default the \code{roadMethod} is used}

\item{...}{Other arguments passed to raster plot call for the \code{costSurface}}
}
\value{
Creates a plot using base graphics
}
\description{
Plot the results of \code{\link{projectRoads}}
}
\examples{
# demo scenario 1
scen <- demoScen[[1]]

# landing set 1 of scenario 1:
land.pnts <- scen$landings.points.sf[scen$landings.points.sf$set==1,]

prRes <- projectRoads(land.pnts, scen$cost.rast, scen$road.line.sf, "lcp")
plotRoads(prRes, "Title")

}
