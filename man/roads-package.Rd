% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roads-package.R
\docType{package}
\name{roads-package}
\alias{roads}
\alias{roads-package}
\title{roads: Road Network Projection}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Project road network development based on an existing road network, target locations to be connected by roads and a cost surface. Road projection methods include minimum spanning tree with least cost path (Kruskal's algorithm (1956) \doi{10.2307/2033241}), least cost path (Dijkstra's algorithm (1959) \doi{10.1007/BF01386390}) or snapping. These road network projection methods are ideal for use with land cover change projection models.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/LandSciTech/roads}
  \item \url{https://landscitech.github.io/roads/}
  \item Report bugs at \url{https://github.com/LandSciTech/roads/issues}
}

}
\author{
\strong{Maintainer}: Sarah Endicott \email{sarah.endicott@ec.gc.ca} (\href{https://orcid.org/0000-0001-9644-5343}{ORCID})

Authors:
\itemize{
  \item Kyle Lochhead \email{Kyle.Lochhead@gov.bc.ca}
  \item Josie Hughes \email{josie.hughes@ec.gc.ca}
  \item Patrick Kirby
}

Other contributors:
\itemize{
  \item Her Majesty the Queen in Right of Canada as represented by the Minister of the Environment (Copyright holder for included functions buildSimList, getLandingsFromTarget, pathsToLines, plotRoads, projectRoads, rasterizeLine, rasterToLineSegments) [copyright holder]
  \item Province of British Columbia (Copyright holder for included functions getGraph, lcpList, mstList, shortestPaths, getClosestRoad, buildSnapRoads) [copyright holder]
}

}
\keyword{internal}
