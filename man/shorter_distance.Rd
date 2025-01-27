% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{shorter_distance}
\alias{shorter_distance}
\title{shoter distance}
\usage{
shorter_distance(x, y, x1, y1, x2, y2)
}
\arguments{
\item{x}{(double) x of point}

\item{y}{(double) y of point}

\item{x1}{(vector) x1 of segment}

\item{y1}{(vector) y1 of segment}

\item{x2}{(vector) x2 of segment}

\item{y2}{(vector) y2 of segment}
}
\value{
(vector) distance to segment (dist),
                 derivate in x (dx),
                 derivate in y (dy)
}
\description{
Give shoter distance a point to extrem point of segment and 
             derivate in x and y
}
