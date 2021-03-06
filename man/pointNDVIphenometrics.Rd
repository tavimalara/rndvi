% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pointNDVIphenometrics.R
\name{pointNDVIphenometrics}
\alias{pointNDVIphenometrics}
\title{Calculate annual phenological metrics of NDVI series for point coordinates}
\usage{
pointNDVIphenometrics(yrs, lns, lts, nms)
}
\arguments{
\item{yrs}{vector c() or sequence seq() of years do be extracted}

\item{lns}{vector of longitudes of point locations}

\item{lts}{vector of latutudes of point locations}

\item{nms}{vector of names of point locations}
}
\value{
A object of the class SpatialPointsDataFrame will be
created. The extracted values are stored in the slot data and
the column.
}
\description{
The NDVI data from AVHRR is gathered from the FTP server
and NDVI values are extracted for (a list of) point
coordinates.
}

