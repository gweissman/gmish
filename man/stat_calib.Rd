% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-calib.R
\docType{data}
\name{StatCalibration}
\alias{StatCalibration}
\title{The calib geom is used to create a calibration plot. The calibration plot is
useful for describing the relationship between predicted probabilities and
observed outcomes for a binary classifier, though the are also used for
predictions of continuous measures (which will be handled later). Creating
a calibration plot using a native ggplot2 geom is especially useful because it
allows for adjustment of line color across groups of unequal sizes.}
\format{
An object of class \code{StatCalibration} (inherits from \code{Stat}, \code{ggproto}, \code{gg}) of length 4.
}
\usage{
StatCalibration
}
\description{
The calib geom is used to create a calibration plot. The calibration plot is
useful for describing the relationship between predicted probabilities and
observed outcomes for a binary classifier, though the are also used for
predictions of continuous measures (which will be handled later). Creating
a calibration plot using a native ggplot2 geom is especially useful because it
allows for adjustment of line color across groups of unequal sizes.
}
\keyword{datasets}
