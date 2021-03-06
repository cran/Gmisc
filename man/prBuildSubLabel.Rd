% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxGrobs_boxPropGrob.R
\name{prBuildSubLabel}
\alias{prBuildSubLabel}
\title{Add a sub-label to \code{boxPropGrob}}
\usage{
prBuildSubLabel(label, prop, txt_gp, side = c("left", "right"))
}
\arguments{
\item{label}{The text of the label}

\item{prop}{The proportion}

\item{txt_gp}{The style as defined by \code{\link[grid]{gpar}()}}

\item{side}{The side that the label belongs to}
}
\value{
A \code{textGrob} with he additional attributes \code{width} and \code{height}.
}
\description{
Add a sub-label to \code{boxPropGrob}
}
