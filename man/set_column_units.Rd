% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_column_units.R
\name{set_column_units}
\alias{set_column_units}
\title{Add [Hmisc::unit()] to multiple columns}
\usage{
set_column_units(x, ...)
}
\arguments{
\item{x}{The data frame that we want to define units on}

\item{...}{Variable names with their intended unit, e.g. `hp = "Hp"`.}
}
\value{
The original data.frame
}
\description{
Add label attribute using `dplyr` syntax using the [Hmisc::unit()]
}
\examples{
library(magrittr)
data(mtcars)
mtcars_with_units <- mtcars \%>\%
  set_column_units(wt = "1000 lbs")
Hmisc::units(mtcars_with_units$wt)
}
\seealso{
Other Hmisc helpers: 
\code{\link{set_column_labels}()}
}
\concept{Hmisc helpers}
