% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxGrobs.R
\name{connectGrob}
\alias{connectGrob}
\alias{plot.connect_boxes}
\alias{print.connect_boxes}
\title{Connect boxes with an arrow}
\usage{
connectGrob(start, end, type = c("vertical", "horizontal", "L", "-"),
  subelmnt = c("right", "left"), lty_gp = gpar(fill = "black"),
  arrow_obj = arrow(ends = "last", type = "closed"))

\method{print}{connect_boxes}(x, ...)

\method{plot}{connect_boxes}(x, ...)
}
\arguments{
\item{start}{The start box}

\item{end}{The end box}

\item{type}{How the boxes are stacked. The \code{L} alternative generates a
straight line up/down and then turns to righT/left for connecting with the end.
The \code{-} generates a straight horizontal arrow.}

\item{subelmnt}{If we have a split box we can specify the right/left x as the
connector point.}

\item{lty_gp}{The \code{\link[grid]{gpar}} for the line.}

\item{arrow_obj}{The arrow spec according to \code{\link[grid]{arrow}}.}

\item{x}{The grob to print/plot}

\item{...}{Passed to \code{\link[grid]{grid.draw}}}
}
\value{
grob with an arrow
}
\description{
The function creates a grob that links two boxes together. It looks for
which side it should attach the arrow to in order to avoid

The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
}
\examples{
library(grid)
grid.newpage()

# Initiate the boxes that we want to connect
start <- boxGrob("Top", x=.5, y=.8)
end <- boxGrob("Bottom", x=.5, y=.2)
side <- boxPropGrob("Side", "Left", "Right", prop=.3, x=.2, y=.8)
sub_side_left <- boxGrob("Left", x = attr(side, "coords")$left_x, y = .5)
sub_side_right <- boxGrob("Right", x = attr(side, "coords")$right_x, y = .5)
exclude <- boxGrob("Exclude:\\n - Too sick\\n - Prev. surgery", x=.8, y=.5,just="left")

# Connect the boxes and print/plot them
connectGrob(start, end, "vertical")
connectGrob(start, side, "horizontal")
connectGrob(side, sub_side_left, "v", "l")
connectGrob(side, sub_side_right, "v", "r")
connectGrob(start, exclude, "L")

# Print the grobs
start
end
side
exclude
sub_side_left
sub_side_right
}
\seealso{
Other box-functions: \code{\link{boxGrob}},
  \code{\link{boxPropGrob}}
}
