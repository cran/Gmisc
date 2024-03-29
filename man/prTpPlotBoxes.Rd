% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transitionPlot_helpers.R
\name{prTpPlotBoxes}
\alias{prTpPlotBoxes}
\title{Plot boxes and arrows}
\usage{
prTpPlotBoxes(
  overlap_order,
  transition_flow,
  no_boxes,
  box_width,
  tot_spacing,
  txt,
  cex,
  txt_gpar = NULL,
  prop_start_sizes,
  prop_end_sizes,
  box_prop,
  lwd_prop_total = lwd_prop_total,
  fill_start_clr,
  fill_end_clr,
  txt_start_clr,
  txt_end_clr,
  lwd = 2,
  line_col = "#000000",
  min_lwd,
  max_lwd,
  overlap_add_width,
  overlap_bg_clr,
  type_of_arrow,
  abs_arrow_width,
  arrow_clr,
  transition_arrow_props,
  color_bar_subspace,
  plot_arrows = TRUE,
  proportion = FALSE
)
}
\arguments{
\item{overlap_order}{The order from first->last for the lines. This means that the last
line will be on top while the first one will appear at the bottom. This should be provided
as a vector.}

\item{transition_flow}{This should be a matrix with the size of the transitions.
The unit for each cell should be number of observations, row/column-proportions
will show incorrect sizes. The matrix needs to be square. The best way to generate
this matrix is probably just do a \code{table(starting_state, end_state)}. The rows
represent the starting positions, while the columns the end positions. I.e. the first
rows third column is the number of observations that go from the first class to the
third class.}

\item{no_boxes}{Number of boxes to plot}

\item{box_width}{The width of the box. By default the box is one fourth of
the plot width.}

\item{tot_spacing}{The proportion of the vertical space that is to be left
empty. It is then split evenly between the boxes.}

\item{txt}{The text}

\item{cex}{The cex \code{\link{gpar}} of the text}

\item{txt_gpar}{The [grid::gpar()] object that is overridden with col & cex by other parameters}

\item{prop_start_sizes}{The proportion of the different boxes
to the left}

\item{prop_end_sizes}{The proportion of the different boxes
to the right}

\item{box_prop}{If you want the boxes to have proportions indicating some other factors then input
a matrix with quantiles for the proportions. Note the size must be \code{nrow(transition_flow) x 2}.}

\item{lwd_prop_total}{The width of the lines may be proportional to either the
other flows from that box, or they may be related to all flows. This is a boolean
parameter that is set to true by default, i.e. relating to all flows.}

\item{fill_start_clr}{The color of the boxes to the left}

\item{fill_end_clr}{The color of the boxes to the right}

\item{txt_start_clr}{The text color of the start boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{txt_end_clr}{The text color of the end boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{lwd}{The line width}

\item{line_col}{The color of the line}

\item{min_lwd}{The minimum width of the line that we want to illustrate the
tranisition with.}

\item{max_lwd}{The maximum width of the line that we want to illustrate the
tranisition with.}

\item{overlap_add_width}{The width of the white cross-over line. You can specify this as a scalar
multiplication of the current line width. In case of non-grid arrows then you can also have this
as a unit which is recommended as it looks better. If the scalar is < 1 then the overlap is ignored.}

\item{overlap_bg_clr}{In order to enhance the 3D perspective and to make it easier
to follow arrows the arrows have a background color to separate them from those underneath.}

\item{type_of_arrow}{The types of arrow may be grid, simple, or gradient. Simple grid
arrows are the \code{\link[grid:grid.bezier]{bezierGrob}} arrows (not that pretty),
simple is the \code{\link{bezierArrowSmpl}} that I've created to get a more exact
control of the arrow position and width, while gradient
corresponds to \code{\link{bezierArrowGradient}}
allowing the arrow to have a fill color that slowly turns into the color of the arrow.}

\item{abs_arrow_width}{The absolute width of the arrow}

\item{arrow_clr}{The color of the arrows. Usually black, can be a vector indicating each arrow
from first to last arrow (counting from the top). If the vector is of the same length as the
boxes then all box arrows will have the same color (that is all the arrows stemming from the
left boxes)}

\item{transition_arrow_props}{The proportions of the different transitions if
available.}

\item{color_bar_subspace}{If there is little or no difference
at the low/high proportions of the spectrum then it
can be of interest to focus the color change to the center
leaving the tails constant}

\item{plot_arrows}{If we are plotting shadow boxes then
arrows should not be plotted and this should be set to \code{FALSE}}

\item{proportion}{It there is a proportion}
}
\value{
\code{void}
}
\description{
Outputs all the boxes and arrow
}
\keyword{internal}
