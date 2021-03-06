% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transitionPlot.R
\name{transitionPlot}
\alias{transitionPlot}
\title{A transition plot}
\usage{
transitionPlot(
  transition_flow,
  type_of_arrow = c("grid", "simple", "gradient"),
  box_txt = rownames(transition_flow),
  tot_spacing = 0.2,
  box_width = 1/4,
  fill_start_box = "darkgreen",
  txt_start_clr = "white",
  fill_end_box = fill_start_box,
  txt_end_clr = txt_start_clr,
  cex = 2,
  min_lwd = if (type_of_arrow == "grid") 1 else unit(0.1, "mm"),
  max_lwd = if (type_of_arrow == "grid") 6 else unit(5, "mm"),
  lwd_prop_total = TRUE,
  arrow_clr = "#000000",
  abs_arrow_width = FALSE,
  overlap_bg_clr = "#FFFFFF",
  overlap_order = 1:nrow(transition_flow),
  overlap_add_width = if (type_of_arrow == "grid") 1.5 else unit(1, "mm"),
  box_prop,
  mar = unit(rep(3, times = 4), "mm"),
  main = NULL,
  box_label = NULL,
  box_label_pos = "top",
  box_label_cex = cex,
  color_bar = TRUE,
  color_bar_cex = cex * 0.33,
  color_bar_labels,
  color_bar_subspace = NULL,
  new_page = FALSE
)
}
\arguments{
\item{transition_flow}{This should be a matrix with the size of the transitions.
The unit for each cell should be number of observations, row/column-proportions
will show incorrect sizes. The matrix needs to be square. The best way to generate
this matrix is probably just do a \code{table(starting_state, end_state)}. The rows
represent the starting positions, while the columns the end positions. I.e. the first
rows third column is the number of observations that go from the first class to the
third class.}

\item{type_of_arrow}{The types of arrow may be grid, simple, or gradient. Simple grid
arrows are the \code{\link[grid:grid.bezier]{bezierGrob}} arrows (not that pretty),
simple is the \code{\link{bezierArrowSmpl}} that I've created to get a more exact
control of the arrow position and width, while gradient
corresponds to \code{\link{bezierArrowGradient}}
allowing the arrow to have a fill color that slowly turns into the color of the arrow.}

\item{box_txt}{The text to appear inside of the boxes. If you need line breaks
then you need to manually add a \\n inside the string.}

\item{tot_spacing}{The proportion of the vertical space that is to be left
empty. It is then split evenly between the boxes.}

\item{box_width}{The width of the box. By default the box is one fourth of
the plot width.}

\item{fill_start_box}{The fill color of the start boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{txt_start_clr}{The text color of the start boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{fill_end_box}{The fill color of the end boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{txt_end_clr}{The text color of the end boxes. This can either
be a single value or a vector if you desire different colors for each
box. If you specify box_prop then this has to be a 2 column matrix.}

\item{cex}{The cex \code{\link{gpar}} of the text}

\item{min_lwd}{The minimum width of the line that we want to illustrate the
tranisition with.}

\item{max_lwd}{The maximum width of the line that we want to illustrate the
tranisition with.}

\item{lwd_prop_total}{The width of the lines may be proportional to either the
other flows from that box, or they may be related to all flows. This is a boolean
parameter that is set to true by default, i.e. relating to all flows.}

\item{arrow_clr}{The color of the arrows. Usually black, can be a vector indicating each arrow
from first to last arrow (counting from the top). If the vector is of the same length as the
boxes then all box arrows will have the same color (that is all the arrows stemming from the
left boxes)}

\item{abs_arrow_width}{The width can either be absolute, i.e. each arrow headed for a box
has the exact same width. The alternative is that the width is related to the line width.}

\item{overlap_bg_clr}{In order to enhance the 3D perspective and to make it easier
to follow arrows the arrows have a background color to separate them from those underneath.}

\item{overlap_order}{The order from first->last for the lines. This means that the last
line will be on top while the first one will appear at the bottom. This should be provided
as a vector.}

\item{overlap_add_width}{The width of the white cross-over line. You can specify this as a scalar
multiplication of the current line width. In case of non-grid arrows then you can also have this
as a unit which is recommended as it looks better. If the scalar is < 1 then the overlap is ignored.}

\item{box_prop}{If you want the boxes to have proportions indicating some other factors then input
a matrix with quantiles for the proportions. Note the size must be \code{nrow(transition_flow) x 2}.}

\item{mar}{A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}}

\item{main}{The title of the plot if any, default \code{NULL}}

\item{box_label}{A vector of length 2 if you want to label each box column}

\item{box_label_pos}{The position of the label, either \code{'top'} or \code{'bottom'}}

\item{box_label_cex}{The cex of the label, defaults to the default cex}

\item{color_bar}{If you have proportions inside the transition_flow variable
then the color_bar will automatically appear at the bottom unless you set
this to \code{FALSE}}

\item{color_bar_cex}{The size of the tick labels for the color bar}

\item{color_bar_labels}{The labels of the two proportions that make up the color bar.
Defaults to the labels of the third dimension for the \code{transition_flow}
argument.}

\item{color_bar_subspace}{If there is little or no difference
at the low/high proportions of the spectrum then it
can be of interest to focus the color change to the center
leaving the tails constant}

\item{new_page}{If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
default it is \code{FALSE}.}
}
\value{
void
}
\description{
This plot's purpose is to illustrate how states change before and
after. In my research I use it before surgery and after surgery
but it can be used in any situation where you have a change from
one state to another
}
\examples{
# This example does not run since it
# takes a little while to assemble the
# arrows and RMD Check complains that this
# is more than allowed for
library(grid)
par_org <- par(ask = TRUE)
# Settings
no_boxes <- 3
# Generate test setting
transition_matrix <- matrix(NA, nrow = no_boxes, ncol = no_boxes)
transition_matrix[1, ] <- 200 * c(.5, .25, .25)
transition_matrix[2, ] <- 540 * c(.75, .10, .15)
transition_matrix[3, ] <- 340 * c(0, .2, .80)

grid.newpage()
transitionPlot(transition_matrix,
  box_txt = c("First", "Second", "Third"),
  type_of_arrow = "simple",
  min_lwd = unit(1, "mm"),
  max_lwd = unit(6, "mm"),
  overlap_add_width = unit(1, "mm")
)


# Setup proportions
box_prop <- cbind(c(1, 0, 0.5), c(.52, .2, .8))
# From the Set2 Colorbrewer
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(
  colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
  colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2]
)
# Create a new grid
grid.newpage()
transitionPlot(transition_matrix,
  box_prop = box_prop,
  fill_start_box = start_box_clr, fill_end_box = end_box_clr,
  txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
  box_txt = c("First", "Second", "Third"),
  type_of_arrow = "gradient",
  min_lwd = unit(1, "mm"),
  max_lwd = unit(10, "mm"),
  overlap_add_width = unit(1, "mm")
)
par(par_org)
}
