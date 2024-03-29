% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSvdMostInfluential.R
\name{getSvdMostInfluential}
\alias{getSvdMostInfluential}
\title{Gets the maximum contributor variables from svd()}
\usage{
getSvdMostInfluential(
  mtrx,
  quantile,
  similarity_threshold,
  plot_selection = TRUE,
  plot_threshold = 0.05,
  varnames = NULL
)
}
\arguments{
\item{mtrx}{A matrix or data frame with the variables. Note: if it contains missing
variables make sure to impute prior to this function as the \code{\link[base]{svd}} can't
handle missing values.}

\item{quantile}{The SVD D-matrix gives an estimate for the amount that is explained.
This parameter is used for selecting the columns that have that quantile
of explanation.}

\item{similarity_threshold}{A quantile for how close other variables have to be in value to
maximum contributor of that particular column. If you only want the maximum value
then set this value to 1.}

\item{plot_selection}{As this is all about variable exploring it is often interesting
to see how the variables were distributed among the vectors}

\item{plot_threshold}{The threshold of the plotted bars, measured as
percent explained by the D-matrix. By default it is set to 0.05.}

\item{varnames}{A vector with alternative names to the colnames}
}
\value{
Returns a list with vector with the column numbers
 that were picked in the "most_influential" variable and the
 svd caluclation in the "svd"
}
\description{
This function is inspired by Jeff Leeks Data Analysis course where
he suggests that one way to use the \code{\link[base]{svd}} is to look
at the most influential rows for first columns in the V matrix.
}
\details{
This function expands on that idea and adds the option of choosing
more than just the most contributing variable for each row. For instance
two variables may have a major impact on a certain component where the second
variable has 95% of the maximum variables value, since this variable is probably
important in that particular component it makes sense to include it
in the selection.

It is of course useful when you have many continuous variables and you want
to determine a subgroup to look at, i.e. finding the needle in the haystack.
}
\examples{
org_par <- par(ask = TRUE)
set.seed(1345)
# Simulate data with a pattern
dataMatrix <- matrix(rnorm(15 * 160), ncol = 15)
colnames(dataMatrix) <- c(
    paste("Pos.3:", 1:3, sep = " #"),
    paste("Neg.Decr:", 4:6, sep = " #"),
    paste("No pattern:", 7:8, sep = " #"),
    paste("Pos.Incr:", 9:11, sep = " #"),
    paste("No pattern:", 12:15, sep = " #"))

for (i in 1:nrow(dataMatrix)) {
  # flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip3 <- rbinom(1, size = 1, prob = 0.5)

  # if coin is heads add a common pattern to that row
  if (coinFlip1) {
    cols <- grep("Pos.3", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] + 3
  }

  if (coinFlip2) {
    cols <- grep("Neg.Decr", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] - seq(from = 5, to = 15, length.out = length(cols))
  }

  if (coinFlip3) {
    cols <- grep("Pos.Incr", colnames(dataMatrix))
    dataMatrix[i, cols] <- dataMatrix[i, cols] + seq(from = 3, to = 15, length.out = length(cols))
  }
}

# Illustrate data
heatmap(dataMatrix, Colv = NA, Rowv = NA, margins = c(7, 2), labRow = "")

svd_out <- svd(scale(dataMatrix))

library(lattice)
b_clr <- c("steelblue", "darkred")
key <- simpleKey(
  rectangles = TRUE, space = "top", points = FALSE,
  text = c("Positive", "Negative")
)
key$rectangles$col <- b_clr

b1 <- barchart(as.table(svd_out$v[, 1]),
               main = "First column",
               horizontal = FALSE, col = ifelse(svd_out$v[, 1] > 0,
                                                b_clr[1], b_clr[2]
               ),
               ylab = "Impact value",
               scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
               key = key
)

b2 <- barchart(as.table(svd_out$v[, 2]),
               main = "Second column",
               horizontal = FALSE, col = ifelse(svd_out$v[, 2] > 0,
                                                b_clr[1], b_clr[2]
               ),
               ylab = "Impact value",
               scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
               key = key
)

b3 <- barchart(as.table(svd_out$v[, 3]),
               main = "Third column",
               horizontal = FALSE, col = ifelse(svd_out$v[, 3] > 0,
                                                b_clr[1], b_clr[2]
               ),
               ylab = "Impact value",
               scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
               key = key
)

b4 <- barchart(as.table(svd_out$v[, 4]),
               main = "Fourth column",
               horizontal = FALSE, col = ifelse(svd_out$v[, 4] > 0,
                                                b_clr[1], b_clr[2]
               ),
               ylab = "Impact value",
               scales = list(x = list(rot = 55, labels = colnames(dataMatrix), cex = 1.1)),
               key = key
)

# Note that the fourth has the no pattern columns as the
# chosen pattern, probably partly because of the previous
# patterns already had been identified
print(b1, position = c(0, 0.5, .5, 1), more = TRUE)
print(b2, position = c(0.5, 0.5, 1, 1), more = TRUE)
print(b3, position = c(0, 0, .5, .5), more = TRUE)
print(b4, position = c(0.5, 0, 1, .5))

# Let's look at how well the SVD identifies
# the most influential columns
getSvdMostInfluential(dataMatrix,
                      quantile = .8,
                      similarity_threshold = .9,
                      plot_threshold = .05,
                      plot_selection = TRUE)
par(org_par)
}
