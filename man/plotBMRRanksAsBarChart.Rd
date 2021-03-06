% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmrVis.R
\name{plotBMRRanksAsBarChart}
\alias{plotBMRRanksAsBarChart}
\title{Create a bar chart for ranks in a BenchmarkResult.}
\usage{
plotBMRRanksAsBarChart(bmr, pos = "tile")
}
\arguments{
\item{bmr}{the BenchmarkResult object}

\item{pos}{Optionally set how the bars are positioned in ggplot2. Ranks are plotted on the x-axis.
“tile” plots a heat map with task as the y-axis. Allows identification of the performance in a
special task. “stack” plots a stacked bar plot. Allows for comparison of learners within and and
across ranks. “dodge” plots a bar plot with bars next to each other instead of stacked bars.}
}
\description{
Plots a bar chart from the ranks of algorithms. Alternatively, tiles can be plotted for every
rank-task combination, see pos for details. In all plot variants the ranks of the learning
algorithms are displayed on the x-axis. Areas are always colored according to the learner.id.
}
\examples{
1
}
