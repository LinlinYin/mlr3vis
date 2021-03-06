% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLearnerPrediction.R
\name{plotLearnerPrediction}
\alias{plotLearnerPrediction}
\title{Show model prediction results diustribution on 2 selected features.}
\usage{
plotLearnerPrediction(learner = NULL, task = NULL,
  interestedFeatures = NULL, otherFeaturesValue = NULL,
  gridsize = 100L, prob.alpha = TRUE, pointsize = 2,
  err.size = pointsize, err.col = "white")
}
\description{
This function need learner and task as input, and will use the orginal model in learner. It is similar as the function
with same name in mlr but it used orginal model in learner (usually used all features) rather than make
a new model based on only two selected features. The interestedFeatures were only used for visulization
purpose as X axis and Y axis. To show the distribution of prediction result or probability, grid with
different colours were plotted as background. It used orginal model in learner, and interestedFeatures
among X and Y axis as data. The median values or most common values for Features used in model but not
in interestedFeatures were used to predict the grid background.
}
\examples{
1
}
