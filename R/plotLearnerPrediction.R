#' @title Show model prediction results diustribution on 2 selected features.
#'
#' @description
#' This function need learner and tak as input, and will swhow use the model. It is similar as the function
#' with same name in mlr but it used orginal model in learner (usually used all features) rather than make
#' a new model based on only two selected features. The interestedFeatures were only used for visulization
#' purpose as X axis and Y axis. To show the distribution of prediction result or probability, grid with
#' different colours were plotted as background. It used orginal model in learner, and interestedFeatures
#' among X and Y axis as data. The median values or most common values for Features used in model but not
#' in interestedFeatures were used to predict the grid background.
#'
#'
#' @param learner
#' @param task
#' @param interestedFeatures
#' @export
#' @examples
#' 1
plotLearnerPrediction = function(learner = NULL, task = NULL,  interestedFeatures = NULL, gridsize = 100L,
                                 prob.alpha = TRUE,
                                 pointsize = 2, err.size = pointsize, err.col = "white") {

  if (!is.null(learner) & !is.null(task)) {
    task = assert_task(task, clone = TRUE)
    learner = assert_learner(learner, task = task, clone = TRUE)
    if (is.null(interestedFeatures)) {
      interestedFeatures = head(task$feature_names, 2)
    }
  } else {
    stop(paste0("learner&task should be defined."))
  }
  #  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

  #  e=makeExperimentByInterestedFeatures(learner,task,interestedFeatures)
  subjectData = makeSubjectData(learner,task, interestedFeatures)

  gridData = makeGridData(task, interestedFeatures, gridsize = gridsize)
  gridData = gridDataPrediction(task, grid = gridData)

  p = plotGridAndSubjectData(subjectData, gridData, interestedFeatures,
                             prob.alpha = prob.alpha,
                             pointsize = pointsize, err.size = err.size, err.col = err.col)
  return(p)
}

commonValue = function(x) {
  # get median or most common value
  if (class(x) == "numeric" | class(x) == "integer") {
    return(median(x, na.rm = T))
  } else if (class(x) == "factor" | class(x) == "character") {
    return(names(tail(sort(table(x)), 1)))
  } else {
    return(NA)
  }
}

makeSubjectData = function(learner,task, interestedFeatures) {

  target = task$target_names
  predictionResult = as.data.table(learner$predict(task))
  predictionResult$.err = predictionResult$response != predictionResult$truth
  colnames(predictionResult)[2] = target

  subjectData = task$data()[, interestedFeatures, with = FALSE]
  subjectData = cbind(subjectData, predictionResult[, -1]) #-1 to remove row_id column
  return(subjectData)

}


makeGridData = function(task, interestedFeatures, gridsize = 100L) {

  taskData = task$data()
  target = task$target_names

  # X1 and X2, and grid
  x1n = interestedFeatures[1L]
  x1 = taskData[, get(x1n)]
  taskdim = length(interestedFeatures)
  if (taskdim == 2L) {
    x2n = interestedFeatures[2L]
    x2 = taskData[, get(x2n)]
  }
  if (taskdim == 1L) {
    grid = data.frame(x = seq(min(x1), max(x1), length.out = gridsize))
  } else if (taskdim == 2L) {
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
  }
  colnames(grid) = interestedFeatures

  # need task target for truth
  grid = cbind(grid, levels(task$truth())[1])
  colnames(grid)[ncol(grid)] = target

  return(grid)
}

# predictions on grid data
gridDataPrediction = function(task, gridData) {
  taskClone = task$clone()
  target = taskClone$target_names
  interestedFeatures = head(colnames(gridData)[-ncol(gridData)], 2)

  notInterestedFeatures = setdiff(taskClone$feature_names, interestedFeatures)
  if (length(notInterestedFeatures) > 0) { # more than two features needed for model in e
    notInterestedFeaturesValue = taskClone$data()[, notInterestedFeatures, with = FALSE]
    notInterestedFeaturesValue = rbind(apply(notInterestedFeaturesValue, 2, commonValue))
    row.names(notInterestedFeaturesValue) = NULL
    gridData = cbind(gridData, notInterestedFeaturesValue)
  }

  gridPredictions=learner$predict_newdata(taskClone,gridData)

  gridData[, target] = gridPredictions$response
  gridData = cbind(gridData, .prob.pred.class = apply(gridPredictions$prob, 1, max))
  return(gridData)
}


plotGridAndSubjectData = function(subjectData, gridData, interestedFeatures,
  target = setdiff(intersect(colnames(subjectData), colnames(gridData)), interestedFeatures), prob.alpha = TRUE,
  pointsize = 2, err.size = pointsize, err.col = "white", err.mark = "train") {

  p = ggplot(gridData, aes_string(x = interestedFeatures[1], y = interestedFeatures[2]))
  # plot grid
  if (prob.alpha) { #
    p = p + geom_raster(data = gridData, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
      show.legend = TRUE) + scale_fill_discrete(drop = FALSE)
    p = p + scale_alpha(limits = range(gridData$.prob.pred.class))
  } else { # Reponse only
    p = p + geom_raster(mapping = aes_string(fill = target))
  }

  ## plot correct points
  p = p + geom_point(data = subset(subjectData, !subjectData$.err),
    mapping = aes_string(x = interestedFeatures[1], y = interestedFeatures[2], shape = target), size = pointsize)

  # plot error mark on error points
  if (err.mark != "none" && any(subjectData$.err)) {
    p = p + geom_point(data = subset(subjectData, subjectData$.err),
      mapping = aes_string(x = interestedFeatures[1], y = interestedFeatures[2], shape = target),
      size = err.size + 1.5, show.legend = FALSE)
    p = p + geom_point(data = subset(subjectData, subjectData$.err),
      mapping = aes_string(x = interestedFeatures[1], y = interestedFeatures[2], shape = target),
      size = err.size + 1, col = err.col, show.legend = FALSE)
  }

  # plot error points
  p = p + geom_point(data = subset(subjectData, subjectData$.err),
    mapping = aes_string(x = interestedFeatures[1], y = interestedFeatures[2], shape = target),  size = err.size, show.legend = FALSE)
  p = p + guides(alpha = FALSE)

  return(p)
}
