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
gridDataPrediction = function(e, gridData) {

  eClone = e$clone()
  taskClone = e$task$clone()
  interestedFeatures = head(colnames(gridData)[-ncol(gridData)], 2)

  notInterestedFeatures = setdiff(eClone$task$feature_names, interestedFeatures)
  if (length(notInterestedFeatures) > 0) { # more than two features needed for model in e
    notInterestedFeaturesValue = eClone$task$data()[, notInterestedFeatures, with = FALSE]
    notInterestedFeaturesValue = rbind(apply(notInterestedFeaturesValue, 2, commonValue))
    row.names(notInterestedFeaturesValue) = NULL
    gridData = cbind(gridData, notInterestedFeaturesValue)
  }
  gridPredictions = eClone$predict(newdata = gridData)

  target = taskClone$target_names
  gridData[, target] = gridPredictions$prediction$response
  gridData = cbind(gridData, .prob.pred.class = apply(gridPredictions$prediction$prob, 1, max))
  return(gridData)
}

makeExperimentByInterestedFeatures = function(learner, task, interestedFeatures) {

  taskClone = task$clone()
  taskClone$select(interestedFeatures)
  # prob
  learnerClone = learner$clone()
  if ("prob" %in% learner$predict_types) {
    learnerClone$predict_type = "prob"
  }

  eClone = Experiment$new(task = taskClone, learner = learnerClone)
  eClone$train()
  eClone$predict()
  return(eClone)
}

makeSubjectDataByExperiment = function(e, interestedFeatures) {

  eClone = e$clone()

  target = eClone$task$target_names
  predictionResult = as.data.table(eClone$prediction)
  predictionResult$.err = predictionResult$response != predictionResult$truth
  colnames(predictionResult)[2] = target

  subjectData = eClone$task$data()[, interestedFeatures, with = FALSE]
  subjectData = cbind(subjectData, predictionResult[, -1]) #-1 to remove row_id column
  return(subjectData)

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


#' @export
#'
plotLearnerPrediction = function(e = NULL, learner = NULL, task = NULL,  interestedFeatures = NULL, gridsize = 100L,
  prob.alpha = TRUE,
  pointsize = 2, err.size = pointsize, err.col = "white") {

  if (!is.null(e)) {
    eClone = e$clone()
    if (is.null(interestedFeatures)) {
      interestedFeatures = head(eClone$task$feature_names, 2)
    }
  } else if (!is.null(learner) & !is.null(task)) {
    task = assert_task(task, clone = TRUE)
    learner = assert_learner(learner, task = task, clone = TRUE)
    if (is.null(interestedFeatures)) {
      interestedFeatures = head(task$feature_names, 2)
    }
    eClone = makeExperimentByInterestedFeatures(learner, task, interestedFeatures)
  } else {
    stop(paste0("At least learner&task or Experiment should be defined."))
  }
  #  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

  #  e=makeExperimentByInterestedFeatures(learner,task,interestedFeatures)
  subjectData = makeSubjectDataByExperiment(eClone, interestedFeatures)

  gridData = makeGridData(eClone$task, interestedFeatures, gridsize = gridsize)
  gridData = gridDataPrediction(e = eClone, grid = gridData)

  p = plotGridAndSubjectData(subjectData, gridData, interestedFeatures,
    prob.alpha = prob.alpha,
    pointsize = pointsize, err.size = err.size, err.col = err.col)
  return(p)
}
