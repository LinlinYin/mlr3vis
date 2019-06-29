makeGridData=function(task,interestedFeatures,gridsize = 100L) {
  taskData=task$data()
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

  #need task target for truth
  grid = cbind(grid, levels(task$truth())[1])
  colnames(grid)[ncol(grid)] = target

  return(grid)
}


# predictions on grid data
gridDataPrediction=function(e,gridData) {
  eClone=e$clone()
  taskClone=e$task$clone()
  interestedFeatures=head(colnames(gridData)[-ncol(gridData)],2)

  notInterestedFeatures=setdiff(eClone$task$feature_names,interestedFeatures)
  if (length(notInterestedFeatures)>0) { #more than two features needed for model in e
    notInterestedFeaturesValue=eClone$task$data()[, notInterestedFeatures, with = FALSE]
    notInterestedFeaturesValue=rbind(apply(notInterestedFeaturesValue,2,commonValue))
    row.names(notInterestedFeaturesValue)=NULL
    gridData=cbind(gridData,notInterestedFeaturesValue)
  }
  gridPredictions = eClone$predict(newdata = gridData)

  target = taskClone$target_names
  gridData[, target] = gridPredictions$prediction$response
  gridData=cbind(gridData,.prob.pred.class=apply(gridPredictions$prediction$prob,1,max))
  return(gridData)
}

makeExperimentByInterestedFeatures=function(learner,task,interestedFeatures) {
  taskClone=task$clone()
  taskClone$select(interestedFeatures)
  # prob
  learnerClone=learner$clone()
  if ("prob" %in% learner$predict_types) {
    learnerClone$predict_type = "prob"
  }

  eClone = Experiment$new(task = taskClone, learner = learnerClone)
  eClone$train()
  eClone$predict()
  return(eClone)
}

makeSubjectDataByExperiment=function(e,interestedFeatures) {
  eClone = e$clone()

  target = eClone$task$target_names
  predictionResult = as.data.table(eClone$prediction)
  predictionResult$.err = predictionResult$response != predictionResult$truth
  colnames(predictionResult)[2] = target

  subjectData = eClone$task$data()[, interestedFeatures, with = FALSE]
  subjectData = cbind(subjectData, predictionResult[,-1]) #-1 to remove row_id column
  return(subjectData)

}

plotGridAndSubjectData=function(subjectData,gridData,interestedFeatures,
                                target=setdiff(intersect(colnames(subjectData),colnames(gridData)),interestedFeatures),prob.alpha=TRUE,
                                pointsize=2,err.size = pointsize, err.col = "white",err.mark="train") {
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


plotLearnerPrediction=function(learner, task, interestedFeatures = head(task$feature_names,2),gridsize = 100L) {
  task = assert_task(task, clone = TRUE)
  learner = assert_learner(learner, task = task, clone = TRUE)
#  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

  e=makeExperimentByInterestedFeatures(learner,task,interestedFeatures)
  subjectData=makeSubjectDataByExperiment(e,interestedFeatures)

  gridData=makeGridData(task,interestedFeatures,gridsize = gridsize)
  gridData=gridDataPrediction(e=e,grid=gridData)

  p=plotGridAndSubjectData(subjectData,gridData,interestedFeatures)
  return(p)
}

plotLearnerPrediction(learner, task)



plotLearnerPredictionByExperiment=function(e, interestedFeatures = head(e$task$feature_names,2),gridsize = 100L) {

  #  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

#  e=makeExperimentByInterestedFeatures(learner,task,interestedFeatures)
  subjectData=makeSubjectDataByExperiment(e,interestedFeatures)

  gridData=makeGridData(task,interestedFeatures,gridsize = gridsize)
  gridData=gridDataPrediction(e=e,grid=gridData)

  p=plotGridAndSubjectData(subjectData,gridData,interestedFeatures)
  return(p)
}









#' @export

plotLearnerPrediction = function(learner, task, features = NULL, cv = 10L, err.mark = "train",
  pointsize = 2, err.size = pointsize, err.col = "white", prob.alpha = TRUE,
  gridsize = 100L) {

  task = assert_task(task, clone = TRUE)
  learner = assert_learner(learner, task = task, clone = TRUE)
  checkmate::assertChoice(err.mark, choices = c("train", "cv", "none"))

  fns = task$feature_names
  if (is.null(features)) {
    features = if (length(fns) == 1L) fns else fns[1:2]
  }
  target = task$target_names

  # subset to features
  task$select(features)
  data = task$data()[, features, with = FALSE]

  #grid
  grid=makeGridData(taskData,interestedFeatures,gridsize = 100L)





  if (err.mark == "train") {
    dataForPlot$response = predictions$response
  } else if (err.mark == "cv") {
    #   #CV
    if (cv > 0L) {
      resampling = mlr_resamplings$get("cv", param_vals = list(folds = cv))
      #    print(resampling)
      cvResult = resample(task, learner, resampling)

      perf.cv = cvResult$aggregated
      responses = unlist(lapply(cvResult$data$prediction, function(x) x$response))
      rowIds = unlist(lapply(cvResult$data$prediction, function(x) x$row_ids))
      pred.cv = responses[order(rowIds)]
      dataForPlot$response = pred.cv
    } else {
      perf.cv = NA_real_
    }
  }
  dataForPlot$.err = dataForPlot$response != dataForPlot[[target]]



  x1n = features[1]
  x2n = features[2]
  #  p=ggplot(dataForPlot,aes_string(x = x1n, y = x2n, col = "response"))+geom_point()

  p = ggplot(grid, aes_string(x = x1n, y = x2n))
  # plot grid
  if (learner$predict_type == "prob" && prob.alpha) { #
    prob = apply(pred.grid$prediction$prob, 1, max)
    grid$.prob.pred.class = prob
    p = p + geom_raster(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
      show.legend = TRUE) + scale_fill_discrete(drop = FALSE)
    p = p + scale_alpha(limits = range(grid$.prob.pred.class))
  } else { # Reponse only
    p = p + geom_raster(mapping = aes_string(fill = target))
  }

  ## plot correct points
  p = p + geom_point(data = subset(dataForPlot, !dataForPlot$.err),
    mapping = aes_string(x = x1n, y = x2n, shape = target), size = pointsize)
  #  p = ggplot(data = subset(dataForPlot, !dataForPlot$.err),
  #             mapping = aes_string(x = x1n, y = x2n, shape = "truth"), size = pointsize)+geom_point()

  # plot error mark on error points
  if (err.mark != "none" && any(dataForPlot$.err)) {
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target),
      size = err.size + 1.5, show.legend = FALSE)
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target),
      size = err.size + 1, col = err.col, show.legend = FALSE)
  }

  # plot error points
  p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
    mapping = aes_string(x = x1n, y = x2n, shape = target),  size = err.size, show.legend = FALSE)
  p = p + guides(alpha = FALSE)
  return(p)



  # p = ggplot(grid, aes_string(x = x1n, y = x2n))
  # if (hasLearnerProperties(learner, "prob") && prob.alpha) {
  #   # max of rows is prob for selected class
  #   prob = apply(getPredictionProbabilities(pred.grid, cl = td$class.levels), 1, max)
  #   grid$.prob.pred.class = prob
  #   p = p + geom_raster(data = grid, mapping = aes_string(fill = target, alpha = ".prob.pred.class"),
  #                       show.legend = TRUE) + scale_fill_discrete(drop = FALSE)
  #   p = p + scale_alpha(limits = range(grid$.prob.pred.class))
  # } else {
  #   p = p + geom_raster(mapping = aes_string(fill = target))
  # }
  # # print normal points
  # p = p + geom_point(data = subset(data, !data$.err),
  #                    mapping = aes_string(x = x1n, y = x2n, shape = target), size = pointsize)
  # # mark incorrect points
  # if (err.mark != "none" && any(data$.err)) {
  #   p = p + geom_point(data = subset(data, data$.err),
  #                      mapping = aes_string(x = x1n, y = x2n, shape = target),
  #                      size = err.size + 1.5, show.legend = FALSE)
  #   p = p + geom_point(data = subset(data, data$.err),
  #                      mapping = aes_string(x = x1n, y = x2n, shape = target),
  #                      size = err.size + 1, col = err.col, show.legend = FALSE)
  # }
  # # print error points
  # p = p + geom_point(data = subset(data, data$.err),
  #                    mapping = aes_string(x = x1n, y = x2n, shape = target), size = err.size, show.legend = FALSE)
  # p = p + guides(alpha = FALSE)




}

#' @export

plotLearnerPredictionExperiment = function(e, features = NULL, err.mark = "train",
  pointsize = 2, err.size = pointsize, err.col = "white", prob.alpha = TRUE,
  gridsize = 100L) {

  eClone = e$clone()

  target = eClone$task$target_names
  predictionResult = as.data.table(eClone$prediction)
  predictionResult$.err = predictionResult$response != predictionResult$truth
  colnames(predictionResult)[2] = target

  fns = eClone$task$feature_names
  if (is.null(features)) {
    features = if (length(fns) == 1L) fns else fns[1:2]
  }
  dataForPlot = eClone$task$data()[, features, with = FALSE]

  # X1 and X2, and grid
  x1n = features[1L]
  x1 = dataForPlot[, get(x1n)]
  taskdim = length(features)
  if (taskdim == 2L) {
    x2n = features[2L]
    x2 = dataForPlot[, get(x2n)]
  }
  if (taskdim == 1L) {
    grid = data.frame(x = seq(min(x1), max(x1), length.out = gridsize))
  } else if (taskdim == 2L) {
    # setup data frames for ggplot. grid = background, data = points
    grid = expand.grid(
      seq(min(x1), max(x1), length.out = gridsize),
      seq(min(x2), max(x2), length.out = gridsize)
    )
  }
  colnames(grid) = features
  #Median of Other features to be added to grid
  commonValue=function(x) {#get median or most common value
    if (class(x)=="numeric" |class(x)=="integer") {
      return(median(x,na.rm=T))
    } else if (class(x)=="factor" |class(x)=="character") {
      return(names(tail(sort(table(x)),1)))
    } else {
      return(NA)
    }
  }
  notInterestedFeatures=setdiff(eClone$task$feature_names,features)
  if (length(notInterestedFeatures)>0) {
    notInterestedFeaturesValue=eClone$task$data()[, notInterestedFeatures, with = FALSE]
    notInterestedFeaturesValue=rbind(apply(notInterestedFeaturesValue,2,commonValue))
    row.names(notInterestedFeaturesValue)=NULL
    grid=cbind(grid,notInterestedFeaturesValue)
  }

  # prob
  if ("prob" %in% eClone$learner$predict_types) {
    eClone$learner$predict_type = "prob"
  }

  # grid for predictions
  grid = cbind(grid, levels(eClone$task$truth())[1])
  colnames(grid)[ncol(grid)] = target
  pred.grid = eClone$predict(newdata = grid)
  grid[, target] = pred.grid$prediction$response





  dataForPlot = cbind(dataForPlot, predictionResult)

  p = ggplot(grid, aes_string(x = x1n, y = x2n))

  ## plot correct points
  p = p + geom_point(data = subset(dataForPlot, !dataForPlot$.err),
    mapping = aes_string(x = x1n, y = x2n, shape = target), size = pointsize)
  #  p = ggplot(data = subset(dataForPlot, !dataForPlot$.err),
  #             mapping = aes_string(x = x1n, y = x2n, shape = "truth"), size = pointsize)+geom_point()

  # plot error mark on error points
  if (err.mark != "none" && any(dataForPlot$.err)) {
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target),
      size = err.size + 1.5, show.legend = FALSE)
    p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target),
      size = err.size + 1, col = err.col, show.legend = FALSE)
  }

  # plot error points
  p = p + geom_point(data = subset(dataForPlot, dataForPlot$.err),
    mapping = aes_string(x = x1n, y = x2n, shape = target),  size = err.size, show.legend = FALSE)
  p = p + guides(alpha = FALSE)
  return(p)

}
