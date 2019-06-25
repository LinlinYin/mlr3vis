#' @export
#'
summaryFeatureByPrediction = function(e, printHtml = FALSE, groupVar = "truth") {

  checkmate::assertChoice(groupVar, c("truth", "response"))

  #  target = e$task$target_names
  predictionResult = as.data.table(e$prediction)
  #  predictionResult$.err = predictionResult$response != predictionResult$truth


  allFeatures = e$task$feature_names
  dataForPlot = e$task$data()[, allFeatures, with = FALSE]
  dataForPlot = cbind(dataForPlot, predictionResult)

  library(Hmisc) # Will move this library out of function if this function is approved to be included
  formulaForTable <- as.formula(paste0(paste(allFeatures, collapse = " + "), "~", groupVar))
  s <- summaryM(formulaForTable, data = dataForPlot,
    overall = FALSE, test = TRUE)
  if (printHtml) {
    html(s, exclude1 = FALSE,  what = c("%"), digits = 3, prmsd = TRUE)
  } else {
    return(s)
  }
}

#' @export
#'
plotFeatureByPrediction = function(e, groupVar = "truth", style = "box") {

  checkmate::assertChoice(groupVar, c("truth", "response"))
  checkmate::assertChoice(style, c("box", "violin", "dot"))

  #  target = e$task$target_names
  predictionResult = as.data.table(e$prediction)
  predictionResult$.err = predictionResult$response != predictionResult$truth


  allFeatures = e$task$feature_names
  dataForPlot = e$task$data()[, allFeatures, with = FALSE]
  dataForPlot = cbind(dataForPlot, predictionResult)

  dataForPlot = dataForPlot[, c(allFeatures, groupVar, ".err"), with = FALSE]
  dataForPlot = melt(dataForPlot, id.vars = c(groupVar, ".err"))

  p = ggplot(data = dataForPlot, aes_string(x = groupVar, y = "value")) + facet_wrap(~variable, scale = "free") + ylab("Features")
  if (style == "box") {
    p = p + geom_boxplot()
  } else if (style == "violin") {
    p = p + geom_violin()
  } else {
    p = p + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", size = 1)
    #    p = p + geom_jitter()
  }
  return(plotWithTheme(p))

}
