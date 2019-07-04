#' @export
#'
summaryFeatureInTask = function(task, printHtml = FALSE) {

  target = task$target_names
  allFeatures = task$feature_names
  dataForPlot = task$data()[, c(allFeatures,target), with = FALSE]

  library(Hmisc) # Will move this library out of function if this function is approved to be included
  formulaForTable <- as.formula(paste0(paste(allFeatures, collapse = " + "), "~", target))
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
plotFeatureInTask = function(task,  style = "pairs") {

  checkmate::assertChoice(style, c("pairs","box", "violin", "dot"))


  target = task$target_names
  allFeatures = task$feature_names
  dataForPlot = task$data()[, c(allFeatures,target), with = FALSE]

  if (style == "pairs") {
    GGally::ggpairs(dataForPlot,aes_string(colour=target))
  } else {
    dataForPlot = melt(dataForPlot, id.vars = c(target))
    p = ggplot(data = dataForPlot, aes_string(x = target, y = "value")) + facet_wrap(~variable, scale = "free") + ylab("Features")
    if (style == "box") {
      p = p + geom_boxplot()
    } else if (style == "violin") {
      p = p + geom_violin()
    } else {
      p = p + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", size = 1)
      #    p = p + geom_jitter()
    }
  }

  return(plotWithTheme(p))

}
