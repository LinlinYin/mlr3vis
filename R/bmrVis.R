#' @title Create box, violin or dot plots for a BenchmarkResult.
#'
#' @description
#' Plots box, violin or dot plots for all `measure` across all iterations
#' of the resampling strategy in all `learner_id`, faceted by the `task.id` and `measure`.
#'
#'
#' @param bmr the BenchmarkResult object
#' @param style Type of plot, can be \dQuote{box} for a boxplot, \dQuote{violin} for a violin plot,
#'   or \dQuote{dot} for a dot plot. Default is \dQuote{box}.
#' @param xVar variable for X axis, can be learner_id, task_id, measure
#' @param facet_x variable for facetting in columns, can be learner_id, task_id, measure, or NULL
#' @param facet_y variable for facetting in rows, can be learner_id, task_id, measure, or NULL
#' @export
#' @examples
#' 1
plotBMRBoxplots = function(bmr, style = "box", xVar = "learner_id", facet_x = "task_id", facet_y = "measure") {

  checkmate::assertChoice(xVar, c("learner_id", "task_id", "measure"))
  if (!is.null(facet_x)) {
    checkmate::assertChoice(facet_x, c("learner_id", "task_id", "measure"))
  }
  if (!is.null(facet_y)) {
    checkmate::assertChoice(facet_y, c("learner_id", "task_id", "measure"))
  }
  checkmate::assertChoice(style, c("box", "violin", "dot"))

  measure = bmr$measures$measure_id
  bmrAgg = bmr$aggregated(objects = FALSE)
  setkey(bmrAgg, "hash")
  bmrAllUniqueHash = unique(bmrAgg$hash)

  dataForPlot = NULL
  for (i in 1:length(bmrAllUniqueHash)) {
    rr = bmrAgg[bmrAllUniqueHash[i], resample_result][[1]]
    #    rr = bmr$resample_result(bmrAllUniqueHash[i])
    dataForPlotOne = cbind(as.data.table(rr)[, measure, with = FALSE, drop = FALSE], bmrAgg[i, c("task_id", "learner_id"), with = FALSE])
    #    print(bmrAgg)
    #    print(bmrAllUniqueHash[i])
    #    print(bmrAgg[bmrAllUniqueHash[i],])
    #    print(bmrAgg[i,])
    #    print(dataForPlotOne)
    dataForPlot = rbind(dataForPlot, dataForPlotOne)
  }

  dataForPlot = melt(dataForPlot, id.vars = setdiff(colnames(dataForPlot), measure))
  colnames(dataForPlot)[3] = "measure"
  p = ggplot(data = dataForPlot, aes_string(x = xVar, y = "value")) + facet_grid((paste0(facet_y, "~", facet_x)), scale = "free") + ylab("Measure")
  if (style == "box") {
    p = p + geom_boxplot()
  } else if (style == "violin") {
    p = p + geom_violin()
  } else {
    p = p + geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", size = 1)
    #    p = p + geom_jitter()
  }
  if (xVar == "learner_id") {
    p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  return(plotWithTheme(p))
}


#' @title Create a bar chart for ranks in a BenchmarkResult.
#'
#' @description
#' Plots a bar chart from the ranks of algorithms. Alternatively, tiles can be plotted for every
#' rank-task combination, see pos for details. In all plot variants the ranks of the learning
#' algorithms are displayed on the x-axis. Areas are always colored according to the learner.id.
#'
#'
#' @param bmr the BenchmarkResult object
#' @param pos Optionally set how the bars are positioned in ggplot2. Ranks are plotted on the x-axis.
#'  “tile” plots a heat map with task as the y-axis. Allows identification of the performance in a
#'  special task. “stack” plots a stacked bar plot. Allows for comparison of learners within and and
#'  across ranks. “dodge” plots a bar plot with bars next to each other instead of stacked bars.
#' @export
#' @examples
#' 1
plotBMRRanksAsBarChart = function(bmr, pos = "tile") {
  checkmate::assertChoice(pos, c("tile", "stack", "dodge"))

  bmrAgg = bmr$aggregated(objects = FALSE)
  dataForPlot = bmrAgg %>%
    group_by(task_id) %>%
    mutate(rank = min_rank(desc(classif.acc)))
  p = ggplot(dataForPlot, aes(x = rank, y = task_id, fill = learner_id))
  if (pos == "tile") {
    p = ggplot(dataForPlot, aes(x = rank, y = task_id, fill = learner_id))
    p = p + geom_tile()
  } else if (pos == "dodge") {
    p = ggplot(dataForPlot, aes_string("rank", fill = "learner_id"))
    p = p + geom_bar(position = pos)
  }
  return(plotWithTheme(p))
}
