#' @export
plotThreshVsPerf = function(performanceByThreshold) {
  dataForPlot = reshape2::melt(performanceByThreshold, id.vars = "threshold")
  colnames(dataForPlot)[-1] = c("measure", "performance")
  plt = ggplot2::ggplot(dataForPlot, aes_string(x = "threshold", y = "performance"))
  plt = plt + geom_line() + facet_wrap(~measure, scales = "free_y")
  return(plt)
}
