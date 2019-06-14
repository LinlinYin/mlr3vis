#' @export
plotROCCurves = function(performanceByThreshold) {
  if (all(c("classif.fpr", "classif.tpr") %in% colnames(performanceByThreshold))) {
    p = ggplot(data = performanceByThreshold, aes(x = classif.fpr, y = classif.tpr)) + geom_line() + xlab("False Positive Rate") + ylab("True Positive Rate") + xlim(c(0, 1)) + ylim(c(0, 1))
    p = p + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  } else {
    stop(paste0("Can't find classif.fpr and classif.tpr in performanceByThreshold data"))
  }
  return(p)
}
