#' @title Generate threshold vs. performance(s) for 2-class classification.
#'
#' @description
#' Generates data on threshold vs. performance(s) for 2-class classification that can be used for plotting.
#'
#'
#' @param e Container object for a machine learning experiment.
#' @param gridsize Grid resolution for x-axis (threshold). Default is 100.
#' @export
#' @examples
generateThreshVsPerfData = function(e, gridsize = 100L) {
  eClone = e$clone()

  performanceByThreshold = NULL
  for (i in 1:gridsize) {
    th = i * (1 / gridsize)
    #  th=c(th, 1-th)
    #  th=c(0.1,0.9)
    #  names(th) = task$class_names
    eClone$prediction = eClone$prediction$set_threshold(th)
    performanceByThreshold = rbind(performanceByThreshold, c(threshold = th, eClone$score()$performance))
  }
  performanceByThreshold = data.table::as.data.table(performanceByThreshold)

  return(performanceByThreshold)
}
