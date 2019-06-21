context("plotBMRBoxplots")

test_that("plotBMRBoxplots range includes all outliers", {
  tasks = mlr_tasks$mget(c("pima", "sonar", "spam"))
  learners = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
  resamplings = mlr_resamplings$mget("cv")
  design = expand_grid(tasks, learners, resamplings)
  bmr = benchmark(design)

  p=ggplot_build(plotBMRBoxplots(bmr))

  figureYRange <- p$layout$panel_params[[1]]$y.range
  dataPerformanceRange=range(unlist(bmr$data$performance))

  expect_true(figureYRange[1] <= dataPerformanceRange[1])
  expect_true(figureYRange[2] >= dataPerformanceRange[2])
})





