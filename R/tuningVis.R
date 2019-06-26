library(mlr3tuning)

task = mlr3::mlr_tasks$get("iris")
learner = mlr3::mlr_learners$get("classif.rpart")
resampling = mlr3::mlr_resamplings$get("holdout")
measures = mlr3::mlr_measures$mget("classif.ce")
task$measures = measures
param_set = paradox::ParamSet$new(params = list(
  paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  paradox::ParamInt$new("minsplit", lower = 1, upper = 10)))

pe = PerformanceEvaluator$new(
  task = task,
  learner = learner,
  resampling = resampling,
  param_set = param_set
)

pe$eval(data.table::data.table(cp = 0.05, minsplit = 5))
