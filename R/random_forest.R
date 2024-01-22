
random_forest <- function(pp,training_data,tuning = FALSE){
  cores <- makeCluster(detectCores())
  registerDoParallel(cores)

  pred_count <- length(pp$var_info$variable)
  if (tuning == FALSE) {
    print('The model is currently being generated with mtry = 16 and min.node.size = 3. Please be patient...')
    grid <- expand.grid(
      .mtry = pred_count/3.5, #default p/3.5
      .min.node.size = 3,         # set to 3
      .splitrule = "variance"     # default variance
    ) #Result of hyperparameter tuning
  }else{print('The model is now in the tuning process. Please be patient...')
    grid <- expand.grid(
      .mtry = c(pred_count/2,pred_count/2.5,pred_count/3,pred_count/3.5,pred_count/4),
      .min.node.size = c(3,5,8,10),         # set to 5
      .splitrule = "variance"     # default variance
    )
  }
group_folds <- groupKFold(training_data$Log_Nr, k = 3)
mod_cv <- caret::train(
  pp,
  data = training_data,
  method = "ranger",
  metric = "RMSE",
  trControl = trainControl(
    method = "cv",
    index = group_folds,
    number = 3,
    savePredictions = "final"),
    parallel = 'foreach',
  tuneGrid = grid,
  # arguments specific to "ranger" method
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 1982
)

stopCluster(cores)
return(mod_cv)
}
