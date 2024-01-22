# We define a function to calculate the KNN model
KNN_Model <- function(pp, training_data = combined_train, tuning = FALSE){

cores <- makeCluster(detectCores())
registerDoParallel(cores)
tuning.vector = c(8:12, 31, 32, 33)

numbers.of.rows <- nrow(training_data)

# Reduce train data set
if(numbers.of.rows > 100000){
  print('The data is split, as we assume a high computing time!')
  split.percent <- as.numeric(round(100000/numbers.of.rows, digits = 3))
  percent <- as.numeric(round(100*split.percent, digits = 0))
  set.seed(123)  # for reproducibility
  split <- rsample::initial_split(training_data, prop = split.percent)
  training_data <- rsample::training(split)
  new.numbers.of.rows <- nrow(training_data)
  print(paste('Your data set has been reduced from',numbers.of.rows,
              'rows to',new.numbers.of.rows,
              'rows, you are now working with',
              percent,'% of your original data'))
}

if(tuning == FALSE){
  print('The model is currently being generated with k = 10. Please be patient...')
  group_folds <- groupKFold(training_data$Log_Nr, k = 3)
  model <- caret::train(pp, data = training_data |> tidyr::drop_na(),
                        # We want a KNN model
                        method = "knn",
                        # we use cross validation as method
                        trControl = caret::trainControl(method = "cv",                                                         index = group_folds,
                                                        number = 3,
                                                        savePredictions = "final"),
                        parallel = 'foreach',
                        # we set k = k to optimize the hyperparameter k. We substitute it later with a vector
                        tuneGrid = data.frame(k = 10),
                        # we want the RMSE as our metrics
                        metric = "RMSE")

}

if(tuning == TRUE){
  print('The model is now in the tuning process. Please be patient...')
  group_folds <- groupKFold(training_data$Log_Nr, k = 3)
  model <- caret::train(pp, data = training_data |> tidyr::drop_na(),
                        # We want a KNN model
                        method = "knn",
                        # we use cross validation as method
                        trControl = caret::trainControl(method = "cv",
                                                        index = group_folds,
                                                        number = 3,
                                                        savePredictions = "final"),
                        parallel = 'foreach',
                        # we set k = k to optimize the hyperparameter k. We substitute it later with a vector
                        tuneGrid = data.frame(k = tuning.vector),
                        # we want the RMSE as our metrics
                        metric = "RMSE")

  best.tune <- knn_model$bestTune$k
  print(paste('The model was fine-tuned with values of k = 8, 9, 10, 11, 12. The optimal k is now:',best.tune))

}


    return(model)
}
