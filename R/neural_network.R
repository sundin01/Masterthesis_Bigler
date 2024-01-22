neural_network <- function(combined_train){

devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")
#Python version 3.11 needed (not 3.12!)!!!!!!

tensorflow::tf_config()
library(keras)
library(dplyr)

# Split the dataset into predictors and target variable
predictors_nn <- combined_train |>
  dplyr::select(all_of(predictors))
temperature <- combined_train |>
  dplyr::select(temperature)

# Normalize/Scale the predictors using dplyr
scaled_predictors <- predictors_nn|>
  mutate(across(everything(), scale))

# Combining the scaled predictors with the temperature into a single data frame
processed_data <- cbind(scaled_predictors, temperature)

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(combined_train), 0.8 * nrow(combined_train))  # 80% for training
train_data <- processed_data[train_indices, ]
test_data <- processed_data[-train_indices, ]

# Separating predictors and target variable in training and testing sets
train_features <- train_data|> select(-temperature)
train_labels <- train_data|> pull(temperature)
test_features <- test_data|> select(-temperature)
test_labels <- test_data|> pull(temperature)

# Create a Keras sequential model (same as previous example)
model <- keras_model_sequential()
model|>
  layer_dense(units = 128, activation = 'relu')|>
  layer_dropout(rate = 0.2)|>
  layer_dense(units = 64, activation = 'relu')|>
  layer_dropout(rate = 0.2)|>
  layer_dense(units = 32, activation = 'relu')|>
  layer_dropout(rate = 0.2)|>
  layer_dense(units = 1)


# Compile the model
model|> compile(
  loss = 'mean_squared_error',
  optimizer = keras$optimizers$legacy$Adam(learning_rate = 0.001),
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model|> fit(
  as.matrix(train_features), train_labels,
  epochs = 30,
  batch_size = 64,
  validation_data = list(as.matrix(test_features), test_labels)
)



# Plot training history
plot(history)



return(model)
}
