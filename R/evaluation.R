# Load the meta data of the climate network
metadata.logger <- read_csv('../data/Metadata_19-22.csv')

# This function evaluates and visualize the model
evaluation_function <- function(data_evaluate = combined_test, train_data = combined_train, model,
                                advanced_model = FALSE){

#------------------------------------------------------------------------------
# Extract the model name. We need it to label the map
model_name <- deparse(substitute(model))

model_input <- str_replace_all(model_name, "_", " ")

###############################################################################
# Data preparation

if('Log_Nr' %in% colnames(data_evaluate)){
unique_numbers <- sort(unique(data_evaluate$Log_Nr))
print(paste('Your model contains:',length(unique_numbers),'lCDs'))
logger.names <- metadata.logger|>
  filter(Log_Nr %in% unique_numbers)|>
  select(Name)|>
  pull()}

#------------------------------------------------------------------------------
# Check whether a KNN model is used. If so, then reduce

# Reduce train data set
if(grepl("knn", model_input)){
  numbers.of.row.train <- nrow(train_data)
  if(numbers.of.row.train > 100000){
  print('The function recognizes a KNN model. High computing time is assumed, and as a result, the train dataset is reduced.')
  split.percent <- as.numeric(round(100000/numbers.of.row.train, digits = 3))
  percent <- as.numeric(round(100*split.percent, digits = 0))
  set.seed(123)  # for reproducibility
  split <- rsample::initial_split(train_data, prop = split.percent)
  train_data <- rsample::training(split)
  new.numbers.of.rows.train <- nrow(train_data)
  print(paste('Your train data set has been reduced from',numbers.of.row.train,
              'rows to',new.numbers.of.rows.train,
              'rows, you are now working with',
              percent,'% of your original train data'))
  }
}

# Reduce test data set
if(grepl("knn", model_input)){
  numbers.of.row.test <- nrow(data_evaluate)
  if(numbers.of.row.test > 88000){
    print('The function recognizes a KNN model. High computing time is assumed, and as a result, the test dataset is reduced.')
    split.percent <- as.numeric(round(5000/numbers.of.row.test, digits = 3))
    percent <- as.numeric(round(100*split.percent, digits = 0))
    set.seed(123)  # for reproducibility
    split <- rsample::initial_split(data_evaluate, prop = split.percent)
    data_evaluate <- rsample::training(split)
    new.numbers.of.rows.test <- nrow(data_evaluate)
    print(paste('Your test data set has been reduced from',numbers.of.row.test,
              'rows to',new.numbers.of.rows.test,
              'rows, you are now predicting with',
              percent,'% of your original test data'))
    }
}

#------------------------------------------------------------------------------
# Metrics Train (RMSE, MAE, RSQ, Bias)

train_data <- train_data |>
  drop_na()
train_data$fitted <- unlist(predict(model, train_data))

metrics_train <- train_data |>
  yardstick::metrics(temperature, fitted)

# RMSE
rmse_train <- round(metrics_train |>
  filter(.metric == "rmse") |>
  pull(.estimate), digits = 3)
# MAE
mae_train <- round(metrics_train |>
  filter(.metric == "mae") |>
  pull(.estimate), digits = 3)
#RSQ
rsq_train <- round(metrics_train |>
  filter(.metric == "rsq") |>
  pull(.estimate), digits = 3)

if(advanced_model == F){
# RMSE
rmse_model <- round(model$results$RMSE, digits = 3)
# MAE
mae_model <- round(model$results$MAE, digits = 3)
# RSQ
rsq_model <- round(model$results$Rsquared, digits = 3)
# Bias
train_data <- train_data|>
  mutate(Bias = fitted - temperature)
bias.train <- round(mean(train_data$Bias), digits = 3)
}else{
  rmse_model <- model$fit$fit$fit$best_msg
  mae_model <- NA
  rsq_model <- NA
  bias.train <- NA
}


# -----------------------------------------------------------------------------
# Metrics Test (RMSE, MAE, RSQ, Bias)
data_evaluate$fitted <- unlist(predict(model, data_evaluate))

metrics_test <- data_evaluate |>
  yardstick::metrics(temperature, fitted)

# RMSE
rmse_test <- round(metrics_test |>
  filter(.metric == "rmse") |>
  pull(.estimate), digits = 3)
# MAE
mae_test <- round(metrics_test |>
  filter(.metric == "mae") |>
  pull(.estimate), digits = 3)
#RSQ
rsq_test <- round(metrics_test |>
  filter(.metric == "rsq") |>
  pull(.estimate), digits = 3)

# Bias
# --> if the bias is negative, then the model underestimate the temperature
# --> if the bias is positive, then the model overestimate the temperature
data_evaluate <- data_evaluate|>
  mutate(Bias = fitted - temperature)
bias.test <- round(mean(data_evaluate$Bias), digits = 3)

###############################################################################
# We want a list as a return for our evaluations:

#------------------------------------------------------------------------------
# Position 1: Table which gives a overview about the metrics: RSQ, RMSE, MAE, Bias

tabl <- tibble::tibble('Metric' = c('RSQ', ' RMSE', 'MAE', 'Bias'),
                       'Values of the model' =c(rsq_model, rmse_model, mae_model, NA),
                       'Values of training set' = c(rsq_train, rmse_train, mae_train, bias.train),
                       'Values of the test set' = c(rsq_test, rmse_test, mae_test, bias.test))|>
  kableExtra::kbl(align = 'lccc', caption = paste('Overview about the metrics of the',model_input))|>
  kableExtra::kable_classic_2(full_width = T, html_font = "Cambria")

#------------------------------------------------------------------------------
# Position 2: Overview about the main statistics

main_stats <- data_evaluate|>
  mutate(Anomaly = fitted - temperature)|>
  select(Anomaly)

main_stats <- tibble(main_stats |>
  summarise(Bias = round(mean(Anomaly), digits = 3),
            Median = round(median(Anomaly), digits = 3),
            Std = round(sd(Anomaly), digits = 3),
            Max = round(max(Anomaly), digits = 3),
            Min = round(min(Anomaly), digits = 3),
            Spread = round((max(Anomaly)- min(Anomaly)), digits = 3),
            IQR = round(IQR(Anomaly), digits = 3),
            Q_25 = round(quantile(Anomaly, probs = .25,na.rm = TRUE), digits = 3),
            Q_75 = round(quantile(Anomaly, probs = .75,na.rm = TRUE), digits = 3),
            skewness = round(moments::skewness(Anomaly, na.rm = TRUE), digits = 3))) |>
  kableExtra::kbl(align = 'lcccccccc', caption = paste('Overview about the main statistic parameters of the',model_input))|>
  kableExtra::kable_classic_2(full_width = T, html_font = "Cambria")


#------------------------------------------------------------------------------
# Position 3: ggplot as a overview about the training set and test set

# We create the plot for the training set
p1 <- ggplot(data = train_data, aes(temperature, fitted)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linewidth = 0.5, color = "orange", linewidth = 0.5) +
  labs(subtitle = bquote(italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                          RMSE == .(format(rmse_train, digits = 3)) ~~
                          Bias == .(format(bias.train, digits = 3))),
                         x = 'Measured temperature [°C]' , y = "Predicted temperature [°C]",
       title = paste("Train set evaluation [",model_input,"]")) +
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))


std_dev <- sd(data_evaluate$fitted)
# We create the plot for the test set
p2 <- ggplot(data = data_evaluate, aes(temperature, fitted)) +
 geom_point(alpha = 0.3) +
 geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
 geom_abline(slope = 1, intercept = 0, color = 'orange', linewidth = 0.5) +
 labs(subtitle = bquote(italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                           RMSE == .(format(rmse_test, digits = 3)) ~~
                           Bias == .(format(bias.test, digits = 3))),
      x = 'Measured temperature [°C]' , y = "Predicted temperature [°C]",
      title = paste("Test set evaluation [",model_input,"]")) +
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))

# We put both plots together
out <- cowplot::plot_grid(p1, p2)

#------------------------------------------------------------------------------
# Position 4: Boxplot for each logger station (where is the bias highest)

boxplot_logger <- ggplot(data = data_evaluate,
             aes(x = as.factor(Log_Nr), y = Bias))+
  geom_boxplot(fill = "skyblue", alpha = 0.5, linewidth = 0.3, width = 0.5,
               outlier.color = "red", outlier.shape = 20, outlier.size = 0.7) +
  stat_boxplot(geom = "errorbar", linewidth = 0.3, width = 0.5)+
  geom_hline(yintercept = 0,linewidth = 0.3) +
  labs(x = "Name of the station and logger number", y = 'Bias [°C]',
       title = paste("Boxplot by logger sides [",model_input,"]")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  scale_x_discrete(labels = paste(logger.names,'[Log Nr:',unique_numbers,']')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))


#------------------------------------------------------------------------------
# Position 5: Boxplot for each hour of the day (bias)

boxplot_hour <- ggplot(data = data_evaluate,
       aes(x = as.factor(hour), y = Bias))+
  geom_boxplot(fill = "skyblue", alpha = 0.5, linewidth = 0.3, width = 0.5,
               outlier.color = "red", outlier.shape = 20, outlier.size = 0.7) +
  geom_hline(yintercept = 0,linewidth = 0.3) +
  stat_boxplot(geom = "errorbar", linewidth = 0.3, width = 0.5)+
  labs(x = "Hour of the day", y = 'Bias [°C]',
       title = paste("Boxplot by hours of the day [",model_input,"]")) +
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))

#------------------------------------------------------------------------------
# Position 6: distribution of the anomaly

df <- data.frame(random = rnorm(10000000, mean = 0, sd = 1))

density <- data_evaluate |>
  ggplot() +
  geom_density(aes(x = Bias), color = "black") +
  geom_vline(xintercept = mean(data_evaluate$Bias), color = 'orange2', linetype = 4) +
  geom_density(data = df, aes(x = random), color = "red3") +
  annotate("text", x = -5, y = 0.3, label = paste("Bias =",bias.test), color = "orange2", size = 5, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.4, label = "Normal Distribution", color = "red3", size = 5, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.35, label = 'Effective Distribution', color = "black", size = 5, hjust = 0, vjust = 0) +
  labs(title = "Distribution of the Temperature Anomaly (Bias)",
       x = "Temperature Anomaly [K]",
       y = "Density") +
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))

#------------------------------------------------------------------------------
# Position 7: distribution of the anomaly

density_2 <- data_evaluate |>
  ggplot() +
  geom_density(aes(x = fitted), color = "black") +
  geom_density(aes(x = temperature), color = "red3") +
  geom_vline(xintercept = mean(data_evaluate$fitted), color = 'orange2', linetype = 4) +
  geom_vline(xintercept = median(data_evaluate$fitted), color = 'green3', linetype = 4) +
  labs(title = "Distribution of the predicted values",
       x = "Distribution of the predicted temperature anomaly (fitted values) [K]",
       y = "Density") +
  annotate("text", x = -5, y = 0.35, label = paste("Mean =", round(mean(data_evaluate$fitted), digits = 3)), color = "orange2", size = 5, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.3, label = paste('Median =', round(median(data_evaluate$fitted), digits = 3)), color = "green3", size = 5, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.45, label = "Distribution of 'temperature'", color = "red3", size = 5, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.4, label = "Distribution of 'predicted values'", color = "black", size = 5, hjust = 0, vjust = 0) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))

###############################################################################
# We define our list for the return:
output <- list(tabl, main_stats, out, boxplot_logger, boxplot_hour, density, density_2)


return(output)}

