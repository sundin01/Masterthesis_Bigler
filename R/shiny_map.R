# Install required packages if not already installed
# install.packages("shiny")

library(shiny)

# Assuming 'model' is your trained random forest model
# Replace this with your actual model fitting code
# model <- randomForest(target_variable ~ ., data = your_data)

# List of input variables
input_vars <- c(
  "temp","rain", "rad", "winds", "windd", "mean_temp_6_hours", "mean_temp_12_hours",
  "mean_temp_1_day", "mean_temp_3_days", "mean_temp_5_days",
  "sum_precipitation_6_hours", "sum_precipitation_12_hours",
  "sum_precipitation_1_day", "sum_precipitation_3_days", "sum_precipitation_5_days")

# UI portion of the Shiny app
ui <- fluidPage(
  titlePanel("Random Forest Predictor"),
  sidebarLayout(
    sidebarPanel(
      lapply(input_vars, function(var) {
        sliderInput(var, var, min = 0, max = 1000, value = 0)
      }),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      plotOutput("predictionPlot")
    )
  )
)

# Server logic
server <- function(input, output) {

  observeEvent(input$predictButton, {


  output$predictionPlot <- renderPlot({

    # Create a data frame with the input values
    meteoswiss <- data.frame(sapply(input_vars, function(var) input[[var]]))


    tiffs<-tiffs_only


    for (name_var in rownames(meteoswiss)) {
      temp <- terra::rast(ncol=293, nrow=247, xmin=2592670, xmax=2607320, ymin=1193202, ymax=1205552,names = name_var)
      terra::values(temp) <- meteoswiss[name_var,1]

      temp <- crop(temp,tiffs)
      temp <- resample(temp,tiffs)
      tiffs <- c(tiffs,temp)


    }

    temperature <- terra::predict(tiffs,random_forest_model,na.rm = T)


    extent <- rgdal::readOGR("../data/Map/Extent_Bern.shp")
    rivers <- rgdal::readOGR("../data/Map/Aare.shp")
    color = colorRampPalette(c("blue", "white", "red"))(20)
    terra::plot(temperature,color = color)
    sp::plot(extent, add = T)
    sp::plot(rivers, add = T)
    points( 2601930.3,1204410.1 , pch = 16, cex = 1)









    # Predict using the random forest model
    # prediction <- predict(model, newdata = new_data)

    # Plot the prediction
    # plot(prediction, main = "Predicted Output", xlab = "Input Parameters", ylab = "Predicted Value")
  })
  })
}

# Run the application


