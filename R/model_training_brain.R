model_training_brain <- function(){
  #asks for how the models should be run
  repeat{ print("The training of the models with the full dataset takes a while.")
    model_demo <<- readline(prompt = "Would you like to run the model training with 5% of the data (demo mode)? [y/n] ")
    if(model_demo %in% c("y","n")){break}
  }
  repeat{
    print("There are two advanced models that are implemented. One is a XGBoost and one is a neural network.")
    print("Both models take a long time to run and are only recommended to run in demo mode!")
    print("To run the neural network, Python version 3.11 must be installed and also devtools must be installed (is not checked for!)")
    advanced_models <<- readline(prompt = "Would you like to run these advanced models? [y/n] ")
    if(advanced_models %in% c("y","n")){break}
  }


}
