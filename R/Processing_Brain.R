
preprocessing <- function(){
#Brain of what to do in main for preprocessing

# We want to know, if a certain file already exists
name.of.file <- "../data/Combined.csv"



# If do not exists such a file, we create it
if (file.exists(name.of.file)){
  repeat{
  print("Combined.csv exists, data processing not required.")
  processing = readline(prompt = "Would you still like to redo the processing of the data? [y/n] ")
  if(processing %in% c("y","n")){break}
  }

}else{

  print("Basefile doesnt exist, data processing required.")
  processing <- "y"
}


  if (processing == "y") {

    dir.create("../data-raw/", showWarnings = FALSE) #create data-raw if missing...
    dir.create("../data/Tiffs/", showWarnings = FALSE) #create data-raw if missing...

    # List all files within Tiffs
    files <- list.files(path = "../data/Tiffs", full.names = TRUE)


    # Remove all Tiffs when processing, to keep organised
    if (length(files) > 0) {
      unlink(files, recursive = TRUE)
    }



     repeat{
      print("File will be processed. There is the option to generate the geospatial layers  from scratch (time intensive; 20min-2h) or download the geospatial layers from an external source (quick,2min).")
      print("The download version will result in all geospatial layers (3 geospatial layers per prediction class) being downloaded.")
      demo = readline(prompt = "Would you like to download the geospatial layers? [y/n] ")
      if(demo %in% c("y","n")){break}
     }
    if (demo == "y") {


      source("../R/demo_download.R")


      }else{
        repeat{
        print("Geospatial layers will be generated. There is the option to generate the geospatial layers as in BURGER et al. 2019 ( 1 geospatial layer per prediction class) or with all geospatial layers ( 3 predictors per prediction class; 25/150/1000 meters per prediction class).")
        full = readline(prompt = "Would you like to generate all geospatial layers? [y/n] ")
        if(full %in% c("y","n")){break}
        }
    if (full == "y") {
      print("all geospatial layers are generated.")
      source("../R/raw_tif_processing_2.R")
    }else{
      print("Geospatial layers from Burger et al. 2019 are generated.")
      source("../R/raw_tif_processing.R")
    }


  }
    #do in any case when processing of tiffs
    print("CSV file is being created... (30 seconds)")
    source("../R/data_combination.R")
    data_combination()



  }else{
    print("there will be no processing")
  }



}
