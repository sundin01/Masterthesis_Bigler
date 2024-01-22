### Generates Tiffs with land use classes by Moritz Burger and distances 25-150-1000 meters zonal mean ###
# Creates a tiff layer from scratch for each prediction layer of the study by Burger et al. 2019 with the distances 25, 150,1000 meters.
# Creates three distances with zonal mean for each predictor.

print("Downloading data MOPU")

options(timeout=4000)
download.file("https://geofiles.be.ch/geoportal/pub/download/MOPUBE/MOPUBE.zip",destfile = paste0(tempdir(),"mopu.zip"))


unzip(paste0(tempdir(),"mopu.zip"),exdir = paste0(tempdir(),"mopu"))







library(dplyr)

# Read in the shapefile
shapefile <- st_read(paste0(tempdir(),"mopu/data/MOPUBE_BBF.shp"))

# Define the extent
extent_to_cut <- st_bbox(c(xmin = 2594313, xmax = 2605813, ymin = 1194069, ymax = 1204804), crs = st_crs(shapefile))

# Cut the shapefile to the specified extent
shapefile_cropped <- st_crop(shapefile, extent_to_cut)



st_rasterize(shapefile_cropped,file = paste0(tempdir(),"mopu/data/MOPUBE_BBF.tif"),dx = 5, dy = 5) #resolution of 5 meters

raster <- terra::rast(paste0(tempdir(),"mopu/data/MOPUBE_BBF.tif"))
raster <- raster[[1]]






#define classification by Burger et al.2019 for LU-classes in MPOU
classification <- tibble(
  Number_raw_data = 0:25,
  Description_raw_data = c(
    "Gebäude", "Strasse Weg", "Trottoir", "Verkehrsinsel", "Bahn", "Flugplatz", "Wasserbecken",
    "Übrige befestigte", "Acker Wiese Weide", "Reben", "Übrige Intensivkulturen", "Gartenanlage",
    "Hoch- Flachmoor", "Übrige humusierte", "Stehendes Gewässer", "Fliessenden Gewässer",
    "Schilfgürtel", "Geschlossener Wald", "Wytweide dicht", "Wytweide offen", "Übrige bestockte",
    "Fels", "Gletscher Firn", "Geröll Sand", "Abbau Deponie", "Übrige Vegetationslose"
  ),
  Reclassified_category = c(
    "Land Cover Building", "Open Space Sealed", "Open Space Sealed", "Open Space Sealed",
    "Open Space Sealed", "Open Space Sealed", "Open Space Water", "Open Space Sealed",
    "Open Space Agriculture", "Open Space Agriculture", "Open Space Agriculture", "Open Space Garden",
    "Did not appear", "Open Space Agriculture", "Open Space Water", "Open Space Water",
    "Open Space Water", "Open Space Forest", "Did not appear", "Did not appear", "Open Space Forest",
    "Open Space Sealed", "Did not appear", "Open Space Sealed", "Open Space Sealed", "Open Space Sealed"
  )
) |> tidyr::drop_na()
#define the meters according to Burger et al., will not be used here, just for abbreviation "Variable".
meters <- tibble(
  Reclassified_category = c("Land Cover Building", "Open Space Sealed", "Open Space Forest", "Open Space Garden", "Open Space Water", "Open Space Agriculture"),
  Variable = c("LC_B", "OS_SE", "OS_FO", "OS_GA", "OS_WA", "OS_AC"),
  Abbreviation = rep("25/50/150/250/500", 6),
  Buffer_radii_tested = rep("25/50/150/250/500", 6),
  Unit = rep("%", 6),
  Chosen_buffer_radiusm = c(250, 500, 250, 25, 150, 500)
)
#combine dataframes
classification <- inner_join(classification,meters, by = "Reclassified_category")

print("Processing MOPU raw layers")
#write rasters according to LU-class in data-raw
for (class in unique(classification$Variable)) {
number_classes <- classification |>
    filter(Variable == class) |>
    dplyr::select(Number_raw_data) |>
  unlist()
print(class)
print(number_classes)
temp_raster <- raster %in% number_classes*1


terra::writeRaster(temp_raster,paste0("../data-raw/",class,".tif"),overwrite = T)

}


#source focal function for zonal mean
source("../R/tiff_focal.R")


for (file in unique(classification$Variable)) {


  raster_data <- rast(paste0("../data-raw/",file,".tif"))

  print(paste0("Current file: ",file))


for (meter in c(25,150,1000)) {


  tiff_focal(raster_data,meter,paste0(file,".tif")) #meters always 25,150,1000 for each class
}

}

#Now BH

download.file("https://geofiles.be.ch/geoportal/pub/download/GEBHOEHE/GEBHOEHE.zip",destfile = paste0(tempdir(),"GEBHOEHE.zip"))
unzip(paste0(tempdir(),"GEBHOEHE.zip"),exdir = paste0(tempdir(),"/GEBHOEH"))

# Read in the shapefile
shapefile <- st_read(paste0(tempdir(),"/GEBHOEH/GEBHOEHE/data/GEBHOEHE_GEBHOEHE.shp"))

# Define the extent, same as before

extent_to_cut <- st_bbox(c(xmin = 2594313, xmax = 2605813, ymin = 1194069, ymax = 1204804), crs = st_crs(shapefile))

# Cut the shapefile to the specified extent
shapefile_cropped <- st_crop(shapefile, extent_to_cut)


#now we rasterize the shapefile
st_rasterize(shapefile_cropped,file = paste0(tempdir(),"/GEBHOEH/GEBHOEHE/data/GEBHOEHE_GEBHOEHE.shp"),dx = 5, dy = 5) #resolution of 5 meters
#read in as raster
raster_BH <- terra::rast(paste0(tempdir(),"/GEBHOEH/GEBHOEHE/data/GEBHOEHE_GEBHOEHE.shp"))
raster_BH <- raster_BH[[2]]

#again make zonal mean, with NA still in
for (meter in c(25,150,1000)) {


  tiff_focal(tiff = raster_BH,meter,"BH_NA.tif")
}

#processing for NA removal, a bit ugly and non generic, sorry..
raster_25 <- terra::rast("../data/Tiffs/BH_NA_25.tif")
raster_150 <- terra::rast("../data/Tiffs/BH_NA_150.tif")
raster_1000 <- terra::rast("../data/Tiffs/BH_NA_1000.tif")
raster_25 <- subst(raster_25, NA, 0)
raster_150 <- subst(raster_150, NA, 0)
raster_1000 <- subst(raster_1000, NA, 0)
names(raster_25) <- "BH_25"
names(raster_150) <- "BH_150"
names(raster_1000) <- "BH_1000"
writeRaster(raster_25, filename="../data/Tiffs/BH_25.tif",overwrite = T)
writeRaster(raster_150, filename="../data/Tiffs/BH_150.tif",overwrite = T)
writeRaster(raster_1000, filename="../data/Tiffs/BH_1000.tif",overwrite = T)
file.remove("../data/Tiffs/BH_NA_25.tif")
file.remove("../data/Tiffs/BH_NA_150.tif")
file.remove("../data/Tiffs/BH_NA_1000.tif")




#DEM
# Read the CSV file containing links
file_data <- read.table("../data/ch.swisstopo.swissalti3d-TMP02zny.csv",header = F)

# Function to download files
download_files <- function(url, destination_folder) {
  # Extract the file name from the URL
  file_name <- basename(url)

  # Create destination file path
  destination_path <- file.path(destination_folder, file_name)

  # Download  file
  download.file(url, destfile = destination_path, mode = "wb")
}

# Folder where to save
output_folder <- paste0(tempdir(),"/DEM")

# Create the output folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Loop through each link and download the file
for (link in file_data$V1) {
  download_files(link, output_folder)
}

# Paths to DEMs
DEM_paths <- paste0(paste0(tempdir(),"/DEM/"),list.files(paste0(tempdir(),"/DEM")))

# Merge them
terrainr::merge_rasters(DEM_paths,output_raster = paste0(tempdir(),"/DEM/DEM.tif"),options = "BIGTIFF=YES",overwrite = TRUE)


DEM <- terra::rast(paste0(tempdir(),"/DEM/DEM.tif"))
ex <- raster_25#not very elegant, maybe improve?

DEM <- terra::resample(DEM,ex)

terra::writeRaster(DEM, filename = "../data/Tiffs/DEM.tif",overwrite = T)


#Slope and aspect(NOR from Burger replacement)


slope <- terra::terrain(DEM,v = "slope")

slope <- terra::resample(slope,ex)
# Again for all distances
for (meter in c(25,150,1000)) {


  tiff_focal(tiff = slope,meter,"SLO.tif")
}


#Now same for aspect

aspect <- terra::terrain(DEM,v = "aspect")
aspect <- terra::resample(aspect,ex)
for (meter in c(25,150,1000)) {


  tiff_focal(tiff = aspect,meter,"ASP.tif")
}


#and Vegetation height
if (Sys.Date()>as.Date("2024-03-01")){return()} #ensure reproducability after march 2024

download.file("https://www.dropbox.com/scl/fi/ywx8f4cufj0l43p9nh5ze/VH_WSL_21.tif?rlkey=swtvr5zw4sit9qtw5pu4ju5o3&dl=1", destfile = paste0(tempdir(),"/VH.tif"))
VH <- terra::rast(paste0(tempdir(),"/VH.tif"))
VH <- terra::resample(VH,ex)

#and zonal mean again
for (meter in c(25,150,1000)) {

  tiff_focal(tiff = VH,meter,"VH.tif")
}



#Flowacc based on DSM, DSM by combinind DEM and VH and BH

VH <- terra::resample(VH,ex)
DSM <- terra::mosaic(DEM,raster_BH,fun = "sum")
DSM <- terra::mosaic(DSM,VH,fun = "sum")


flowacc <- terra::terrain(DSM,v = "flowdir")
flowacc <- terra::resample(flowacc,ex)
for (meter in c(25,150,1000)) {



  tiff_focal(tiff = flowacc,meter,"FLAC.tif")
}



#sky view alternative: roughness? just the opposite?

roughness <- terra::terrain(DSM,v = "roughness")
roughness <- terra::resample(roughness,ex)
for (meter in c(25,150,1000)) {
  tiff_focal(tiff = roughness,meter,"ROU.tif")
}


tpi <- terra::terrain(DSM,v = "TPI")
tpi <- terra::resample(tpi,ex)
for (meter in c(25,150,1000)) {
  tiff_focal(tiff = tpi,meter,"TPI.tif")
}
