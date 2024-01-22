# Function to generate a map
map_generater <- function(model, meteoswiss_input){

meteoswiss <- meteoswiss_input

print('read geopsatial data...')
tiff_names <- str_sub(list.files("../data/Tiffs/"),end = -5)
tiffs_only <- terra::rast(paste0("../data/Tiffs/",tiff_names,".tif"))

#------------------------------------------------------------------------------
# Extract the model name. We need it to label the map
model_name <- deparse(substitute(model))

model_input <- str_replace_all(model_name, "_", " ")
print('read meteorologic data...')

#------------------------------------------------------------------------------
# Read all column for which we want to use for our predictions
meteoswiss_selection <- paste(c("temp","precip","rad","wind","rain"), collapse = "|")
names(meteoswiss) <- combined |>
  dplyr::select(matches(meteoswiss_selection),
                -c(temperature)) |>
  colnames()

# Goes:""temp" "rain" rad" "winds" "windd" "mean_temp_6_hours" "mean_temp_12_hours" "mean_temp_1_day"
# "mean_temp_3_days" "mean_temp_5_days" "sum_precipitation_6_hours" "sum_precipitation_12_hours"
# "sum_precipitation_1_day" "sum_precipitation_3_days" "sum_precipitation_5_days" "sum_precipitation_10_days"

#------------------------------------------------------------------------------
# Loop to process the tiffs
for (name_var in names(meteoswiss)) {
  temp <- terra::rast(ncol=293, nrow=247, xmin=2592670, xmax=2607320, ymin=1193202, ymax=1205552,names = name_var)
  terra::values(temp) <- unname(meteoswiss[name_var])
  print(paste0(unname(meteoswiss[name_var]),": ",name_var))
  temp <- crop(temp,tiffs_only)
  temp <- resample(temp,tiffs_only)
  tiffs_only <- c(tiffs_only,temp)
}


print('Tiff processing successful. Model prediction in progress...')
temperature <- terra::predict(tiffs_only, model, na.rm = TRUE)

print('Model prediction sucessful. Create a data frame')
temperature_df <- terra::as.data.frame(temperature, xy = TRUE)

#------------------------------------------------------------------------------
# load the spatial layer for communal boarders and the rivers
extent <- st_read("../data/Map/Extent_Bern.shp")
rivers <- st_read("../data/Map/Aare.shp")

xmin <- min(temperature_df$x)
xmax <- max(temperature_df$x)
ymin <- min(temperature_df$y)
ymax <- max(temperature_df$y)

new_bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
rivers <- st_crop(rivers, new_bbox)
extent <- st_crop(extent, new_bbox)

print('Your card will now be generated')

#------------------------------------------------------------------------------
# Read out the absolute maximum value to generate leveles for the legend
max_value <- max(abs(temperature_df$lyr1), na.rm = TRUE)

#------------------------------------------------------------------------------
# Generate the map
p <- ggplot() +
  geom_raster(data = temperature_df, aes(x = x, y = y, fill = lyr1)) +
  geom_sf(data = rivers, color = 'black', linewidth = .4) +
  geom_sf(data = extent, linewidth = .2, color = 'black') +
  geom_point(aes(x = 2601930.3, y = 1204410.1)) +
  annotate("text", x = 2602300, y = 1204410.1, label= "AWS Zollikofen", hjust = 0) +
  labs(title = paste('Temperatureanomaly for the suburban area of Bern'),
       subtitle = paste('This map uses a',model_input,'and the following variable inputs:',
                        "\nTemp [°C]: now = ",meteoswiss[1],', 6h =',meteoswiss[6],
                        ', 12h =',meteoswiss[7],', 1d =',meteoswiss[8],', 3d =',meteoswiss[9],
                        ', 5d =', meteoswiss[10],
                        "\nPrec [mm]: now =",meteoswiss[2],', 6h =',meteoswiss[11],
                        ', 12h =',meteoswiss[12],', 1d  =',meteoswiss[13],', 3d =', meteoswiss[14],
                        ', 5d =', meteoswiss[15],', 10d =',meteoswiss[16],
                        '\nWindspeed [m/s] =',meteoswiss[4],', Winddirection [°] =',meteoswiss[5],
                        '\nRadiation [W/m^2*s] =',meteoswiss[3]), x = 'Longitude', y = 'Latitude',
       fill = expression(paste(Delta,'Temp [K]'))) +
  scale_fill_gradient2(low = "blue4",
                       mid = "white",
                       high = "red4",
                       midpoint = 0,
                       space = 'Lab',
                       guide = 'colourbar',
                       aesthetics = 'fill',
                       limits = c(-max_value, max_value)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  theme(
    title = element_text(size = 10, face = 'bold'),  # Title text size # Subtitle text size
    axis.title = element_text(size = 8),  # Axis label text size
    axis.text = element_text(size = 8),  # Axis tick labels text size
    legend.title = element_text(size = 8),  # Legend title text size
    legend.text = element_text(size = 8),  # Legend labels text size
    plot.subtitle=element_text(size=8, face = 'plain'))

print(p)
print('Everything went fine!')
}
