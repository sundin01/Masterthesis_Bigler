### Combines data sources ###
# Combines data from meteoswiss and the logger data, both aggregated to the hour, then also calculates rolling means
# Later, Tiffs files are used to add layer data to each measurement point, (land use classes value sand other geolayers)

data_combination <- suppressWarnings(function(){

  measurement_files <- list.files("../data/Measurements/")
  measurement_files <- purrr::map(as.list(paste("../data/Measurements/",measurement_files,sep = "")),~read_csv(.))
  measurement_files <- bind_rows(measurement_files, .id = "column_label")
  measurement_files <-  mutate(measurement_files,time = mdy_hm(Zeit)) |>
    dplyr::select(-Zeit)
  measurement_files <- measurement_files |>
    mutate(hour = hour(time),
           month = month(time),
           day = day(time),
           year = year(time))

  measurement_files <- measurement_files |>
    group_by(hour, day, month, year) |>
    dplyr::summarise(across(where(is.numeric), mean))

  measurement_files <-  measurement_files |> pivot_longer(cols = starts_with("L"), names_to = "Log_Nr",values_to = "temperature") |>
    mutate(Log_Nr = as.numeric(str_replace(Log_Nr, "Log_", ""))) |>
    drop_na()


  meteoswiss <- read_delim("../data/Meteoswiss/order_114596_data.txt",delim = ";")
  meteoswiss <- meteoswiss |>
    mutate(time = as.POSIXct(as.character(time), format = "%Y%m%d%H%M"),
           temp = as.numeric(tre200s0),
           rain = as.numeric(rre150z0),
           rad = as.numeric(gre000z0),
           winds = as.numeric(fkl010z0),
           windd = as.numeric(dkl010z0))|>
    dplyr::select(time,temp,rain,rad,winds,windd) |>
    mutate(time = time+hours(2)) |>
    drop_na() #some parsing error,dk why, 60 NA..





  meteoswiss <- meteoswiss |>
    mutate(hour = hour(time),
           month = month(time),
           day = day(time),
           year = year(time))
  meteoswiss <- meteoswiss |>
    dplyr::group_by(hour, day, month, year) |>
    dplyr::summarise(across(where(is.numeric), mean),.groups = 'drop') |>
    mutate(rain = rain*6) # to get sum... was mean

  meteoswiss <- meteoswiss|>
    mutate(timestamp = ymd_h(paste(year,month,day,hour,sep = "-"))) |>
    arrange(timestamp)|>
    mutate(
      mean_temp_6_hours = zoo::rollmean(temp, k = 6, fill = NA, align = "right"),
      mean_temp_12_hours = zoo::rollmean(temp, k = 12, fill = NA, align = "right"),
      mean_temp_1_day = zoo::rollmean(temp, k = 24 * 1, fill = NA, align = "right"),
      mean_temp_3_days = zoo::rollmean(temp, k = 24 * 3, fill = NA, align = "right"),
      mean_temp_5_days = zoo::rollmean(temp, k = 24 * 5, fill = NA, align = "right"),
      sum_precipitation_6_hours = zoo::rollsum(rain, k = 6, fill = NA, align = "right"),
      sum_precipitation_12_hours = zoo::rollsum(rain, k = 12, fill = NA, align = "right"),
      sum_precipitation_1_day = zoo::rollsum(rain, k = 24 * 1, fill = NA, align = "right"),
      sum_precipitation_3_days = zoo::rollsum(rain, k = 24 * 3, fill = NA, align = "right"),
      sum_precipitation_5_days = zoo::rollsum(rain, k = 24 * 5, fill = NA, align = "right"),
      sum_precipitation_10_days = zoo::rollsum(rain, k = 24 * 10, fill = NA, align = "right")
    ) |>
    mutate_at(vars(starts_with("sum_precip")), ~ ifelse(. < 0.1, 0, .))






  combined = inner_join(measurement_files,meteoswiss,by = c("hour","day","month","year"))

  measurement_metadata <- read_csv("../data/Metadata_19-22.csv")


  combined = inner_join(combined, measurement_metadata, by = "Log_Nr")

  combined <- combined |> ungroup()

  combined <- combined |>
    dplyr::mutate(ID = row_number())


  tiff_names <- list.files("../data/Tiffs/")

  tiff_paths <- paste0("../data/Tiffs/",tiff_names)

  tiffs<-terra::rast(tiff_paths)



  spat_points <- combined |> dplyr::select(c(LV_03_E,LV_03_N))

  extracted <- terra::extract(tiffs,spat_points)
#up to here works...
  combined <- inner_join(combined,extracted,by = "ID")

combined <- combined |>
  select(-ID)

  write_csv(combined,"../data/Combined.csv")
  return(combined)
})
