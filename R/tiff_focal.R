tiff_focal <- function(tiff,meter,filename){

library(stringr)





  n <-2 * round(meter/2)/5+1 #get unequal number, div by 5 since 5m resolution

  mean_focal <- focal(tiff, w=matrix(1, nrow=n, ncol=n), fun=mean, na.rm=TRUE)
  filename1 = paste("../data/Tiffs/",str_sub(filename, end = -5),"_",meter,".tif",sep = "")
  names(mean_focal) <- paste0(str_sub(filename, end = -5),"_",meter)
  print(filename1)
  writeRaster(mean_focal, filename=filename1,overwrite = T)




}








