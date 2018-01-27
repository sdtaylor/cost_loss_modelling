#Take the original modis landcover rasters for each year and crop them to
#the training and testng areas
library(raster)
library(rgdal)
library(sp)

training_years = c(2001,2002,2003,2004,2005,2006,2007)
testing_years  = c(2008,2009,2010,2011,2012,2013)

raw_modis_dir='./raw_data/idaho_fire/downloaded_modis/'

file_list=list.files(raw_modis_dir, pattern='*tif', full.names = TRUE)

file_list=file_list[!grepl('xml', file_list)]
file_list=file_list[!grepl('ovr', file_list)]


study_area=readOGR('./raw_data/idaho_fire/gis/', 'study_area')

#Crop training years
for(this_year in training_years){
  file_path=grep(paste0('A',this_year), file_list, value=TRUE)
  whole_raster=raster::raster(file_path)
  cropped_raster=crop(whole_raster, study_area)

  filename=paste0(this_year,'.tif')
  writeRaster(cropped_raster, paste0('./raw_data/idaho_fire/training/',filename))
}

#Crop testing years
for(this_year in testing_years){
  file_path=grep(paste0('A',this_year), file_list, value=TRUE)
  whole_raster=raster::raster(file_path)
  cropped_raster=crop(whole_raster, study_area)
  
  filename=paste0(this_year,'.tif')
  writeRaster(cropped_raster, paste0('./raw_data/idaho_fire/testing/',filename))
}
