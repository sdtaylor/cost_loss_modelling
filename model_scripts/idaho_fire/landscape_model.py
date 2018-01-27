import gdalnumeric
import pandas as pd
import numpy as np
import gdal
from sklearn.linear_model import LogisticRegression

######################################################################################
#Write out a raster from a numpy array.
#Template: a raster file on disk to use for pixel size, height/width, and spatial reference.
#array: array to write out. Should be an exact match in height/width as the template.
#filename: file name of new raster
#inspired by: http://geoexamples.blogspot.com/2012/12/raster-calculations-with-gdal-and-numpy.html
def write_raster(array, template, filename):
    driver = gdal.GetDriverByName("GTiff")
    raster_out = driver.Create(filename, template.RasterXSize, template.RasterYSize, 1, template.GetRasterBand(1).DataType)
    gdalnumeric.CopyDatasetInfo(template,raster_out)
    bandOut=raster_out.GetRasterBand(1)
    gdalnumeric.BandWriteArray(bandOut, array)

#Read in a list of rasters and stack them into a single array. (rows x column x numRasters)
def stackImages(fileList):
    fullYear=gdalnumeric.LoadFile(fileList[0]).astype(np.int)
    for thisImage in fileList[1:]:
        image=gdalnumeric.LoadFile(thisImage)
        fullYear=np.dstack((fullYear, image))
    return(fullYear)

#Predictor values are the current state of a pixel x,y + the state of surrounding pixels n
#image: 2d x y array
#x,y: location of the focal pixel
#n  : number of surrounding pixels to consider
def extract_predictor_values(image, row, col, n):
    all_pixel_data={}
    all_pixel_data['t0']=image[row,col]

    if n==8:
        surrounding=image[row-1:row+2 , col-1:col+2].reshape((9))
        #Delete the focal pixel that is in this 3x3 array
        surrounding=np.delete(surrounding, 4)
    elif n==24:
        surrounding=image[row-2:row+3 , col-2:col+3].reshape((25))
        surrounding=np.delete(surrounding, 12)

    #Convert surrounding pixel values to percent of each class
    surrounding_size=len(surrounding)
    cover_catagories=list(range(1,15))
    for catagory in range(1, len(cover_catagories)+1):
        all_pixel_data['surrounding_cat'+str(catagory)]= np.sum(surrounding==catagory) / surrounding_size

    return(all_pixel_data)


#Extract and organize a timeseries of arrays for model fitting
def array_to_model_input_fitting(a):
    array_data=[]
    for row in range(1, a.shape[0]-1):
        for col in range(1, a.shape[1]-1):
            for time in range(0, a.shape[2]-1):
                array_data.append(extract_predictor_values(a[:,:,time], row, col, 8))
                array_data[-1]['t1'] = a[row,col,time+1]

    array_data=pd.DataFrame(array_data)
    return array_data

#Extract and organize data for model predictions
#Same as above function but doesn't get t+1, and
#accepts just a single timestep at a time
def array_to_model_input(a):
    array_data=[]
    for row in range(1, a.shape[0]-1):
        for col in range(1, a.shape[1]-1):
            array_data.append(extract_predictor_values(a, row, col, 8))

    array_data = pd.DataFrame(array_data)
    return array_data

#The model outpus a 1d array of pixel predictions. This converts it back to that
#and adds on the padded layer that is subtracted in array_to_model_input.
#The value of the padded layer is 1, the most common class (evergreen needleleaft)
def model_output_to_array(output, desired_shape, pad_value=1):
    reshaped = np.zeros(desired_shape)
    reshaped[:] = pad_value
    reshaped[1:-1,1:-1] = output.reshape((desired_shape[0]-2, desired_shape[1]-2))
    return reshaped



###################################################################
###################################################################
test_data_dir='./raw_data/idaho_fire/testing/'
train_data_dir='./raw_data/idaho_fire/training/'

training_years = [2001,2002,2003,2004,2005,2006,2007]
testing_years  = [2008,2009,2010,2011,2012,2013]

##########################################################
#Build the training model

training_file_list = [train_data_dir+str(year)+'.tif' for year in training_years]

all_training_years = stackImages(training_file_list)
training_data = array_to_model_input_fitting(all_training_years)

model = LogisticRegression(multi_class='multinomial', solver='lbfgs')
model.fit(training_data.drop('t1',1), training_data['t1'])

############################################################
#Make predictions of the testinig data using only the initial year as input
#Also accumulate observations of the rest of the data.

testing_file_list=[test_data_dir+str(year)+'.tif' for year in testing_years]
observations=stackImages(testing_file_list)
#initial year is the  last of the training timeseries
initial_year=all_training_years[:,:,-1]
#A matching array to hold  predictions
predictions = np.zeros_like(observations)

#extents in the testing timeseries
num_years=observations.shape[2]
area_shape=initial_year.shape

#The width, height, CRS, and pixel size of the template will be
#used to write rasters that were modified using numpy arrays
raster_template=gdal.Open(train_data_dir+'2001.tif')

#Do an auto-regressive model and write out each yearly prediction raster
last_year_prediction=initial_year.copy()
for year_i, year in enumerate(testing_years):
    #this_year_prediction = model.predict(array_to_model_input(this_year_prediction)).reshape(area_shape)
    this_year_prediction_binary = model.predict(array_to_model_input(last_year_prediction))
    this_year_prediction_binary = model_output_to_array(this_year_prediction_binary, desired_shape=area_shape)

    # Probability of class 10 for this year
    this_year_prediction_prob = model.predict_proba(array_to_model_input(last_year_prediction))[:,9]
    this_year_prediction_prob = model_output_to_array(this_year_prediction_prob, desired_shape=area_shape)

    predictions[:,:,year_i] = this_year_prediction_binary
    last_year_prediction = this_year_prediction_binary.copy()
    write_raster(this_year_prediction_prob, template=raster_template, filename='./initial_model_output/idaho_fire/'+str(year)+'_prediction.tif')


