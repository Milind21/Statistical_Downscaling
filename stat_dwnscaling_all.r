#installing the libraries
install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("stats")   #linear regression
install.packages("Metrics") #mae,mape,mse
install.packages("BBmisc")  #normalise
#loading the necessary libraries
library(sp)
library(raster)
library(rgdal)
library(stats)
library(Metrics)
library(BBmisc)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#intialising vetor file for clipping
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/internship_TERISAS/maharastra_bnd")
temp_vect<-readOGR("maha_utm.shp")                #loading maharastra shape file

#creaing a stack
files <- list.files(path="C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirp_daily_2015", pattern = "tif$",full.names = TRUE)
rast_stack<-stack(files)
sr<-" +proj=utm +zone=43 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "

#changing the projection
for (i in 1:length(files)) {
   tempras <- raster(files[i])
   projras<-projectRaster(tempras,crs=sr)
   if(i==1){
     ras_st <- projras
   }
   if(i>1){
     ras_st <- stack(ras_st, projras)
   }
}

rast_stack2<-ras_st

#clipping chirps data
for(i in 1:length(files)){
  tempras<-rast_stack2[[i]]
  cropras<-crop(tempras,extent(temp_vect))
  maskras<-mask(cropras,temp_vect)
  if(i==1){
    ras_st <- maskras
  }
  if(i>1){
    ras_st <- stack(ras_st, maskras)
  }
}

rast_stack3<-ras_st

#clipping ndvi
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/internship_TERISAS/ndvi_annual_mean_2015")
temp_rast_ndvi<-raster("ndvi_01_17_annual_year_2015_Mean.rst")
croprast_ndvi<-crop(temp_rast_ndvi,extent(temp_vect))
maskrast_ndvi<-mask(croprast_ndvi,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
writeRaster(maskrast_ndvi,filename = "clipping_ndvi.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#resampling the resolution
i<-1
for(i in 1:length(files)){
  tempras<-rast_stack3[[i]]
  rsmpldrs<-raster::resample(tempras,maskrast_ndvi,method='ngb')
  if(i==1){
    rst_stck<-rsmpldrs
  }
  if(i>1){
    rst_stck<-stack(rst_stck,rsmpldrs)
  }
}
rast_stack4<-rst_stck

#clipping lst
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/internship_TERISAS/lst_annual_mean_2015")
rast_lst<-raster("lst_01_17_annual_year_2015_Mean.rst")
croprast_lst<-crop(rast_lst,extent(temp_vect))
maskrast_lst<-mask(croprast_lst,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
writeRaster(maskrast_lst,filename = "clipping_lst.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#clipping elevation
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/internship_TERISAS/elevation")
rast_ele<-raster("srtm_5km.rst")
croprast_ele<-crop(rast_ele,extent(temp_vect))
maskrast_ele<-mask(croprast_ele,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
writeRaster(maskrast_ele,filename = "clipping_ele.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#clipping location i.e lattitude and longitude
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/internship_TERISAS/location")
rast_lat<-raster("xloc.rst")
rast_long<-raster("yloc.rst")
croprast_lat<-crop(rast_lat,extent(temp_vect))
croprast_long<-crop(rast_long,extent(temp_vect))
maskrast_lat<-mask(croprast_lat,temp_vect)
maskrast_long<-mask(croprast_long,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
writeRaster(maskrast_lat,filename = "clipping_lat.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')
writeRaster(maskrast_long,filename = "clipping_long.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#setting the negative vaue in chirps to zero
rast_stack5<-rast_stack4
for (i in 1:length(files)) {
  tempras<-rast_stack5
  values(tempras)[values(tempras)<0] = 0
}
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
writeRaster(rast_stack5,filename = "clipping_chirps.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#summing the raster stack to get cumulative 
sum_ras<-calc(rast_stack5,sum)
sum_ras2<-raster("sum_ras.tif")
writeRaster(sum_ras,filename = "sum_ras.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#finding proportion and storing in a stack
for (i in 1:length(files)) {
    tempras<-rast_stack5[[i]]
    propras<-tempras/sum_ras
    if(i==1){
      ras_st<-propras
    }
    if(i>1){
      ras_st<-stack(ras_st,propras)
    }
}
rast_stack6<-ras_st
writeRaster(rast_stack6,filename = "proportion_stack.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/daily proportion/")
for (i in 1:length(files)) {
  rastname<-paste0("proportion_ras_",i,".tif")
  writeRaster(rast_stack6[[i]],filename = rastname,datatype='FLT8S',options='GTiff',overwrite=TRUE )
}

#creating dataframe with all annual data
annual_Stack<-stack(sum_ras,maskrast_ndvi,maskrast_lst,maskrast_long,maskrast_lat,maskrast_ele)
annual_mat<-rasterToPoints(annual_Stack)

#deleting the rows with any NA in the dataframe
annual_mat_noNA<-annual_mat[rowSums(is.na(annual_mat)) <= 0,]
annual_df<-as.data.frame(annual_mat_noNA)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#training and testing data and normalization
train<-sample(1:12077,8454,replace = FALSE)
traindata<-annual_df[train,]
testdata<-annual_df[-train,]

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#linear multiple regression
modellm<-lm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,traindata)
predictlm<-predict(modellm,testdata)
rmse(testdata$layer,predictlm)
mae(testdata$layer,predictlm)
mape(testdata$layer,predictlm)
cor(testdata$layer,predictlm)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#svm
install.packages("e1071")
install.packages("psych")
library(e1071)
library(psych)
describe(annual_df)               #to check the skewness and kurtosis
set.seed(1000)
#training eps-regression
fitepslinear<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="eps-regression",kernel="linear",cross=10)
fitepspoly<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="eps-regression",kernel="polynomial",cross=10)
fitepsradial<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="eps-regression",kernel="radial",cross=10)
fitepssigmoid<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="eps-regression",kernel="sigmoid",cross=10)
#testing eps regression
predepslinear<-predict(fitepslinear,testdata)
predepspoly<-predict(fitepspoly,testdata)
predepsradial<-predict(fitepsradial,testdata)
predepssigmoid<-predict(fitepssigmoid,testdata)

#training nu-regression
fitnulinear<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="linear",cross=10)
fitnupoly<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="polynomial",cross=10)
fitnuradial<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="radial",cross=2)
fitnusigmoid<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="sigmoid",cross=10)
#testing nu-regression
prednulinear<-predict(fitnulinear,testdata)
prednupoly<-predict(fitnupoly,testdata)
prednuradial<-predict(fitnuradial,testdata)
prednusigmoid<-predict(fitnusigmoid,testdata)

#checking correlation implies usage of radial basis for nu-regression
#tuning nu-regression
#tuning the cost
tune_c_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
#testing for beyond 100 
tune_c_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(cost=c(100,250,500,1000)))
#checking the summary gives cost to be 500

#tuning the nu
tune_nu_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(nu=c(0.01,0.1,0.5,1,5,10)))
#testing for less than 0.01
tune_nu_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(nu=c(0.001,0.005,0.01)))
#tuning for less than 0.001
tune_nu_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(nu=c(0.00001,0.0001,0.001)))
#tuning for less than 1e-05
tune_nu_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(nu=c(1e-06,1e-07,1e-05)))
#checking the summary gives nu to be 1e-06
tune_svm<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(cost=c(1,10,100),nu=c(0.001,0.01,0.1)))
tune_svm_2<-tune(svm,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,kernel="radial",ranges = list(cost=c(500,1000,100),nu=c(1e-06,1e-04,0.001)))

fitnuradial2<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="radial",cross=2,cost=500)
prednuradial2<-predict(fitnuradial2,testdata)
rmse(testdata$layer,prednuradial2)
mae(testdata$layer,prednuradial2)
mape(testdata$layer,prednuradial2)
cor(testdata$layer,prednuradial2)

#for default cases
rmse(testdata$layer,prednuradial)
mae(testdata$layer,prednuradial)
mape(testdata$layer,prednuradial)
cor(testdata$layer,prednuradial) 

fitnuradial3<-svm(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,type="nu-regression",kernel="radial",cross=2,cost=1000,nu=1e-06)
prednuradial3<-predict(fitnuradial3,testdata)
cor(testdata$layer,prednuradial3)
#best model fitnuradial2
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#random forest
install.packages("randomForest")
library(randomForest)
#random forest model default values
set.seed(1000)
rforest1<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,replace=TRUE)
predrf1<-predict(rforest1,testdata)
rmse(testdata$layer,predrf1)
mae(testdata$layer,predrf1)
mape(testdata$layer,predrf1)
cor(testdata$layer,predrf1) 

#tuning the random forest 
tune_rf<-tune(randomForest,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,ranges = list(ntree=c(100,500,1000),mtry=c(1,2)))
tune_rf2<-tune(randomForest,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,ranges = list(ntree=c(100,500,1000)))
#new model according to tuning
rforest2<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,replace=TRUE,ntree=1000,mtry=2)
predrf2<-predict(rforest2,testdata)
rmse(testdata$layer,predrf2)
mae(testdata$layer,predrf2)
mape(testdata$layer,predrf2)
cor(testdata$layer,predrf2) 
#tuning for refinement
tune_rf3<-tune(randomForest,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,ranges = list(ntree=c(500,750,1000),mtry=c(1,2,3)))
tune_rf4<-tune(randomForest,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,ranges = list(ntree=c(750,1000),mtry=c(2,3,4,5)))
rf_best<-tune_rf4$best.model
save(rf_best,file="rf_bestmodel.RDAta")
#model 3
rforest3<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,replace=TRUE,ntree=750,mtry=3)
predrf3<-predict(rforest3,testdata)
rmse(testdata$layer,predrf3)
mae(testdata$layer,predrf3)
mape(testdata$layer,predrf3)
cor(testdata$layer,predrf3) 
#best model
rforest4<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,replace=TRUE,ntree=750,mtry=5,importance=TRUE)
predrf4<-predict(rforest4,testdata)
rmse(testdata$layer,predrf4)
mae(testdata$layer,predrf4)
mape(testdata$layer,predrf4)
cor(testdata$layer,predrf4) 
rforest5<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+srtm_5km,data=traindata,replace=TRUE,ntree=750,mtry=4,importance=T)
rforest6<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc,data=traindata,replace=TRUE,ntree=1000,mtry=2)
rforest7<-randomForest(layer~lst_01_17_annual_year_2015_Mean+yloc+srtm_5km,data=traindata,replace=T,ntree=750,mtry=2)
rforest8<-randomForest(layer~ndvi_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata,replace=TRUE,ntree=750,mtry=2,importance=TRUE)
tune_rf5<-tune(randomForest,data=traindata,layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc,ranges = list(ntree=c(750,1000),mtry=c(2,3)))
tune_rf6<-tune(randomForest,data=traindata,layer~lst_01_17_annual_year_2015_Mean+xloc+srtm_5km,ranges = list(ntree=c(750,1000),mtry=c(2,3)))

#neural networks  ######not working currently
install.packages("neuralnet")
library(neuralnet)

f<-function(x){(x-min(x))/(max(x)-min(x))}
train<-sample(1:12077,8454,replace = FALSE)
traindata<-annual_df[train,]
traindata_normalized<-normalize(traindata[,3:8],method = 'scale')
traindata_normalized<-cbind(traindata[,1:2],traindata_normalized)
traindata_normalized<-as.data.frame(apply(traindata[,c(3:8)],FUN = f))
testdata<-annual_df[-train,]
testdata_normalized<-normalize(testdata[3:8],method = 'scale')
testdata_normalized<-cbind(testdata[,c(1:2)],testdata_normalized)



nn1<-neuralnet(layer~ndvi_01_17_annual_year_2015_Mean+lst_01_17_annual_year_2015_Mean+yloc+xloc+srtm_5km,data=traindata_normalized,hidden = 2,linear.output = TRUE)
prednn1<-predict(nn1,testdata_normalized)
prednn1_v2<-compute(nn1,testdata[,c(4:8)])
cor(prednn1_v2$net.result,testdata$layer)

rmse(testdata$layer,prednn1)
mae(testdata$layer,prednn1)
mape(testdata$layer,prednn1)
cor(testdata$layer,prednn1) 

#####################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################

#calculations at 1km

#loading and clipping lst
setwd( "C:/Users/P K Choudhary/Desktop/Stat_dwnscal/1km/arch1_lst")
rast_lst_1km<-raster("lst_1km_annual_year_2015_Mean.rst")
croprast_lst_1km<-crop(rast_lst_1km,extent(temp_vect))
maskrast_lst_1km<-mask(croprast_lst_1km,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_annual_1km/")
writeRaster(maskrast_lst_1km,filename = "clipping_lst_1km.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#loading and clipping elevation
setwd( "C:/Users/P K Choudhary/Desktop/Stat_dwnscal/1km/loc_Srtm")
rast_ele_1km<-raster("SRTM_1km_b1.rst")
croprast_ele_1km<-crop(rast_ele_1km,extent(temp_vect))
maskrast_ele_1km<-mask(croprast_ele_1km,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_annual_1km/")
writeRaster(maskrast_ele_1km,filename = "clipping_ele_1km.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')


#loading and clipping coordinates
setwd( "C:/Users/P K Choudhary/Desktop/Stat_dwnscal/1km/loc_Srtm")
rast_lat_1km<-raster("xloc_1km_sa.rst")
rast_long_1km<-raster("yloc_1km_sa.rst")
croprast_lat_1km<-crop(rast_lat_1km,extent(temp_vect))
maskrast_lat_1km<-mask(croprast_lat_1km,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_annual_1km/")
writeRaster(maskrast_lat_1km,filename = "clipping_lat_1km.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')
croprast_long_1km<-crop(rast_long_1km,extent(temp_vect))
maskrast_long_1km<-mask(croprast_long_1km,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_annual_1km/")
writeRaster(maskrast_long_1km,filename = "clipping_long_1km.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#loading and clipping ndvi
setwd( "C:/Users/P K Choudhary/Desktop/Stat_dwnscal/1km/Archive")
rast_ndvi_1km<-raster("ndvi_1km_annual_year_2015_Mean.rst")
croprast_ndvi_1km<-crop(rast_ndvi_1km,extent(temp_vect))
maskrast_ndvi_1km<-mask(croprast_ndvi_1km,temp_vect)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_annual_1km/")
writeRaster(maskrast_ndvi_1km,filename = "clipping_ndvi_1km.tif",datatype='FLT8S',overwrite=TRUE,options='GTiff')

#creating dataframe with all annual data at 1km
annual_Stack_1km<-stack(maskrast_ndvi_1km,maskrast_lst_1km,maskrast_long_1km,maskrast_lat_1km,maskrast_ele_1km)
annual_mat_1km<-rasterToPoints(annual_Stack_1km)

#deleting the rows with any NA in the dataframe
annual_mat_noNA_1km<-annual_mat_1km[rowSums(is.na(annual_mat_1km)) <= 0,]
annual_df_1km<-as.data.frame(annual_mat_noNA_1km)

#data preparation and matching col names for prediction
colnames(annual_df_1km)<-colnames(annual_df[1,-3])

#predicting using linear model
predictlm_1km<-predict(modellm,annual_df_1km)
rmse(testdata$layer,predictlm)
mae(testdata$layer,predictlm)
mape(testdata$layer,predictlm)
cor(testdata$layer,predictlm)

#predicting using best model of svm
prednuradial2_1km<-predict(fitnuradial2,annual_df_1km) #type= response brk
rmse(testdata$layer,prednuradial2)
mae(testdata$layer,prednuradial2)
mape(testdata$layer,prednuradial2)
cor(testdata$layer,prednuradial2)

#predicting using best model of random forest
predrf4_1km<-predict(rforest4,annual_df_1km)
rmse(testdata$layer,predrf4)
mae(testdata$layer,predrf4)
mape(testdata$layer,predrf4)
cor(testdata$layer,predrf4) 


#testing on rain gauge data
install.packages("sf")
library(sf)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal")
rainfall_csv<-read.csv("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/Rainfall_Maharashtra_2015_24x365_v2.csv")
rainfall_prj1<-st_as_sf(rainfall_csv,coords = c("Latitude","Longitude"),crs = sr)
class(rainfall_prj1)
rainfall_shpfile<-readOGR("rainfall_Station.shp")
#changing the projection 
rainfall_shpfile_utm<-spTransform(rainfall_shpfile,sr)
writeOGR(rainfall_shpfile_utm,'.',driver="ESRI Shapefile",layer = "rainfall_shpfile_utm")

write.csv(annual_df_1km,file = "annual_df_1km.csv")
annual_csv_1km<-read.csv("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/annual_df_1km.csv")

rast_shpfile<-raster(ncol=865,nrow=711)
ext_rast<-extent(annual_Stack_1km)
rast_shpfile<-projectRaster(rast_shpfile,crs=sr)
extent(rast_shpfile)<-ext_rast
rp<-rasterize(rainfall_shpfile_utm,rast_shpfile)

#=================================================================
#loading directly using raster 
ndvi_01_17_annual_year_2015_Mean<-reclassify(maskrast_ndvi_1km,cbind(NA,0))
lst_01_17_annual_year_2015_Mean<-reclassify(maskrast_lst_1km,cbind(NA,0))
yloc<-reclassify(maskrast_long_1km,cbind(NA,0))
xloc<-reclassify(maskrast_lat_1km,cbind(NA,0))
srtm_5km<-reclassify(maskrast_ele_1km,cbind(NA,0))
brk<-brick(ndvi_01_17_annual_year_2015_Mean,lst_01_17_annual_year_2015_Mean,yloc,xloc,srtm_5km)
name.array<-c("ndvi_01_17_annual_year_2015_Mean","lst_01_17_annual_year_2015_Mean","yloc","xloc","srtm_5km")
for (i in 1:nlayers(brk)) {
  names(brk[[i]])<-name.array[i]
}
prdlm<-raster::predict(brk,modellm,type='response',progress='window')
prdlm2<-crop(prdlm,extent(temp_vect))
prdlm2<-mask(prdlm2,temp_vect)
writeRaster(prdlm,filename = "predict_lm.tif",fomat="GTiff",overwrite=TRUE)
predsvm<-raster::predict(brk,fitnuradial2,type='response',progress='window')
predsvm2<-crop(predsvm,extent(temp_vect))
predsvm2<-mask(predsvm2,temp_vect)
writeRaster(predsvm,filename = "predict_svm.tif",fomat="GTiff",overwrite=TRUE)
predrf<-raster::predict(brk,rforest4,type='response',progress='window')
predrr_bnd<-crop(predrf,extent(temp_vect))
predrr_bnd<-mask(predrr_bnd,temp_vect)
writeRaster(predrf,filename = "predict_rf.tif",fomat="GTiff",overwrite=TRUE)
predrf_4var<-raster::predict(brk,rforest5,type='response',progress='window')
predrr_bnd_4var<-crop(predrf_4var,extent(temp_vect))
predrr_bnd_4var<-mask(predrr_bnd_4var,temp_vect)

predrf_3var_nosrtm<-raster::predict(brk,rforest6,type='response',progress='window')
predrf_3var_nondvi<-raster::predict(brk,rforest7,type='response',progress='window')
pred_rf8<-raster::predict(brk,rforest8,type='response')


#=============================================================================================================================
install.packages("EnvStats")
library(EnvStats)
library(sf)
setwd("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/cumulative/")
rainfall_shpfile_cumu<-readOGR("rainfall_cumu.shp")
#changing the projection 
rainfall_shpfile_utm_cumu<-spTransform(rainfall_shpfile,sr)
extractedlm_rast<-extract(prdlm,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=mean)
extractedsvm_rast<-extract(predsvm,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
extractedrf_rast<-extract(predrf,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
extractedrf_rast_4var<-extract(predrf_4var,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
extractedrf_rast_3var_nosrtm<-extract(predrf_3var_nosrtm,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
extractedrf_rast_3var_nondvi<-extract(predrf_3var_nondvi,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
extractrf8<-extract(pred_rf8,rainfall_shpfile_utm_cumu,method='simple',buffer=1000,fun=median)
rainfall_csv_cumu<-as.data.frame(read.csv("C:/Users/P K Choudhary/Desktop/Stat_dwnscal/cumulative/cumulative.csv"))
#####distance from center !!!!!!!!!!!!!!!!!!!dont use
distnce<-distanceFromPoints(predrf_4var,rainfall_shpfile_utm_cumu)
writeRaster(distnce,filename = "distance.tif",fomat="GTiff",overwrite=T)
#=============================================================================================================================
#error analysis
cor(rainfall_csv_cumu$sum,extractedlm_rast)
cor(rainfall_csv_cumu$sum,extractedsvm_rast)
cor(rainfall_csv_cumu$sum,extractedrf_rast)
cor(rainfall_csv_cumu$sum,extractedrf_rast_4var)

mae(rainfall_csv_cumu$sum,extractedlm_rast)
mae(rainfall_csv_cumu$sum,extractedsvm_rast)
mae(rainfall_csv_cumu$sum,extractedrf_rast)
mae(rainfall_csv_cumu$sum,extractedrf_rast_4var)

rmse(rainfall_csv_cumu$sum,extractedlm_rast)
rmse(rainfall_csv_cumu$sum,extractedsvm_rast)
rmse(rainfall_csv_cumu$sum,extractedrf_rast)
rmse(rainfall_csv_cumu$sum,extractedrf_rast_4var)
rmse(rainfall_csv_cumu$sum,extractedrf_rast_3var_nosrtm)
rmse(rainfall_csv_cumu$sum,extractedrf_rast_3var_nondvi)
rmse(rainfall_csv_cumu$sum,extractrf8)


#================================================================================================================================
#checking correlation and rmse and mape for chirps data and rain gauge data
setwd("C://Users/P K Choudhary/Desktop/Stat_dwnscal/chirps_daily_2015_utm/cumulative/")
chirps_ras<-raster("sum_ras.tif")
extracted_rast_chirp<-extract(chirps_ras,rainfall_shpfile_utm_cumu,method='simple',buffer=5000,fun=median)
cor(rainfall_csv_cumu$sum,extracted_rast_chirp)
mae(rainfall_csv_cumu$sum,extracted_rast_chirp)
rmse(rainfall_csv_cumu$sum,extracted_rast_chirp)

rmse(extracted_rast_chirp,extractedrf_rast)
cor(extracted_rast_chirp,extractedrf_rast)

varImpPlot()

#various plots
r2_vect<-c(cor(rainfall_csv_cumu$sum,extractedrf_rast),cor(rainfall_csv_cumu$sum,extractedrf_rast_4var),cor(rainfall_csv_cumu$sum,extractedsvm_rast),cor(rainfall_csv_cumu$sum,extractedlm_rast))
mae_vect<-c(mae(rainfall_csv_cumu$sum,extractedrf_rast),mae(rainfall_csv_cumu$sum,extractedrf_rast_4var),mae(rainfall_csv_cumu$sum,extractedsvm_rast),mae(rainfall_csv_cumu$sum,extractedlm_rast))
rmse_vect<-c(rmse(rainfall_csv_cumu$sum,extractedrf_rast),rmse(rainfall_csv_cumu$sum,extractedrf_rast_4var),rmse(rainfall_csv_cumu$sum,extractedsvm_rast),rmse(rainfall_csv_cumu$sum,extractedlm_rast))

hist(mae_vect)
install.packages("car")
library(car)
rg_vect<-c(rainfall_csv_cumu[,5])
extractedlm_rast
extractedrf_rast
extractedrf_rast_4var
extractedsvm_rast
scatterplot(rg_vect,extractedrf_rast_4var,boxplots = F,smooth=F,xlab="Rain Gauge Data",ylab = "RF(4 variables)")
scatterplot(rg_vect,extractedrf_rast,boxplots = F,smooth=F,xlab="Rain Gauge Data",ylab = "RF(5 variables)")
scatterplot(rg_vect,extractedlm_rast,boxplots = F,smooth=F,xlab="Rain Gauge Data",ylab = "LM")
scatterplot(rg_vect,extractedsvm_rast,boxplots = F,smooth=F,xlab="Rain Gauge Data",ylab = "SVM")
scatterplot(rg_vect,extracted_rast_chirp,boxplots = F,smooth=F,xlab="Rain Gauge Data",ylab = "CHIRP")

library(EnvStats)
cdfPlot(param.list = list(mean=mean(rg_vect),sd = sd(rg_vect)),ylab = "CDF",xlab = "Annual Precipitation (mm)",main = "Obs VS RF(4 variables)",cdf.col = "blue")
cdfPlot(param.list = list(mean=mean(extractedrf_rast_4var),sd = sd(extractedrf_rast_4var)),add = T,cdf.col ="red")
legend("bottomright",c("Obs","RF(4 variables)"),fill=c("blue","red"))

cdfPlot(param.list = list(mean=mean(rg_vect),sd = sd(rg_vect)),ylab = "CDF",xlab = "Annual Precipitation (mm)",main = "Obs VS RF(5 variables)",cdf.col = "blue")
cdfPlot(param.list = list(mean=mean(extractedrf_rast),sd = sd(extractedrf_rast)),add = T,cdf.col ="red")
legend("bottomright",c("Obs","RF(5 variables)"),fill=c("blue","red"))

cdfPlot(param.list = list(mean=mean(rg_vect),sd = sd(rg_vect)),ylab = "CDF",xlab = "Annual Precipitation (mm)",main = "Obs VS LM",cdf.col = "blue")
cdfPlot(param.list = list(mean=mean(extractedlm_rast),sd = sd(extractedlm_rast)),add = T,cdf.col ="red")
legend("bottomright",c("Obs","LM"),fill=c("blue","red"))

cdfPlot(param.list = list(mean=mean(rg_vect),sd = sd(rg_vect)),ylab = "CDF",xlab = "Annual Precipitation (mm)",main = "Obs VS SVM",cdf.col = "blue")
cdfPlot(param.list = list(mean=mean(extractedsvm_rast),sd = sd(extractedsvm_rast)),add = T,cdf.col ="red")
legend("bottomright",c("Obs","SVM"),fill=c("blue","red"))

cdfPlot(param.list = list(mean=mean(rg_vect),sd = sd(rg_vect)),ylab = "CDF",xlab = "Annual Precipitation (mm)",main = "Obs VS CHIRP",cdf.col = "blue")
cdfPlot(param.list = list(mean=mean(extracted_rast_chirp),sd = sd(extracted_rast_chirp)),add = T,cdf.col ="red")
legend("bottomright",c("Obs","CHIRP"),fill=c("blue","red"))

varImpPlot(rforest4,main="RF(5 variables)")
varImpPlot(rforest5,main = "RF(4 variables)")
