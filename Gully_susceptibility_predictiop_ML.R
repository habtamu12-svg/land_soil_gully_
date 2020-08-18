library(rgdal)
library(raster)
library(caret)
library(sp)
library(e1071)
library(sp)
library(RStoolbox)
library(rgeos)
library(maptools)
library(gdalUtils)
library(ROCR)
library(randomForest)
library(pROC) 
library(gplots)
library(sf)
package:raster
##Seed is to get the same result at every run
options(max.print=1000000000)

## New_data_new _approach --------------------------------------------------
setwd("D:/selam_RF_R")
### calling  response and predictor variables 
training_data_new_updated<-read.csv("D:/selam_RF_R/AllinputGulNoGulFit.csv")
head(training_data_new_updated)
training_data_new1<-training_data_new_updated[2:20]
head(training_data_new1)
##checking how well each real world class represented in the sample
presence<-subset(training_data_new1,training_data_new1$Code==1)
abscnce_no_guly<-subset(training_data_new1,training_data_new1$Code==0)
nrow(abscnce_no_guly)
## predictor variables 
predictor_variables<-list.files(path="D:/selam_RF_R/RawData/RawData", 
                          pattern =".tif$", full.names=TRUE)
soil_var<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Soiltype_rec.tif")
aspect_var<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Aspect_recl1.tif")
elevation_var<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Elevation1.tif")
CI_var<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\CI_reclass.tif")
DD<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\DD_reclass.tif")
DGW<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\dGW_reclass.tif")
TWI<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\TWI_reclass.tif")
SpI<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\SPI_reclass.tif")
land_cover<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Landcover_reclass.tif")
prof_cur<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Profile_cur1.tif")
plan_cur<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\PlanCuv1.tif")
slope<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Slope_class.tif")
LS<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\LS_reclass.tif")
TPI<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\TPI_reclass.tif")
texture<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\Testure_reclass.tif")
TRI<-raster("D:\\Selamawit_netherland\\All dataset\\Habtamu\\selamawit__R_randomforest\\selamawit__R_randomforest\\TRI_reclass.tif")
## bringng predictor varibales together
Covariate_final <-addLayer(soil_var,aspect_var,elevation_var,CI_var,DD,DGW,TWI,SpI,land_cover,prof_cur,plan_cur,slope,LS,TPI,texture,TRI)
##spatailization of  the no_spatial traing data 
##create cRS
Coordsys<-crs(Covariate_final)
training_data_new12<-SpatialPointsDataFrame(training_data_new1[,18:19],training_data_new1,proj4string=Coordsys)
head(training_data_new12)
training_data_new13<-training_data_new12[-18:-19]
head(training_data_new13)
## extracting value for each co-varietes at the location of the training point
  extract_value2<-as.data.frame(extract(Covariate_final,training_data_new13
                                 ,df=TRUE))
  head(extract_value2)
## row -colum match
training_data_new13@data=data.frame(training_data_new13@data,extract_value2[match(rownames(training_data_new13@data),rownames(extract_value2)),])
head(training_data_new13@data)
## avoiding un necesary columns 
training_data_final<-training_data_new13@data[,-2:-18]

head(training_data_final)

## multi_colinearity test
library(rfUtilities)

cl<- multi.collinear(training_data_final[,2:ncol(training_data_final)], p=0.05)
cl
##we did not find co-linear varaibale 

##Random Forests Model

##We will now coerce the dependent variable (y) into factorial vector and calculate the percent of the positive (1) class 
#to check for the sample balance (zero inflation issues) in the model.  

(dim(training_data_final[training_data_final$Code == 1, ])[1] / dim(training_data_final)[1] ) * 100
##We observe that the sample balance of presence locations is 31% thus, meeting the 1/3 rule for sample balance.

#3Data split into training and testing
library(caret)
Train2 <- createDataPartition(training_data_final$Code,p=0.7, list=FALSE)
training1 <-training_data_final[ Train2, ]
testing1 <- training_data_final[ -Train2, ]

head(training1)
 tail(training1)
##model fit
set.seed(1111)
Modelfit.rf4<-randomForest(x = training1[,2:ncol(training1)],
                          y =as.factor(training1$Code),proximity=TRUE,type="prob",importance=TRUE,ntree=500,
                          norm.votes=TRUE)
Modelfit.rf4
##variable importance estimation 
varImpPlot(Modelfit.rf4)

##Model Estimation (probability raster creation)
P2<-predict(Covariate_final, Modelfit.rf4, "model.probs.tif1", type="prob", index=2, na.rm=TRUE, overwrite=TRUE, progress="text",format="GTiff")

plot(P2)

#######Confusion matrix

prediction4<-predict(Modelfit.rf4, testing1)

CM1<-confusionMatrix(prediction4,as.factor(testing1$Code))
CM1

##Calculate area under curve

pr<-predict(Modelfit.rf4, testing1)

tree.probs=predict(Modelfit.rf4,
                   newdata=testing1,
                   type="prob")
head(tree.probs)
rocCurve.tree <- roc(testing1$Code,tree.probs[,"1"])
auc(rocCurve.tree)
plot(rocCurve.tree)

------------------------------------------------------------------------------------------------------------------------------------
  library(rgdal)
library(raster)
library(caret)
library(e1071)
library(sp)
library(RStoolbox)
library(rgeos)
library(maptools)
library(gdalUtils)
library(ROCR)
library(randomForest)
library(pROC) 

##Seed is to get the same result at every run
set.seed(1235)

##Read data

data<-read.csv("D:\\selam_RF_R\\Input_Code (1)\\data.csv" , header = TRUE, sep = ",")

##Getting the variables (the response and the predictors) and transform the categorical variables to factor

for(i in 2:6){
  
  data[,i] <- as.factor(data[,i])
  
}
data[,13] <- as.factor(data[,13])
YXs<-data[,2:17]
sapply(YXs, class)
##Check for 1/3 rule
(dim(data[data$Code == 1, ])[1] / dim(data)[1] ) * 100
##Data split into training and testing
Train1 <- createDataPartition(YXs$Code, p=0.7, list=FALSE)
training <- YXs[ Train1, ]
testing <- YXs[ -Train1, ]
## Fitting the model, tuning can be done here
modFit.rf <- randomForest::randomForest(Code ~ ., data = training)
##Confusion matrix
prediction <-predict(modFit.rf, testing)
cm<-confusionMatrix(prediction, testing$Code)
cm
##Calculate area under curve
pr<-predict(modFit.rf, testing)
tree.probs=predict(modFit.rf,
                   newdata=testing,
                   type="prob")
head(tree.probs)
rocCurve.tree <- roc(testing$Code,tree.probs[,"1"])
auc(rocCurve.tree)
##Variable Importance
importance(modFit.rf)
varImpPlot(modFit.rf)



