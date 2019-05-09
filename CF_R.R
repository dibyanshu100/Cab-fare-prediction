#Remove all objects stored
rm(list=ls())

#Setting working directory
setwd("D:\\edwisor\\Cab_Fare")

#loading libraries
library(ggplot2)
library(corrgram)
library(DMwR)
library(caret)
library(randomForest)
library(gbm)
library(xlsx)
library(DataCombine)

#loading the data
train_data = read.csv("train_cab.csv")
test_data = read.csv("test.csv")

#first 5 columns of train_data
head(train_data,5)

#Shape of train data
dim(train_data)

#shape of test data
dim(test_data)

#first five columns of test data
head(test_data)

#Converting data type of fare amount to float
train_data$fare_amount= as.numeric(as.character(train_data$fare_amount))

##--------------------------------MISSING VALUE ANALYSIS-------------------------------------------------------------

missing_val = data.frame(apply(train_data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train_data)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

#deleting the rows containing NA
train_data=na.omit(train_data)
rownames(train_data) = seq(length=nrow(train_data)) 

##---------------------------------OUTLIER ANALYSIS----------------------------------------------------------

#Box plot of continuous data
#Distance from Residence to Work

#dropoff_longitude
boxplot(train_data$dropoff_longitude, data = train_data, ylab = "dropoff_longitude")

#dropoff_latitude
boxplot(train_data$dropoff_latitude, data = train_data, ylab = "dropoff_latitude")

#pickup_longitude
boxplot(train_data$pickup_longitude, data = train_data, ylab = "pickup_longitude")

#pickup_latitude
boxplot(train_data$pickup_latitude, data = train_data, ylab = "pickup_latitude")

#fare_amount
boxplot(train_data$fare_amount, data = train_data, ylab = "fare_amount")

#Replace all outliers with appropriate values
con= c('fare_amount', 'pickup_longitude', 'pickup_latitude',
       'dropoff_longitude', 'dropoff_latitude')
for(i in con)
{
  maxm=array()
  minm=array()
  print(i)
  k=1
  l=1
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  
  for(j in (1:length(val)))
  {
    if( val[j]>boxplot.stats(train_data[,i])$stats[5] )
    {
      maxm[l]=val[j]
      l=l+1
    }
    if( val[j]<boxplot.stats(train_data[,i])$stats[1] )
    {
      minm[k]=val[j]
      k=k+1
    }
    
  }
  train_data[,i][train_data[,i] %in% maxm] = boxplot.stats(train_data[,i])$stats[5]
  train_data[,i][train_data[,i] %in% minm] = boxplot.stats(train_data[,i])$stats[1]

}

#Rechecking after removing outliers
for(i in con)
{
  val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
  print(length(val))
}

#for fare amount less than 0
for (i in (1:15987))
{  
  if(train_data$fare_amount[i]<0)
  train_data$fare_amount[i]=0.01
}

#Passenger_count
train_data$passenger_count=as.integer(train_data$passenger_count)

for (i in (1:15987))
{  
  if(train_data$passenger_count[i]>6)
    train_data$passenger_count[i]=6
  
  if(train_data$passenger_count[i]==0)
    train_data$passenger_count[i]=1
}

##-------------------------------Feature Selection---------------------------------------------------

install.packages("purrr")
install.packages("geosphere")
install.packages("rlist")
install.packages("VIF")

library(purrr)
library(geosphere)
library(rlist)
library(VIF)

#adding a new feature distance
get_geo_distance = function(long1, lat1, long2, lat2, units = "km") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

for(i in (1:15987))
{
  train_data$distance[i]= get_geo_distance(train_data$pickup_longitude[i],train_data$pickup_latitude[i],train_data$dropoff_longitude[i],train_data$dropoff_latitude[i])
}

con= c('fare_amount', 'pickup_longitude', 'pickup_latitude',
       'dropoff_longitude', 'dropoff_latitude','distance')

#VIF
model= lm(fare_amount ~. ,data= train_data[,-2])
vif(model)

#Correlation Plot 
corrgram(train_data[,con], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

##---------------------------------EDA-----------------------------------------------------------

#plot between fare and passenger count
ggplot(train_data, aes_string(x=train_data$passenger_count, y=train_data$fare_amount))+
  geom_bar(stat="summary",fill =  "DarkSlateBlue") + theme_bw() +
  xlab('passenger_count')+ylab("fare")+  
  theme(text=element_text(size=15))

#count plot for passengers
ggplot(train_data, aes_string(x = train_data$passenger_count)) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("no of passengers") + ylab('Count')

#histogram of different variables

#fare amount
ggplot(train_data, aes_string(x = train_data$fare_amount)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("fare_amount") + ylab("Frequency")
 
#pickup latitude
ggplot(train_data, aes_string(x = train_data$pickup_latitude)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("pickup latitude") + ylab("Frequency")

#pickup longitude
ggplot(train_data, aes_string(x = train_data$pickup_longitude)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("pickup longitude") + ylab("Frequency")

#dropoff latitude
ggplot(train_data, aes_string(x = train_data$dropoff_latitude)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("dropoff latitude") + ylab("Frequency")

#dropoff longitude
ggplot(train_data, aes_string(x = train_data$dropoff_longitude)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("dropoff longitude") + ylab("Frequency")

#distance
ggplot(train_data, aes_string(x = train_data$distance)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")

#Scatter plot between distance and fare
ggplot(train_data, aes(x = fare_amount, y =distance))+geom_point()

##----------------------------------Feature Scaling----------------------------------------------

#Standardisation
for(i in con)
{
  train_data[,i] = (train_data[,i] - mean(train_data[,i]))/sd(train_data[,i])
}

##-----------------------------------Modelling--------------------------------------------

#Divide data into train and test using stratified sampling method
set.seed(123)
train.index = sample(1:nrow(train_data), 0.8 * nrow(train_data))
train = train_data[ train.index,-2]
test  = train_data[-train.index,-2]

##Linear Regression
LR = lm(fare_amount ~ ., data = train)
#predicting for test data
y_pred = predict(LR,test[,names(test) != "fare_amount"])
# For testing data 
print(postResample(pred = y_pred, obs =test$fare_amount))

##RMSE= 0.5810
##Rsquare= 0.6609


##Decision Tree
#Develop Model on training data
DT = rpart(fare_amount ~., data = train, method = "anova")
#predicting for test data
y_pred = predict(DT,test)
# For testing data 
print(postResample(pred = y_pred, obs = test$fare_amount))

##RMSE= 0.5554
##Rsquare= 0.6898


##Random Forest
RF = randomForest(fare_amount~., data = train)
#predicting for test data
y_pred = predict(RF,test)
# For testing data
print(postResample(pred = y_pred, obs = test$fare_amount))

##RMSE= 0.49
##Rsquare= 0.75


##final Prediction for test data
#creating distance column for test data
for(i in (1:9914))
{
  test_data$distance[i]= get_geo_distance(test_data$pickup_longitude[i],test_data$pickup_latitude[i],test_data$dropoff_longitude[i],test_data$dropoff_latitude[i])
}
for(i in con)
{
  train_data[,i] = (train_data[,i] - mean(train_data[,i]))/sd(train_data[,i])
}

#scaling the test data
con= c('pickup_longitude', 'pickup_latitude','passenger_count',
       'dropoff_longitude', 'dropoff_latitude','distance')
for(i in con)
{
  test_data[,i] = (test_data[,i] - mean(test_data[,i]))/sd(test_data[,i])
}
test = test_data[,-1]

#prediction
prediction= predict(RF,test)

