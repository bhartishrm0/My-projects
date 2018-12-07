

#---------------------------------------------------Problem - Bike Renting--------------------------------------------------------



# Load Libraries --
rm(list=ls(all=T))
setwd("/Users/bhartisharma/Desktop/Bike Renting")
# Load Libraries --
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)



#--------------------------------------Data


#Load Data
dataset = read.csv("day.csv")

#Converting dataset into a dataframe
dataset<-as.data.frame(dataset)
View(dataset)

#Checking the default datatypes of dataset
str(dataset)

#statistics of dataset
summary(dataset)



#--------------------------------Exploratory data analysis



#Creating two different dataframes DataCat and DataCon which contains categorical and continuos variables respectively as explained in the report
#DataCat is a dataframe containing all categorical variables from dataset
DataCat = data.frame()
DataCat = subset(dataset, select = c('instant','dteday', 'season', 'yr', 'mnth', 'holiday', 'weekday','workingday', 'weathersit'))
View(DataCat)

#DataCon is a dataframe containing all continuous variables from dataset
DataCon = data.frame()
DataCon = subset(dataset , select = c('temp', 'atemp', 'hum', 'windspeed','casual', 'registered', 'cnt') )
View(DataCat)



#weathersit 
#weathersit: As per problem statement
#1: Clear, Few clouds, Partly cloudy, Partly cloudy
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('weathersit')] == 1)
  {
    dataset[i,('weathersit')] = 'Partly cloudy'
  }
    if(dataset[i,('weathersit')] == 2)
    {
      dataset[i,('weathersit')] = 'cloudy'
    }
    if(dataset[i,('weathersit')] == 3)
    {
      dataset[i,('weathersit')] = 'Light Rain'
    }
    if(dataset[i,('weathersit')] == 4)
    {
      dataset[i,('weathersit')] = 'Heavy Rain'
    }
  
}


#Year (0: 2011, 1:2012)
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('yr')] == 0)
  {
    dataset[i,('yr')] = '2011'
  }
  else{
    if(dataset[i,('yr')] == 1)
    {
      dataset[i,('yr')] = '2012'
    }}
}

#Seasons (1:springer, 2:summer, 3:fall, 4:winter)
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('season')] == 1)
  {
    dataset[i,('season')] = "springer"
  }
  if(dataset[i,('season')] == 2)
  {
    dataset[i,('season')] = 'summer'
  }
  if(dataset[i,('season')] == 3)
  {
    dataset[i,('season')] = 'fall'
  }
  if(dataset[i,('season')] == 4)
  {
    dataset[i,('season')] = 'winter'
  }
  
}

#mnth: Month (1 to 12)
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('mnth')] == 1)
  {
  dataset[i,('mnth')] = 'January'
  }
  if(dataset[i,('mnth')] == 2)
  {
    dataset[i,('mnth')] = 'February'
  }
  if(dataset[i,('mnth')] == 3)
  {
    dataset[i,('mnth')] = 'March'
  }
  if(dataset[i,('mnth')] == 4)
  {
    dataset[i,('mnth')] = 'April'
  }
  if(dataset[i,('mnth')] == 5)
  {
    dataset[i,('mnth')] = 'May'
  }
  if(dataset[i,('mnth')] == 6)
  {
    dataset[i,('mnth')] = 'June'
  }
  if(dataset[i,('mnth')] == 7)
  {
    dataset[i,('mnth')] = 'July'
  }
  if(dataset[i,('mnth')] == 8)
  {
    dataset[i,('mnth')] = 'August'
  }
  if(dataset[i,('mnth')] == 9)
  {
    dataset[i,('mnth')] = 'September'
  }
  if(dataset[i,('mnth')] == 10)
  {
    dataset[i,('mnth')] = 'October'
  }
  if(dataset[i,('mnth')] == 11)
  {
    dataset[i,('mnth')] = 'November'
  }
  if(dataset[i,('mnth')] == 12)
  {
    dataset[i,('mnth')] = 'December'
  }
}
  
#holiday: weather day is holiday or not (extracted fromHoliday Schedule)
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('holiday')] == 0)
  {
    dataset[i,('holiday')] = 'No'
  }
  else{
    if(dataset[i,('holiday')] == 1)
    {
      dataset[i,('holiday')] = 'Yes'
    }}
}

#weekday: Day of the week
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('weekday')] == 1)
  {
    dataset[i,('weekday')] = 'Monday'
  }
  if(dataset[i,('weekday')] == 2)
  {
    dataset[i,('weekday')] = 'Tuesday'
  }
  if(dataset[i,('weekday')] == 3)
  {
    dataset[i,('weekday')] = 'Wednesday'
  }
  if(dataset[i,('weekday')] == 4)
  {
    dataset[i,('weekday')] = 'Thursday'
  }
  if(dataset[i,('weekday')] == 5)
  {
    dataset[i,('weekday')] = 'Friday'
  }
  if(dataset[i,('weekday')] == 6)
  {
    dataset[i,('weekday')] = 'Saturday'
  }
  if(dataset[i,('weekday')] == 0)
  {
    dataset[i,('weekday')] = 'Sunday'
  }
}

#workingday: If day is neither weekend nor holiday is 1, otherwise is 0.
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('workingday')] == 0)
  {
    dataset[i,('workingday')] = 'No'
  }
  else{
    if(dataset[i,('workingday')] == 1)
    {
      dataset[i,('workingday')] = 'Yes'
    }}
}

#Continuous data is already in float and int type , But we have to convert categorical variables to object type
#Converting some integer and float variables to categorical variable as per requirement

for (i in colnames(DataCat))
{ print(i)
  dataset[,i] = as.factor(dataset[,i])
}

#Checking the converted datatypes of dataset
str(dataset)
View(dataset)

#temp: Normalized temperature in Celsius. 
#The values are derived via (t-t_min)/(t_max-t_min),t_min=-8, t_max=+39 (only in hourly scale)
t_min=-8
t_max=+39
ActualTemp = data.frame() 
ActualTemp = (dataset['temp']*(t_max - t_min) ) + t_min
dataset$ActualTemp = ActualTemp

#atemp: Normalized feeling temperature in Celsius. 
#The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
t_min=-16
t_max=+50
ActualAtemp =data.frame()  
ActualAtemp = (dataset['atemp']*(t_max - t_min) ) + t_min
dataset$ActualAtemp = ActualAtemp


#hum: Normalized humidity. The values are divided to 100 (max)
Actualhum = data.frame()
Actualhum = dataset$hum* 100
dataset$Actualhum = Actualhum

#windspeed: Normalized wind speed. The values are divided to 67 (max)
Actualwindspeed = data.frame()
Actualwindspeed= dataset$windspeed * 67
dataset$Actualwindspeed = Actualwindspeed

#After data exploratory analysis View dataset
View(dataset)

#DataCon is a dataframe containing all continuous variables from dataset 
#Here we are adding actual values for ActualTemp and ActualAtemp which we have calculated using given formula in problem statement
#Since we will use actual values for these variable in feature analysis and outier analysis to get accurate results
DataConActual = data.frame(dataset['atemp'])
DataConActual$ActualTemp = dataset$ActualTemp
DataConActual$ActualAtemp = dataset$ActualAtemp
DataConActual$Actualhum = dataset$Actualhum
DataConActual$Actualwindspeed = dataset$Actualwindspeed 
DataConActual['atemp'] = NULL
View(DataConActual)



#-------------------------------------------------------------Data PreProcessing



#---------------Missing Values

missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
View(missing_val)



#----------------Outlier Analysis

#BoxPlots - Distribution and Outlier Check
cnames = colnames(DataConActual)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot  for",cnames[i])))
}
#Viewing plots
gn1
gn2
gn3
gn4


#Remove outliers using boxplot method
df = dataset

#loop to remove from all variables
for(i in cnames){
  #print(i)
  val = dataset[,i][dataset[,i] %in% boxplot.stats(dataset[,i])$out]
  #print(length(val))
  dataset = dataset[which(!dataset[,i] %in% val),]
}

#Checking for outliers after outlier removal
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot  for",cnames[i])))
}

#Viewing plots
gn1
gn2
gn3
gn4


#----------------------------------------Feature Selection


#--------------for continuous data

# Correlation Plot for continuous data
cnames = colnames(DataConActual)
corrgram(dataset[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Dimension Reduction
dataset = subset(dataset, select = -c(atemp))



#-----------------For categorical 

## Chi-square Test for categorical variables
#1 Comparing  "season"     "yr"         "mnth"       "holiday"    "weekday"    "workingday" "weathersit"
p1 = chisq.test(table(DataCat$season,DataCat$weathersit))
p2 = chisq.test(table(DataCat$season,DataCat$yr))
p3 = chisq.test(table(DataCat$season,DataCat$mnth))
p4 = chisq.test(table(DataCat$season,DataCat$holiday))
p5 = chisq.test(table(DataCat$season,DataCat$weekday))
p6 = chisq.test(table(DataCat$season,DataCat$workingday))


##If p-value < 0.05: significant result, reject null hypothesis (H0), dependent.
#If independent variables are dependent to each other we can drop one of them .
#If p-value > 0.05: not significant result, fail to reject null hypothesis (H0), independent.
p1$p.value  < 0.05
p2$p.value  < 0.05
p3$p.value  < 0.05
p4$p.value  < 0.05
p5$p.value  < 0.05
p6$p.value  < 0.05


#Here we got p1 and p3 , which are less than 0.05 .If independent variables are dependent to each other we can drop one of them .
dataset$weathersit = NULL
dataset$mnth = NULL


#2  As we have compared 'season' with all variables,So we will not compare again
#Comparing "yr" "holiday"    "weekday"    "workingday" 

p1 = chisq.test(table(DataCat$yr,DataCat$holiday))
p2 = chisq.test(table(DataCat$yr,DataCat$weekday))
p3 = chisq.test(table(DataCat$yr,DataCat$workingday))


##If p-value < 0.05: significant result, reject null hypothesis (H0), dependent.
#If independent variables are dependent to each other we can drop one of them .
#If p-value > 0.05: not significant result, fail to reject null hypothesis (H0), independent.
p1$p.value  < 0.05
p2$p.value  < 0.05
p3$p.value  < 0.05


#3  As we have compared 'yr' with all variables,So we will not compare again
#Comparing "holiday"    "weekday"    "workingday" 

p1 = chisq.test(table(DataCat$workingday,DataCat$weekday))
p2 = chisq.test(table(DataCat$holiday,DataCat$weekday))
p3=  chisq.test(table(DataCat$holiday,DataCat$workingday))

##If p-value < 0.05: significant result, reject null hypothesis (H0), dependent.
#If independent variables are dependent to each other we can drop one of them .
#If p-value > 0.05: not significant result, fail to reject null hypothesis (H0), independent.
p1$p.value  < 0.05
p2$p.value  < 0.05
p3$p.value  < 0.05

dataset$weekday = NULL

#Dropping extra variables which have been created for transforming normalised data 
dataset$ActualAtemp= NULL
dataset$ActualTemp= NULL
dataset$Actualhum= NULL
dataset$Actualwindspeed= NULL
dataset$instant = NULL
dataset$dteday = NULL

#Dataset after feature Selection
View(dataset)

#-------------------------------Feature Scaling

#Since we already have continuous data given in normalised form .
#We are passing the same data to our model.



#------------------------------------------------------------Model Development----------------------------------------------





#---------------------------------------Linear Regression Model for 'Casual'

#Sampling
#Divide the data into train and test
#y = DependentVariables$dataset.casual
dataset_casual = dataset
dataset_casual$registered = NULL
dataset_casual$cnt = NULL
View(dataset_casual)
TrainData_index = sample(1:nrow(dataset_casual), 0.8 * nrow(dataset_casual))
TrainData = dataset_casual[TrainData_index,]
TestData = dataset_casual[-TrainData_index,]

#run regression model
lm_model = lm(casual ~., data = TrainData)
#Summary of the model
summary(lm_model)

#Predict
predictions_LR_casual = predict(lm_model, TestData[,-8])
#predictions_LR_casual

#As from summary we can see negative values in predictions 
#Replacing negative predictions with 1 ,Since count can't be negative for bikes rented
predictions_LR_casual[predictions_LR_casual  < 0] = 1
predictions_LR_casual[predictions_LR_casual  < 0]

#Calculate MAE
MAE( predictions_LR_casual,TestData[,8])
#Calculate RMSE
RMSE(predictions_LR_casual,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$casual
  y.pred <- predictions_LR_casual
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y +1))^2)))
}
rmlse(lm_model)




#-------------------------------------------Linear Regression Model for 'registered'

#Sampling
#Divide the data into train and test
#y = DependentVariables$dataset.casual
dataset_registered = dataset
dataset_registered$casual = NULL
dataset_registered$cnt = NULL
View(dataset_registered)
TrainData_index = sample(1:nrow(dataset_registered), 0.8 * nrow(dataset_registered))
TrainData = dataset_registered[TrainData_index,]
TestData = dataset_registered[-TrainData_index,]

#run regression model
lm_model = lm(registered ~., data = TrainData)
#Summary of the model
summary(lm_model)

#Predict
predictions_LR_registered = predict(lm_model, TestData[,-8])
#predictions_LR_casual

#As from summary we can see negative values in predictions 
#Replacing negative predictions with 1 ,Since count can't be negative for bikes rented
predictions_LR_registered[predictions_LR_registered  < 0] = 1


#Calculate MAE
MAE( predictions_LR_registered,TestData[,8])
#Calculate RMSE
RMSE(predictions_LR_registered,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$registered
  y.pred <- predictions_LR_registered
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y +1))^2)))
}
rmlse(lm_model)




#------------------------------------------Decision Tree Regression Model for 'casual'
#Sampling
#Divide the data into train and test

TrainData_index = sample(1:nrow(dataset_casual), 0.8 * nrow(dataset_casual))
TrainData = dataset_casual[TrainData_index,]
TestData = dataset_casual[-TrainData_index,]

#Load Libraries
library(rpart)
#rpart for regression
Model_DT_casual = rpart(casual ~ ., data = TrainData, method = "anova")
#Predict for new test cases
predictions_DT_casual = predict(Model_DT_casual, TestData[,-8])

#calculate MAE
MAE(predictions_DT_casual,TestData[,8])
#Calculate RMSE
RMSE(predictions_DT_casual,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$casual
  y.pred <- predictions_DT_casual
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y +1))^2)))
}
rmlse(Model_DT_casual)




#--------------------------------------------Decision Tree Regression Model for 'registered'
#Sampling
#Divide the data into train and test

TrainData_index = sample(1:nrow(dataset_registered), 0.8 * nrow(dataset_registered))
TrainData = dataset_registered[TrainData_index,]
TestData = dataset_registered[-TrainData_index,]

#Load Libraries
library(rpart)
#rpart for regression
Model_DT_registered = rpart(registered ~ ., data = TrainData, method = "anova")
#Predict for new test cases
predictions_DT_registered = predict(Model_DT_registered, TestData[,-8])

#calculate MAE
MAE(predictions_DT_registered,TestData[,8])
#Calculate RMSE
RMSE(predictions_DT_registered,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$registered
  y.pred <- predictions_DT_registered
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y+1))^2)))
}
rmlse(Model_DT_registered)




#---------------------------------------------Random Forest Regression Model for casual
#Sampling
#Divide the data into train and test

TrainData_index = sample(1:nrow(dataset_casual), 0.8 * nrow(dataset_casual))
TrainData = dataset_casual[TrainData_index,]
TestData = dataset_casual[-TrainData_index,]

Model_RF_casual = randomForest(formula = casual ~ ., data = TrainData, ntree = 45, method = "anova")
#Predict for new test cases
predictions_RF_casual = predict(Model_RF_casual, TestData[,-8])

#calculate MAE
MAE(predictions_RF_casual,TestData[,8])
#Calculate RMSE
RMSE(predictions_RF_casual,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$casual
  y.pred <- predictions_RF_casual
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y+1))^2)))
}
rmlse(Model_RF_casual)


#-----------------------------------------------Random Forest Regression Model for registered
#Sampling
#Divide the data into train and test

TrainData_index = sample(1:nrow(dataset_registered), 0.8 * nrow(dataset_registered))
TrainData = dataset_registered[TrainData_index,]
TestData = dataset_registered[-TrainData_index,]

Model_RF_registered = randomForest(formula = registered ~ ., data = TrainData, ntree = 45, method = "anova")
#Predict for new test cases
predictions_RF_registered = predict(Model_RF_registered, TestData[,-8])

#calculate MAE
MAE(predictions_RF_registered,TestData[,8])
#Calculate RMSE
RMSE(predictions_RF_registered,TestData[,8])

#Calculate RMSLE
rmlse <- function(model) { 
  y <- TestData$registered
  y.pred <- predictions_RF_registered
  return(sqrt(1/length(y)*sum((log(y.pred +1)-log(y+1))^2)))
}
rmlse(Model_RF_registered)


#------------------------------------------------------------Final Model-----------------------------------------



#We are freezing Linear Regression model 
#As we have observed ERROR MESEARE rates for Linear Regression model are low as compared to other Models.
#As per problem statement given cnt = casual + Registered
#cnt: count of total rental bikes including both casual and registered
#We are calculating predicted cnt with the sum of predicted values of registered and casual variables.
#Applying Linear Regression on entire DataSet


#-------------------Predicting casual


#run regression model
lm_model = lm(casual ~., data = dataset_casual)

#Predict
predictions_LR_casual = predict(lm_model, dataset_casual[,-8])
predictions_LR_casual

#As from summary we can see negative values in predictions 
#Replacing negative predictions with 1 ,Since count can't be negative for bikes rented
predictions_LR_casual[predictions_LR_casual  < 0] = 1


#----------------------Predicting registered
#run regression model
lm_model = lm(registered ~., data = dataset_registered)

#Predict
predictions_LR_registered = predict(lm_model, dataset_registered[,-8])
#predictions_LR_registered

#As from summary we can see negative values in predictions 
#Replacing negative predictions with 1 ,Since count can't be negative for bikes rented
predictions_LR_registered[predictions_LR_registered  < 0] = 1


#As per problem statement given cnt = casual + Registered
#cnt: count of total rental bikes including both casual and registered
#We are calculating predicted cnt with the sum of predicted values of registered and casual variables.

#Final Count
Count_Prediction = round(predictions_LR_registered) + round(predictions_LR_casual)


#Creating output file for dataset
DataTestFinal= read.csv("Day.csv")
df_output = data.frame(DataTestFinal)
df_output['predicted_casual'] = round(predictions_LR_casual)
df_output['predicted_registered'] = round(predictions_LR_registered)
df_output['Predicted_Count'] = Count_Prediction
write.csv(df_output, "Bike_Renting_R_Output.csv",row.names = F)





