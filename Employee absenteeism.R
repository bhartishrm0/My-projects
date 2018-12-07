rm(list=ls(all=T))
setwd("/Users/bhartisharma/Desktop/Employee Absenteeism")
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#Load Data
#install.packages("xlsx")
library(xlsx)
dataset = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex =1,header = T)


#Converting dataset into a dataframe
dataset<-as.data.frame(dataset)
View(dataset)

#Exploratory data analysis
#Checking the default datatypes of dataset
str(dataset)

#statistics of dataset
summary(dataset)

#Creating two different dataframes DataCat and DataCon which contains categorical and continuos variables respectively as explained in the report
DataCon = data.frame()
DataCon = subset(dataset, select = c("Transportation.expense","Distance.from.Residence.to.Work" ,"Service.time","Age","Work.load.Average.day.","Hit.target","Weight" ,"Height","Body.mass.index","Absenteeism.time.in.hours"))
View(DataCon)

DataCat = data.frame()
DataCat = subset(dataset , select = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Disciplinary.failure","Education","Son","Social.drinker","Social.smoker","Pet") )
View(DataCat)

#Dividing Categorical variables into two dataframes

#Variables like ID,Reason for absence,Day of the week ,Seasons ,Education. Since in this case 0 shows loss of information,So we will replace them with Nan values and then impute them
DataCat_1 = data.frame()
DataCat_1 = subset(DataCat , select = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Education'))
View(DataCat_1)

#Variables like 'Disciplinary failure','Son', 'Social drinker','Social smoker', 'Pet' include 0 as important information so we are keeping them in other dataset
DataCat_2 = data.frame()
DataCat_2 = subset(DataCat, select = c('Disciplinary.failure', 'Social.drinker','Social.smoker'))
View(DataCat_2)

#DataCat_1
#replace 0 with NA 
DataCat_1[DataCat_1 == 0] <- NA
View(DataCat_1)


##Assigning (-1) to all NA and INF values for temporary basis for float datattype conversion to int data type. Missing value data will be handled as per process under the data preprocessing
dataset[is.na(dataset)] = -1
View(dataset)

#Missing Values
#missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
#missing_val


#Converting required float variables to integer
for (i in colnames(DataCon))
  { print(i)
  dataset[,i] = as.integer(dataset[,i])
}

#Checking the converted datatypes of dataset
str(dataset)

#categorising given data for  variable 'Reason for absence' in two categories as per problem statement
#dataset$Reason.for.absence[is.na(dataset$Reason.for.absence)] = -1

for (i in c(1,2:nrow(dataset)))
{ 

 if(dataset[i,'Reason.for.absence'] > 0)
   {
    if(dataset[i,('Reason.for.absence')] <= 21)  
      {
      dataset[i,('Reason.for.absence')] = 'ICD'
      }
   else
   {
    if(dataset[i,('Reason.for.absence')] >= 22)
    {
      dataset[i,('Reason.for.absence')] = 'Not ICD'
    }
   }
  }
}

View(dataset['Reason.for.absence'])

#Categorising given data for Disciplinary failure , Social drinker , Social smoker 


for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Disciplinary.failure')] == 0)
  {
  dataset[i,('Disciplinary.failure')] = 'No'
  }
  else{
  if(dataset[i,('Disciplinary.failure')] == 1)
  {
  dataset[i,('Disciplinary.failure')] = 'Yes'
  }}
}

for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Social.drinker')] == 0)
  {
  dataset[i,('Social.drinker')] = 'No'
  }
  else{
  if(dataset[i,('Social.drinker')] == 1)
  {
  dataset[i,('Social.drinker')] = 'Yes'
  }}
}

for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Social.smoker')] == 0)
  {
    dataset[i,('Social.smoker')] = 'No'
  }
  else{
  if(dataset[i,('Social.smoker')] == 1)
  {
    dataset[i,('Social.smoker')] = 'Yes'
  }}
}

View(dataset)

#Day of the week (Monday (2), Tuesday (3), Wednesday (4), Thursday (5), Friday (6))

for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Day.of.the.week')] == 2)
  {
  dataset[i,('Day.of.the.week')] = 'Monday'
  }
  if(dataset[i,('Day.of.the.week')] == 3)
  {
  dataset[i,('Day.of.the.week')] = 'Tuesday'
  }
 if(dataset[i,('Day.of.the.week')] == 4)
 {
  dataset[i,('Day.of.the.week')] = 'Wednesday'
 }
 if(dataset[i,('Day.of.the.week')] == 5)
 {
  dataset[i,('Day.of.the.week')] = 'Thursday'
 }
 if(dataset[i,('Day.of.the.week')] == 6)
 {
  dataset[i,('Day.of.the.week')] = 'Friday'
 }
}

#Seasons (summer (1), autumn (2), winter (3), spring (4))
for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Seasons')] == 1)
  {
  dataset[i,('Seasons')] = 'summer'
  }
  if(dataset[i,('Seasons')] == 2)
  {
  dataset[i,('Seasons')] = 'autumn'
  }
  if(dataset[i,('Seasons')] == 3)
  {
  dataset[i,('Seasons')] = 'winter'
  }
  if(dataset[i,('Seasons')] == 4)
  {
  dataset[i,('Seasons')] = 'spring'
  }
}


# Education (high school (1), graduate (2), postgraduate (3), master and doctor (4))
dataset$Education[is.na(dataset$Education)] = -1

for (i in c(1,2:nrow(dataset)))
{
  if(dataset[i,('Education')] == 1)
  {
  dataset[i,('Education')] = 'high school'
  }
  if(dataset[i,('Education')] == 2)
  {
  dataset[i,('Education')] = 'graduate'
  }
  if(dataset[i,('Education')] == 3)
  {
  dataset[i,('Education')] = 'postgraduate'
  }
  if(dataset[i,('Education')] == 4)
  {
  dataset[i,('Education')] = 'master and doctor'
  }
}
  
View(dataset)

#Converting required variables to categorical and integer as explained in the project report
for (i in colnames(DataCat))
  {
   #print(dataset[,i]) 
  dataset[,i] = as.factor(dataset[,i])
}

for (i in colnames(DataCon))
{
  #print(dataset[,i]) 
  dataset[,i] = as.integer(dataset[,i])
}

str(dataset)


#Missing Values

#Dividing Categorical variables into two dataframes

#Variables like ID,Reason for absence,Day of the week ,Seasons ,Education. Since in this case 0 shows loss of information,So we will replace them with Nan values and then impute them
DataCat_1 = data.frame()
DataCat_1 = subset(DataCat , select = c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Education'))
#DataCat_1.isnull().sum()

#Variables like 'Disciplinary failure','Son', 'Social drinker','Social smoker', 'Pet' include 0 as important information so we are keeping them in other dataset
DataCat_2 = data.frame()
DataCat_2 = subset(DataCat, select = c('Disciplinary.failure', 'Social.drinker','Social.smoker'))

#DataCat_1
#replace 0 with NA 
DataCat_1[DataCat_1 == 0] <- NA

#creting a new dataframe for DataSet for implementing data preprocessing techniques
#Adding firstly continuous data
DataSet = data.frame()
DataSet = DataCon
View(DataSet)

#Missing values percentage for continuous data
missing_val = data.frame(apply(DataSet,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(dataset)) * 100
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val


#Firstly Imputating missing values for Continuous Variables
#Creating Null value (Actual value is 36)
DataSet[1,('Distance.from.Residence.to.Work')] = NA
DataSet[1,('Distance.from.Residence.to.Work')]

#Mean Method (29.659)
DataSet$Distance.from.Residence.to.Work[is.na(DataSet$Distance.from.Residence.to.Work)] = mean(DataSet$Distance.from.Residence.to.Work, na.rm = T)
DataSet[1,('Distance.from.Residence.to.Work')]

#Median Method(26)
DataSet$Distance.from.Residence.to.Work[is.na(DataSet$Distance.from.Residence.to.Work)] = median(DataSet$Distance.from.Residence.to.Work, na.rm = T)
DataSet[1,('Distance.from.Residence.to.Work')]

# kNN Imputation
DataSet = knnImputation(DataSet, k = 3)
DataSet[1,('Distance.from.Residence.to.Work')]

##For Categorical Variables Imputting with Mode
for (i in colnames(DataCat_1))
{
  #print(dataset[,i]) 
  DataCat_1[,i] = as.integer(DataCat_1[,i])
}

#R does not have a standard in-built function to calculate mode. So we create a user function to calculate mode of a data set in R.
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the user function.
r1 <- getmode(DataCat_1$Reason.for.absence)
r2 <- getmode(DataCat_1$ID)
r3 <- getmode(DataCat_1$Month.of.absence)
r4 <- getmode(DataCat_1$Day.of.the.week)
r5 <- getmode(DataCat_1$Seasons)
r6 <- getmode(DataCat_1$Education)

DataCat_1$Reason.for.absence[is.na(DataCat_1$Reason.for.absence)] = r1
DataCat_1$ID[is.na(DataCat_1$ID)] = r2
DataCat_1$Month.of.absence[is.na(DataCat_1$Month.of.absence)] = r3
DataCat_1$Day.of.the.week[is.na(DataCat_1$Day.of.the.week)] = r4
DataCat_1$Seasons[is.na(DataCat_1$Seasons)] = r5
DataCat_1$Education[is.na(DataCat_1$Education)] = r6
View(DataCat_1)

#For DataCat_2
r7 <- getmode(DataCat_2$Disciplinary.failure)
r8 <- getmode(DataCat_2$Social.drinker)
r9 <- getmode(DataCat_2$Social.smoker)

DataCat_2$Disciplinary.failure[is.na(DataCat_2$Disciplinary.failure)] = r7
DataCat_2$Social.drinker[is.na(DataCat_2$Social.drinker)] = r8
DataCat_2$Social.smoker[is.na(DataCat_2$Social.smoker)] = r9
View(DataCat_2)

#reviewing missing values(If they got imputed correctly or not)
sum(is.na(DataCat_1))
sum(is.na(DataCat_2))

#Joining categorical data with main dataset as we have imputed it seperatly 
df1 = cbind.data.frame(DataSet,DataCat_1,DataCat_2)
DataSet = df1
#DataSet is our final DataSet after imputing all the missing values
View(DataSet)


#Outlier Analysis
# ## BoxPlots - Distribution and Outlier Check
cnames = colnames(DataCon)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(DataSet))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)

# #Remove outliers using boxplot method
df = DataSet

# # #loop to remove from all variables
for(i in cnames){
  #print(i)
  val = DataSet[,i][DataSet[,i] %in% boxplot.stats(DataSet[,i])$out]
  #print(length(val))
  DataSet = DataSet[which(!DataSet[,i] %in% val),]
}

cnames = colnames(DataCon)

## Correlation Plot 
corrgram(DataSet[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


## Dimension Reduction
DataSet = subset(DataSet, select = -c(Age,Weight,Body.mass.index))


## Chi-square Test for categorical variables
#1
print(chisq.test(table(DataCat$ID,DataCat$Reason.for.absence)))
DataSet$ID = NULL
DataCat$ID = NULL
#2
print(chisq.test(table(DataCat$Month.of.absence,DataCat$Reason.for.absence)))
DataSet$Reason.for.absence = NULL
DataCat$Reason.for.absence = NULL

#3
print(chisq.test(table(DataCat$Month.of.absence,DataCat$Day.of.the.week)))

#4
print(chisq.test(table(DataCat$Seasons,DataCat$Day.of.the.week)))

#5
print(chisq.test(table(DataCat$Seasons,DataCat$Disciplinary.failure)))
DataSet$Disciplinary.failure = NULL
DataCat$Disciplinary.failure = NULL

#6
print(chisq.test(table(DataCat$Education,DataCat$Seasons)))

#7
print(chisq.test(table(DataCat$Son,DataCat$Education)))
DataSet$Son = NULL
DataCat$Son = NULL

#8
print(chisq.test(table(DataCat$Social.drinker,DataCat$Day.of.the.week)))

#9
print(chisq.test(table(DataCat$Social.drinker,DataCat$Education)))
DataSet$Education = NULL
DataCat$Education = NULL

#10
print(chisq.test(table(DataCat$Social.drinker,DataCat$Social.smoker)))
DataSet$Social.smoker = NULL
DataCat$Social.smoker = NULL

#11
print(chisq.test(table(DataCat$Pet,DataCat$Social.drinker)))
DataSet$Pet = NULL
DataCat$Pet = NULL

#12
print(chisq.test(table(DataCat$Month.of.absence,DataCat$Day.of.the.week)))

#13
print(chisq.test(table(DataCat$Month.of.absence,DataCat$Seasons)))
DataSet$Seasons = NULL
DataCat$Seasons = NULL

#14
print(chisq.test(table(DataCat$Day.of.the.week,DataCat$Social.drinker)))

#15
print(chisq.test(table(DataCat$Month.of.absence,DataCat$Social.drinker)))
DataSet$Month.of.absence = NULL
DataCat$Month.of.absence = NULL

#Dataset after feature Selection
View(DataSet)


#Normalisation
cnames = c(colnames(DataCon))

for(i in cnames){
  print(i)
  DataSet[,i] = (DataSet[,i] - min(DataSet[,i]))/
    (max(DataSet[,i] - min(DataSet[,i])))
}

#Dataset after feature Scaling
View(DataSet)


#Divide the data into train and test
TrainData_index = sample(1:nrow(DataSet), 0.8 * nrow(DataSet))
TrainData = DataSet[TrainData_index,]
TestData = DataSet[-TrainData_index,]


#1. Linear Regression Model

#check multicollearity
#usdm helps us to do use vif function to find out multicollinearity effect
library(usdm)
vif(DataSet[,-7])
vifcor(DataSet[,-7], th = 0.9)
#As a result we haven't got    min correlation( Day.of.the.week ~ Work.load.Average.day. ):  -0.0001513119 
#                             max correlation ( Social.drinker ~ Distance.from.Residence.to.Work ):  0.448258
#Which are far from our threshold value 0.9 , So we do not need to remove any of the variables from our current DataSet

#run regression model
lm_model = lm(Absenteeism.time.in.hours ~., data = TrainData)
#Summary of the model
summary(lm_model)
#Predict
predictions_LR = predict(lm_model, TestData[,-7])

#Calculate MAE
MAE( predictions_LR,TestData[,7])
#Calculate RMSE
RMSE(predictions_LR,TestData[,7])


#2. Decision Tree Regression Model

#Load Libraries
library(rpart)
#rpart for regression
fit2 = rpart(Absenteeism.time.in.hours ~ ., data = TrainData, method = "anova")
#Predict for new test cases
predictions_DT = predict(fit2, TestData[,-7])

#calculate MAE
MAE(predictions_DT,TestData[,7])
#Calculate RMSE
RMSE(predictions_DT,TestData[,7])



#3. Random Forest Regression Model

fit3 = randomForest(formula = Absenteeism.time.in.hours ~ ., data = TrainData, ntree = 45, method = "anova")
#Predict for new test cases
predictions_RF = predict(fit3, TestData[,-7])

#calculate MAE
MAE(predictions_RF,TestData[,7])
#Calculate RMSE
RMSE(predictions_RF,TestData[,7])



#Applying Random Forest on entire DataSet
fit_N = randomForest(formula = Absenteeism.time.in.hours ~ ., data = DataSet, ntree = 45 , method = "anova") 
#Predict test data using random forest model
predictions_RF = predict(fit_N, DataSet[,-7])



#Write output results
DataTestFinal = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex =1,header = T)
df_Output = data.frame(DataTestFinal)
df_Output['Predicted_Absenteeism'] = predictions_RF
write.csv(df_Output, "R_Absenteeism_Output.csv",row.names = F)

