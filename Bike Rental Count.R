#Removing any pre-existing objects
rm(list=ls())

#Setting the directory
print("Setting the current working directory")
setwd("C:/Users/Akash/Desktop/Works/data frames/Edwisor/Bike Rental Count/edwisor")

#loading the libraries
print("Loading the libraries")
x = c("randomForest", "ggplot2",  "rpart",  "dplyr", "sampling",  "lubridate", "DMwR", "corrplot", "caret","mltools")


#loading all packages at once and then removing x as it will of no use
lapply(x, require, character.only = TRUE)
rm(x)


#----------------------------------------------------------------------------------
#loading the data

print("Loading the data")
train= read.csv("train_data.csv")
print("Check first few rows of the data")
head(train)

#----------------------------------------------------------------------------------

#Missing Value Analysis
print("Missing Value Analysis")

#checking missing values
missing_val= data.frame(apply(train, 2, function(x){sum(is.na(x))}))

missing_val$columns= row.names(missing_val)
row.names(missing_val)= NULL

#renaming first var name as Missing_percentage
names(missing_val)[1]= "Missing_percentage"

#sorting the dataframe in descending order according to Missing_percentage variable
missing_val= missing_val[order(-missing_val$Missing_percentage),]

#rearranging the columns
missing_val= missing_val[,c(2,1)]

print("Sum of Percentage of Missing values in all variables is:")
print(sum(missing_val$Missing_percentage))
print("We can see that there are no missing values in any of the variables in the dataset")

#----------------------------------------------------------------------------------

#checking structure of the dataset
str(train)

#we can delete dteday as all the relevant information like month, year etc are already present
#we can also delete instant as its of no use

train$dteday <- NULL
train$instant <- NULL

# Converting integer to factor on training set

train$season <- as.factor(train$season)
train$yr <- as.factor(train$yr)
train$mnth <- as.factor(train$mnth)
train$holiday <- as.factor(train$holiday)
train$weekday <- as.factor(train$weekday)
train$workingday <- as.factor(train$workingday)
train$weathersit <- as.factor(train$weathersit)


# barchart on Bike Rental Count based on the working day or Not
ggplot(train, aes(workingday)) + geom_bar(fill = "#0073C2FF") + scale_x_discrete(name="",labels=c("Holiday / Weekend","Working Day")) +
                                                                                scale_y_continuous(name="Bike Count") +
                                                                                ggtitle("Effect on Bike Count due to Working Day/Holiday")


#effect on bike count due to temp
ggplot(data=train, aes(train$temp)) + geom_histogram(bins =10, fill = "#0FB7D9") +
                                      scale_x_continuous(name="Temperature") +
                                      scale_y_continuous("Bike Count")+
                                      ggtitle("Effect on Bike Count due to Temp")

#effect due to humidity
ggplot(data= train, aes(train$hum)) + geom_histogram(bins =10, fill= "#BC60E7") + 
                                      scale_x_continuous("Humidity")+ 
                                      scale_y_continuous("Bike Count")+
                                      ggtitle("Effect on Bike Count due to Humidity")


#creating boxplot to see effect of bike count due to season
ggplot(train, aes(x = season, y = cnt, fill= season)) +geom_boxplot()+  scale_x_discrete(name="",labels=c('Spring','Summer','Fall','Winter'))+scale_y_continuous(name="Bike Count")+theme(legend.position="none") +ggtitle("Effect on Bike Count due to Season")


#creating boxplot to see effect of bike count due to weather
ggplot(train, aes(x = weathersit, y = cnt, fill= weathersit)) + geom_boxplot() + scale_x_discrete(name="",labels=c('Clear, Few Clouds, Partly Cloudy','Mist+ Cloudy, Mist + Broken Clouds, Mist+Few Clouds','Light Snow, Light Rain+Thunderstorm, Scattered Clouds'))+scale_y_continuous(name="Bike Count")+theme(legend.position="none") +ggtitle("Effect on Bike Count due to Weather")

#creating boxplot to see effect of bike count due to weekday
ggplot(train, aes(x = weekday, y = cnt, fill= factor(weekday))) + geom_boxplot() + scale_x_discrete(name="",labels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))+scale_y_continuous(name="Bike Count")+theme(legend.position="none")+theme(legend.position="none")+ggtitle("Effect on Bike Count during WeekDays")


#creating boxplot to see effect of bike count in year
ggplot(train, aes(x = yr, y = cnt, fill= factor(yr))) +geom_boxplot()+ scale_x_discrete(name="",labels=c('2011','2012'))+ scale_y_continuous(name="Bike Count")+ theme(legend.position="none") + ggtitle("Bike Count in Year 2011 and 2012")


#Checking Outliers

#replacing outliers with Null
cnames= c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered')
  
for (i in cnames){
  val= train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  train[,i][train[,i] %in% val] = NA
}

# checking count of NA in each variable
table(is.na(train))

#we will impute the values using knn
train= knnImputation(train, k=3)

# checking count of NA in each variable
table(is.na(train))

#Now we can see that there are no NA now

#---------------------------------------------------------------------------------------------------------------

#Feature Selection

#correlation plot
corr_chart= cor(train[,8:14])

#plotting corr chart
corrplot(corr_chart, method = "number") 

# We can delete atemp and casual
# We can see that temp and atemp are very highly correlated with each other, and they have same amt of correlation with output variable,
# so we will drop one of them (let's say atemp). We can see that casual and registered variables are also very correlated with each other,
# but registered is very highly correlated with output var, so we will drop casual variable

train$atemp <- NULL
train$casual <- NULL


#------------------------------------------------------------------------------------------------------------
#Split for train and test data

set.seed(1234)
train_index= createDataPartition(train$cnt, p=0.80, list= FALSE)

#making training and test data
train_sample= train[train_index,]
test_sample= train[-train_index,]

#-------------------------------------------------------------------------------------------------------------

#Applying Models

#Random forest
set.seed(123)
random_forest <-randomForest(cnt~., data=train_sample, ntree=100, importance= TRUE) 

#predicting
pred_random_forest <- predict(random_forest, newdata=test_sample[,-12])
pred_random_forest<- unname(pred_random_forest)

#Error Metrics

#1 RMSLE
rmsle(test_sample[,12], pred_random_forest)

#2 MAPE
y= test_sample[,12]

mape= function (y, pred_random_forest ) {
  mean( abs ( (y- pred_random_forest ) / y ) ) * 100
}

mape(y, pred_random_forest)

#3 MSLE
msle(y, pred_random_forest)


#4 Mean Absolute Error
mean(abs(pred_random_forest - test_sample[, 12]))

#-----------------------------------------------------------------------------------------------------------------------

#Decision tree
decision_tree= rpart(cnt~., data= train_sample, method="anova")

# make predictions
decision_predictions <- predict(decision_tree, test_sample[-12])

#Error Metrics

#1 RMSLE
rmsle(decision_predictions, test_sample[,12])

#2 MAPE
y= test_sample[,12]

mape= function (y, decisionpredictions) {
  mean( abs ( (test_sample[,12]- decision_predictions) / test_sample[,12] ) )*100
}

mape(y, decision_predictions)

#3 MSLE
msle(y, decision_predictions)

#4 MAE
mean(abs(decision_predictions - test_sample[, 12]))


#------------------------------------------------------------------------------------------------------------------------

#Applying KNN

set.seed(123)
model_knn <- train(cnt~., data = train_sample, method = "knn")

# Make predictions on the test data
knn_predictions <- predict(model_knn, test_sample[,-12])

#Error Predictions
#1 RMSLE
rmsle(test_sample[,12], knn_predictions)

#2 MAPE
y= test_sample[,12]

mape= function (y, knn_predictions ) {
  mean( abs ( (y- knn_predictions ) / y ) ) * 100
}

mape(y, knn_predictions)

#3 MSLE
msle(y, knn_predictions)

#4 MAE
mean(abs(knn_predictions - test_sample[, 12]))

#----------------------------------------------------------------------------------------------------------------------------------------------------

#saving Actual vs Predicted Values file

output_file= data.frame(test_sample[,12], pred_random_forest)
names(output_file)[1] <- "Actual Values"
names(output_file)[2] <- "Predicted Values"
write.csv(output_file, 'Actual vs Predicted(From R).csv', row.names = FALSE)

