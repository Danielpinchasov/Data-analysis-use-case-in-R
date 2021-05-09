data(package = .packages(all.available = TRUE))

## Task 1 Solution

setwd("C:\\Users\\User\\Desktop\\שפת R")
Dataset.df <- read.csv('FinalWork.csv',stringsAsFactors = F)
## First we will look at the data
str(Dataset.df)
summary(Dataset.df)

## Create a data frame with the variables needed for the task
my_dataframe.df<-Dataset.df[c(29,31,33)]
summary(my_dataframe.df)
str(my_dataframe.df)

## Checking the values in the variables 
as.factor(my_dataframe.df$Usage)
as.factor(my_dataframe.df$Product)

install.packages("ggplot2")
library(ggplot2)

## Checking the minimum and maximum values for building a chart
summary(my_dataframe.df$Age)
summary(my_dataframe.df$Product)

## Build a chart that visually depicts the relationship between the product sold to the customer and his age 
ggplot(data = my_dataframe.df) + 
  geom_jitter(aes(x = my_dataframe.df$Product, y = my_dataframe.df$Age, color = my_dataframe.df$Usage))+
  ylab("Age of customer")+ 
  xlab("Products")+
  ggtitle("The connection between product type and cutomer age")+
  scale_color_manual(name = "Product Type",labels = c("Artificial ", "Natural"), values = c("blue", "red"))+
  ylim(c(10,100)) 


## Task 2 Solution

## Convert a variable to a factor for the purpose of arranging the data 
Dataset.df$Prod_dest<-as.factor(Dataset.df$Prod_dest)
str(Dataset.df)  
levels(Dataset.df$Prod_dest)

## Removing unnecessary states for the task 
Dataset.df<- Dataset.df[(Dataset.df$Prod_dest!="Benin") & (Dataset.df$Prod_dest!="Norway"),]
Dataset.df$Prod_dest <- droplevels(Dataset.df$Prod_dest)


## Finding the independent variables that are best for building the model 
All_var = lm(Profit ~ .,data= Dataset.df)
formula(FitAll)
FirstModel = lm(Profit ~ 1, data=Dataset.df)
summary(FirstModel)
step(FirstModel,direction="both",scope=formula(All_var))


## Building an explanatory model and running a separate model for each country
country <- unique(Dataset.df$Prod_dest) 
List <- vector('list',length(country))
names(List )<-country
for (i in seq_along(country)){
  List [[i]]<- lm( Profit ~ Machine_482 + Tan_39 + Machine_7 + Machine_100 + Machine_66t ,data= filter(Dataset.df, Prod_dest==country[[i]]))
}

x<-lapply(List , summary)
x

## Task 3 Solution

## We will summarize the data to understand this well
summary(Dataset.df)
## We will use this libary so that we can check with certain commands the different types of data and remove blank data if necessary
library(funModeling)
df_status(Dataset.df)
## We see that there are no empty values, so this function is unnecessary
Dataset.df <- Dataset.df[complete.cases(Dataset.df)]
df_status(Dataset.df)
## Here you can see the different types of data: factor, numeric and more
str(Dataset.df)
## Using this library we can select the variables we will focus on in the prediction task
library(dplyr)
Dataset.df <- Dataset.df %>%
  select(Product, Usage, Prod_dest, Profit )
df_status(Dataset.df)
## The variable we chose to predict is the type of product: it means we want to predict all the natural products purchased
## For this purpose, the task we have chosen to perform the data is classification
summary(Dataset.df$Usage)
## In order to use the levels function later to classify the distribution types of the variable values, we need to make the usage variable a type-factor variable
Dataset.df$Usage <- as.factor(Dataset.df$Usage)
df_status(Dataset.df)
library(caret)
## A function that shows us the categories of the selected target variable
?levels
levels(Dataset.df$Usage)
## From what we see here, the first variable that appears is products with artificial materials, but since we are interested in predicting the natural products we must reverse the order
Dataset.df$Usage <- relevel(Dataset.df$Usage,'Natural')
levels(Dataset.df$Usage) ## Now we can move on
## To ensure that all results and data can be recovered we will use the following function
set.seed(500)
## At this point, before we select an implementation algorithm, we must divide the data. And as a rule, 70% of the data is usually allocated to the training group where we can perform further calculations and manipulations. (The rest of the data is about 30% untouched and will be in the Test group)
## This step is necessary to avoid over-fitting the data on the model
trainIndex <- createDataPartition(Dataset.df$Usage,
                                  p = .7, list = FALSE)
training <- Dataset.df[trainIndex,]
testing <- Dataset.df[-trainIndex,]
## At this point we would like to "fold" the data in the training model in order to reach more groups within the training model, this part is very important because it affects the quality of the model
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 2,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = 'all')

## We would like to choose two algorithms for applying the model
## In each model we use, we will examine the indices we received, such as: accuracy index, recall, precision and other indices
## And finally we will compare the indices we got in each model and see what is the best model we built

## A classification model called: logistical regression
## This algorithm is a prediction model
Logistic_regression <- train(Usage ~ .,data = training,
                             method= 'glm',family="binomial",
                             trControl = fitControl)
Logistic_regression
Logistic_regression$bestTune
class(Logistic_regression)
typeof(Logistic_regression)
Logistic_regression$results 
Logistic_regression$pred 
varImp(Logistic_regression) ## Here you can see that the "product" variable has had more impact on the degree of forecasting than all other variables

## The second prediction algorithm chosen is: Decision Tree
Classification_trees <- train(Usage ~., training,'ctree',
                              trControl = fitControl,metric = "ROC")
Classification_trees

## the confusion matrix for the selected model
LR_predict <- predict(Logistic_regression, testing)
class(LR_predict)
install.packages("e1071")
LR_ConfusionMat <- confusionMatrix(LR_predict,
                                   testing$Usage)
LR_ConfusionMat