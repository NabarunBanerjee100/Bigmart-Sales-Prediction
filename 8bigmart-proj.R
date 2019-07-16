library(data.table) # used for reading and manipulation of data 
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(dplyr)
library(plyr)
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
# Importing the data set by giving the required path
setwd("H:\business analytics\bigmart sales prediction")
train = fread("Train_UWu5bXk.txt") 
test = fread("Test_u94Q5KV.txt")
submission = fread("SampleSubmission_TmnO39y.txt")
test
sum(is.na(test$Item_Identifier))

str(test)## displaying the structure of the data set

str(train)
# checking the dimensions of the data
dim(train)
dim(test)

sum(!complete.cases(train))# checking the missing values in the data set

test$Item_Outlet_Sales<-NA
str(test)
combine<-rbind(train,test) # combining the train and test data set 


# plotting the numerical variables
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")
ggplot(combine) + geom_histogram(aes(combine$Item_Weight), binwidth = 0.5, fill = "darkgreen") +  xlab("Item_Weight")
ggplot(combine) + geom_histogram(aes(combine$Item_MRP), binwidth = 0.5, fill = "darkblue") +  xlab("Item_MRP")



ggplot(combine, aes(factor(Item_Fat_Content),
                   fill = factor(Item_Fat_Content))) +
  geom_bar()

# combining the misprinted elements in the variable
combine$Item_Fat_Content[combine$Item_Fat_Content == "LF"] <- "Low Fat"
combine$Item_Fat_Content[combine$Item_Fat_Content == "low fat"] <-"Low Fat"
combine$Item_Fat_Content[combine$Item_Fat_Content == "reg"] <- "Regular" 

#Outlet_identifier vs Item_weight
ggplot(combine, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet_Identifier")

#imputing the missing values in item_weight

weights1 <- as.data.frame( ddply(na.omit(combine), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

# we can now use these values to fill in the missing weight values:
combine$Item_Weight <- ifelse(is.na(combine$Item_Weight), 
                            weights1$mean[
                              match(combine$Item_Identifier, weights1$Item_Identifier)], combine$Item_Weight)


sum(is.na(combine$Item_Weight )) # checking the number of missing values in the variable

ggplot(combine1) + geom_histogram(aes(Item_Visibility), bins = 100)
combine1$Item_Visibility [combine1$Item_Visibility == 0]<-NA # replacing the zeroes with NA in the variable
combine1<-combine
weights2 <- as.data.frame( ddply( na.omit(combine1), 
                                 ~Item_Identifier, 
                                 summarise, 
                                 mean=mean(Item_Visibility))) 
           
combine$Item_Visibility <- ifelse(is.na(combine$Item_Visibility), weights2$mean[match(combine$Item_Visibility, weights2$Item_Visibility)], combine$Item_Visibility)# imputing the missing values

 combine2<-combine # making a copy of combine
 
 combine$Year <- as.factor(2013 - combine$Outlet_Establishment_Year) # creating another numerical variable and subtracting 
 # each year from 2003(base)
combine$ Outlet_Establishment_Year = NULL



str(combine)

sapply(combine, function(x) sum(is.na(x))) #checking the missing value in each variable in the dataset


combine$Item_Visibility




sum(is.na(combine$Item_Visibility))


table(is.na(combine))

combine1<-combine

zero_index = which(combine$Item_Visibility == 0) 
# dividing Item_Mrp into groups based on the plot 
combine$MRP_group<- ifelse(combine$Item_MRP<69, "1st","2nd")
combine$MRP_group<- ifelse(combine$Item_MRP>69 & combine$Item_MRP<136, "2nd",combine$MRP_group)
combine$MRP_group<- ifelse(combine$Item_MRP>136 & combine$Item_MRP<204, "3rd",combine$MRP_group)
combine$MRP_group<- ifelse(combine$Item_MRP>204 , "4th",combine$MRP_group)

combine$MRP_group<-as.factor(combine$MRP_group)
boxplot(train$Item_Outlet_Sales)
#Outlet_Location_Type is grouped into 1,2 and 3 which makes it easier to interpret
str(combine)
combine$Outlet_Location_Type<- ifelse(combine$Outlet_Location_Type=="Tier 1", 1,2)
combine$Outlet_Location_Type<- ifelse(combine$Outlet_Location_Type=="Tier 2", 2,combine$Outlet_Location_Type)
combine$Outlet_Location_Type<- ifelse(combine$Outlet_Location_Type=="Tier 3", 3,combine$Outlet_Location_Type)


combine[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))] #creating another variable 

combine$Outlet_Size<-NULL

sapply(combine, function(x) sum(is.na(x))) # checking if there is any missing value in the data set combine

combine$Item_MRP<-NULL # dropping the Item_MRP variable because it has already been grouped

str(combine)

table(combine$Item_Type, substr(combine$Item_Identifier, 1, 2))

combine$itemtype
# creating a new variable Item_Type_new and segregating on the basis of the table formed
combine$Item_Type_new<- ifelse(combine$Item_Type == "Health and Hygiene","NC","FD")
combine$Item_Type_new<- ifelse(combine$Item_Type == "Household","NC",combine$Item_Type_new)
combine$Item_Type_new<- ifelse(combine$Item_Type == "Others","NC",combine$Item_Type_new)
combine$Item_Type_new<- ifelse(combine$Item_Type == "Hard Drinks","DR",combine$Item_Type_new)
combine$Item_Type_new<- ifelse(combine$Item_Type == "Soft Drinks","DR",combine$Item_Type_new)

count(combine$Item_Type_new[combine$Item_Type_new=="NC"])
summary(combine)
str(combine)

combine1<-combine
combine$Item_Type<-NULL
combine2<-combine

str(combine)
summary(combine)
boxplot(combine$Item_Outlet_Sales)
combine$Item_MRP<-NULL
combine3<-combine
train1<-combine[1:8523,] # seperating into again training and test data set
sum(is.na(train1$Item_Outlet_Sales))


combine$Item_Fat_Content<-as.factor(combine$Item_Fat_Content)
combine$Item_Type_new<-as.factor(combine$Item_Type_new)
levels(combine$Item_Type_new)
combine$Outlet_Type<-as.factor(combine$Outlet_Type)
combine$Item_Identifier<-NULL
str(combine)

combine$Outlet_Identifier<-as.factor(combine$Outlet_Identifier)


sum(is.na(train1$Item_Outlet_Sales))
str(train1)
test1<-combine[is.na(combine$Item_Outlet_Sales),]

sapply(combine ,function(x) sum(is.na(x)))
boxplot(train1$Item_Outlet_Sales) # presence of outliers in the data , we need to treat it

sunny <- function(x,y){
  attach(x)
  b <- IQR(y)
  q1 <- as.numeric(quantile(y)[2] )
  q3 <- as.numeric(quantile(y)[4] )
  iqr <- IQR(y)
  mild_low <- q1-(1.5*iqr)#mild lower limit
  mild_high <- q3+(1.5*iqr)#mild higher limit
  extreme_low <- q1-(3*iqr)#extreme lower limit
  extreme_high <- q3+(3*iqr)#extreme higher limit
  final <- cbind(mild_low,mild_high,extreme_low,extreme_high)
  return(final)
}
train2<-train1
mia <- sunny(train1,Item_Visibility)
mia

mia <- sunny(train1,Item_Visibility)
mia
nrow(train1)



# mild outliers
data <- train1[train1$Item_Visibility > mia[1],]
data <- train1[train1$Item_Visibility < mia[2],]

# extreme outliers
data <- data[data$Checking_amount > mia[3],]
data <- data[data$Checking_amount < mia[4],]
train1<-train1[train1$Item_Outlet_Sales<6270,]
boxplot(train1$Item_Visibility)
str(train1)
train1<-train1[train1$Item_Visibility<0.1979314,]# deleting outliers from the data

#train1$Item_Fat_Content[train1$Item_Fat_Content == "LF"] <- "Low Fat"
#train1$Item_Fat_Content[train1$Item_Fat_Content == "low fat"] <-"Low Fat"
#train1$Item_Fat_Content[train1$Item_Fat_Content == "reg"] <- "Regular" 
#train1$Item_Fat_Content<-as.factor(train1$Item_Fat_Content)
levels(train1$Item_Fat_Content)
train1[train1$ Item_Visibility==0,]
str(test1)
#train1$Item_Type<-train$Item_Type[1:8153]
sapply(train1, function(x) sum(is.na(x)))
test1$Item_Fat_Content[test1$Item_Fat_Content == "LF"] <- "Low Fat"
test1$Item_Fat_Content[test1$Item_Fat_Content == "low fat"] <-"Low Fat"
test1$Item_Fat_Content[test1$Item_Fat_Content == "reg"] <- "Regular" 
test1$Item_Fat_Content<-as.factor(test1$Item_Fat_Content)
levels(test1$Item_Fat_Content)
str(train1)
levels(train1$Item_Fat_Content)
nrow(na.omit(train1))
names(train1)

train1$Item_Type<-as.factor(train1$Item_Type)# adding the removed column which had been deletd by mistake


fit<- lm(log(Item_Outlet_Sales) ~ ., data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~ Item_Weight +  Item_Fat_Content+ Item_Visibility + Outlet_Identifier+Outlet_Location_Type + MRP_group  +Item_Type_new, data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~ Item_Weight + Item_Visibility + Outlet_Identifier+Outlet_Location_Type + MRP_group  +Item_Type_new, data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~  Item_Visibility + Outlet_Identifier + MRP_group  +Item_Type_new, data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~   Outlet_Identifier + MRP_group  +Item_Type_new, data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~   Outlet_Identifier + MRP_group  , data=train1)
summary(fit)

fit<- lm(log(Item_Outlet_Sales) ~ I(Outlet_Identifier=="OUT027") +I(Outlet_Identifier=="OUT018")+ I(Outlet_Identifier=="OUT035")+I(Outlet_Identifier=="OUT017")+ I(Outlet_Identifier=="OUT045")+I(Outlet_Identifier=="OUT013")+ I(Outlet_Identifier=="OUT046")+ I(Outlet_Identifier=="OUT049")+ I(MRP_group=="2nd")+I(MRP_group=="3rd") +I(MRP_group=="4th") , data=train1)
summary(fit)# final model

vif(fit) # checking the variation inflation factor for the training model


## MAPE
train1$pred <- fitted(fit)
train1$pred<-exp(train1$pred)
#Calculating MAPE
attach(train1)
(sum((abs(Item_Outlet_Sales-pred))/Item_Outlet_Sales))/nrow(train1) # as the target variable is rightly skewed so mape is little bit higher than expected

str(train2)
plot(fit)
summary(train1)
train1$pred<-NULL
str(train1)
plot(pred,train1$Item_Outlet_Sales, # checking the actual vs residual plot
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


error <- train1$pred - train1$Item_Outlet_Sales

sqrt(mean(error^2)) # RMSE = 1006.763

str(train1)


head(train1)
test1$Item_Type<-test$Item_Type


p<-predict(fit,test1) # applying the model on the test data
exp(p)
test1$Item_Outlet_Sales<-exp(p)
hist(test1$Item_Outlet_Sales)

