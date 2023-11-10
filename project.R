###### SIM PROJECT

# Libraries
library(tidyverse)
library(skimr)
library(FactoMineR)
library(car)
library(lmtest)
library(ISLR)
library(arm)
library(mice)

# Clear space
rm(list=ls())
par(mfrow=c(1,1))

# Load data
#getwd()
setwd("C:/Users/Silvia/OneDrive - Universitat Politècnica de Catalunya/Escritorio/UPC/MASTER DS/1A/SIM/assignment 1")
#setwd("/Users/ali/Desktop/MASTER/SIM/PROJECT 1")

train<-read.delim("train.csv", sep=',') 

# Explore dataset
dim(train)
names(train)
str(train)
#skim(train)

# Preproces dataset

## recode incorrect NA's 
train$Alley[which(is.na(train$Alley))] <- "No alley access"
train$BsmtQual[which(is.na(train$BsmtQual))] <- "No Basement"
train$BsmtCond[which(is.na(train$BsmtCond))] <- "No Basement"
train$BsmtExposure[which(is.na(train$BsmtExposure))] <- "No Basement"
train$BsmtFinType1[which(is.na(train$BsmtFinType1))] <- "No Basement"
train$BsmtFinType2[which(is.na(train$BsmtFinType2))] <- "No Basement"
train$FireplaceQu[which(is.na(train$FireplaceQu))] <- "No Fireplace"
train$GarageType[which(is.na(train$GarageType))] <- "No Garage"
train$GarageFinish[which(is.na(train$GarageFinish))] <- "No Garage"
train$GarageCond[which(is.na(train$GarageCond))] <- "No Garage"
train$PoolQC[which(is.na(train$PoolQC))] <- "No Pool"
train$Fence[which(is.na(train$Fence))] <- "No Fence"
train$MiscFeature[which(is.na(train$MiscFeature))] <- "None"


## Recode character variables to factor
char_cols <- which(sapply(train, is.character))
train[, char_cols] <- lapply(train[, char_cols], as.factor) 

# Recode: numeric to factor
train$MSSubClass <- factor(train$MSSubClass)
train$MoSold <- factor(train$MoSold) # month 
#train$GarageYrBlt<-factor(train$GarageYrBlt) 
#train$OverallCond <- factor(train$OverallCond)
#train$OverallQual <- factor(train$OverallQual)

# Target variable exploration "SalePrice"
res.con <- condes(train,81)
res.con$quali
shapiro.test(train$SalePrice)  ## Normality test of the target variable  ->>>  # We test the null hypothesis H0: variable is normal, H1: variable not normal.  # The p-value is less than 0.05, therefore we reject H0.  # For that we will not consider the target variable as normally distributed
qqnorm(train$SalePrice)
qqline(train$SalePrice)
hist(train$SalePrice, prob = TRUE, col = 'lightblue', main = 'SalePrice Distribution', xlab = 'SalePrice')
lines(density(train$SalePrice), col = 'red', lwd = 2)

# selection of 10 factor variables and numerical variables 
vect<-row.names(res.con$quali)
vect[1:10] 
factor_train<-train[,c(vect[1:10])] # select the first 10 factor variables that are more related with the target
num_cols <- names(train)[sapply(train, is.numeric)]
train2 <- train[,c(num_cols, vect[1:10])]
dim(train2)
# we make the following assumptions : 
# choose the 10 categorical variables that are more correlated with the target variable

## numeric variable description function
numeric_description <- function(variable, n_breaks) {
  cat("Summary:\n")
  print(summary(variable))
  
  cat("\nCount of missing values:",sum(is.na(variable)),"\n")
  
  hist(variable, breaks = n_breaks, freq = F)
  curve(dnorm(x, mean(variable), sd(variable)), add = T)
  
  # Normality test with Shapiro-Wilk
  print(shapiro.test(variable))
}

## outliers functions
analyze_outliers <- function(data, column_name) {
  sm <- summary(data[[column_name]])
  iqr <- sm["3rd Qu."] - sm["1st Qu."]
  
  # Mild Outliers
  mild_ub <- sm["3rd Qu."] + 1.5 * iqr
  mild_lb <- sm["1st Qu."] - 1.5 * iqr
  
  mild_outliers <- length(which(data[[column_name]] > mild_ub | data[[column_name]] < mild_lb))
  cat("Number of mild outliers:", mild_outliers, "\n")
  
  # Plotting mild outliers
  Boxplot(data[[column_name]], main = paste("Outlier Analysis for", column_name),
          ylab = column_name, outline = TRUE)
  abline(h = mild_ub, col = "orange", lwd = 2)
  abline(h = mild_lb, col = "orange", lwd = 2)
  
  # Severe Outliers
  severe_ub <- sm["3rd Qu."] + 3 * iqr
  severe_lb <- sm["1st Qu."] - 3 * iqr
  severe_outliers <- length(which(data[[column_name]] > severe_ub | data[[column_name]] < severe_lb))
  cat("Number of severe outliers:", severe_outliers, "\n")
  # Plotting severe outliers
  abline(h = severe_ub, col = "red", lwd = 2.5)
  abline(h = severe_lb, col = "red", lwd = 2.5)
}
train2$univ_outl_count <- 0 # count outliers (new column)
update_outliers_count <- function(dataframe, column_name) {
  df <- dataframe
  sm <- summary(df[[column_name]])
  iqr <- sm["3rd Qu."] - sm["1st Qu."]
  # Severe Outliers
  severe_ub <- sm["3rd Qu."] + 3 * iqr
  severe_lb <- sm["1st Qu."] - 3 * iqr
  severe_outliers_id <- which(df[[column_name]] > severe_ub | df[[column_name]] < severe_lb)
  df$univ_outl_count[severe_outliers_id] <- df$univ_outl_count[severe_outliers_id] + 1
  return(df)
}

# EDA EXPLORATORY DATA ANALYSIS 
skim(train2) # overall exploration
names(train2)

### Numerical variables
## Id (not considered for the analysis)

## "LotFrontage" Linear feet of street connected to property (cont)
numeric_description(train2$LotFrontage, 30)
# not normal distribution
analyze_outliers(train2, "LotFrontage")
train2 <- update_outliers_count(train2, "LotFrontage")

## "LotArea" Lot size in square feet (cont)
numeric_description(train2$LotArea, 20)
# not normal distribution
analyze_outliers(train2, "LotArea")
train2 <- update_outliers_count(train2, "LotArea")

## "OverallQual" (cont)
numeric_description(train2$OverallQual, 10)
# not normal distribution
analyze_outliers(train2, "OverallQual")
train2 <- update_outliers_count(train2, "OverallQual")

## "YearBuilt" (int)
summary(train2$YearBuilt)
barplot(table(train2$YearBuilt), main = "Distribution of Year Built",xlab = "Year",col = "skyblue")
sum(is.na(train2$YearBuilt))
analyze_outliers(train2, "YearBuilt")
train2 <- update_outliers_count(train2, "YearBuilt")

## "YearRemodAdd" (int) - Remodel date (same as construction date if no remodeling or additions)
summary(train2$YearRemodAdd)
barplot(table(train2$YearRemodAdd), main = "Distribution of Year Remodelized",xlab = "Year",col = "skyblue")
sum(is.na(train2$YearRemodAdd))
analyze_outliers(train2, "YearRemodAdd")
train2 <- update_outliers_count(train2, "YearRemodAdd")


## MasVnrArea - Masonry veneer area in square feet (cont)
numeric_description(train2$MasVnrArea, 10)
# not normal distribution
analyze_outliers(train2,"MasVnrArea")
train2 <- update_outliers_count(train2, "MasVnrArea")
# comparar amb variable MasVnrType
# si el type != None i es 0 -> convertir en NA
train2$MasVnrArea[which(train2$MasVnrArea == 0)] <- NA
none_idx <- which(train$MasVnrType == 'None')
train2$MasVnrArea[none_idx] <- 0
#Boxplot(train2$MasVnrArea)
#sm<-summary(train2$MasVnrArea)
#iqr <- sm["3rd Qu."]-sm["1st Qu."]; iqr
#train2$f.MasVnrArea <- ifelse(train2$MasVnrArea <= sm["1st Qu."], 1, ifelse(train2$MasVnrArea > sm["1st Qu."] & train2$MasVnrArea < sm["Median"], 2, ifelse(train2$MasVnrArea >= sm["Median"] & train2$MasVnrArea <= sm["3rd Qu."], 3, ifelse(train2$MasVnrArea > sm["3rd Qu."], 4,0))))
#train2$f.MasVnrArea <- factor(train2$f.MasVnrArea,labels=c("LowmasVnrArea","LowMidmasVnrArea","HighMidmasVnrArea","HighmasVnrArea"), order = T, levels=c(1,2,3,4))
#table(train2$f.MasVnrArea)


## "BsmtFinSF1" - Type 1 finished square feet (cont)
numeric_description(train2$BsmtFinSF1, 10)
# not normal distribution
analyze_outliers(train2,"BsmtFinSF1")
train2 <- update_outliers_count(train2, "BsmtFinSF1")


# BsmtFinSF2 - Rating of basement finished area (if multiple types) (cont)
numeric_description(train2$BsmtFinSF2, 10)
# not normal distribution
analyze_outliers(train2,"BsmtFinSF2")
train2 <- update_outliers_count(train2, "BsmtFinSF2")

# "BsmtUnfSF" (cont)
numeric_description(train2$BsmtUnfSF, 20)
# not normal distribution
analyze_outliers(train2,"BsmtUnfSF")
train2 <- update_outliers_count(train2, "BsmtUnfSF")

# "TotalBsmtSF" (cont)
numeric_description(train2$TotalBsmtSF, 10)
# not normal distribution
analyze_outliers(train2,"TotalBsmtSF")
train2 <- update_outliers_count(train2, "TotalBsmtSF")


# X1stFlrSF - First Floor square feet (cont)
numeric_description(train2$X1stFlrSF, 10)
# not normal distribution
analyze_outliers(train2,"X1stFlrSF")
train2 <- update_outliers_count(train2, "X1stFlrSF")

## X2ndFlrSF - Second floor square feet (cont)
numeric_description(train2$X2ndFlrSF, 10)
# not normal distribution
analyze_outliers(train2,"X2ndFlrSF")
train2 <- update_outliers_count(train2, "X2ndFlrSF")
# create a new variable if has second floor = 1 / not second floor = 0. 
train2$secondfloor<-0
train2$secondfloor[which(train2$X2ndFlrSF!=0)]<-1
train2$secondfloor<-factor(train2$secondfloor)
table(train2$secondfloor) 


## LowQualFinSF - Low quality finished square feet (all floors) (cont)
numeric_description(train2$LowQualFinSF, 10)
# not normal distribution
table(train2$LowQualFinSF)# very centered in 0 
analyze_outliers(train2,"LowQualFinSF")
train2 <- update_outliers_count(train2, "LowQualFinSF")

## GrLivArea - Above grade (ground) living area square feet (cont)
numeric_description(train2$GrLivArea, 20)
# not normal distribution
analyze_outliers(train2,"GrLivArea")
train2 <- update_outliers_count(train2, "GrLivArea")

## BsmtFullBath - # full baths in the basement (int)
summary(train2$BsmtFullBath) 
table(train2$BsmtFullBath)
sum(is.na(train2$BsmtFullBath))
barplot(table(train2$BsmtFullBath), main = "Distribution of Baths in the basement",xlab = "Number of Baths",col = "skyblue")
#train2$BsmtFullBath<- factor(train2$BsmtFullBath,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
#plot(train2$BsmtFullBath)

## BsmtHalfBath - # half baths in the basement (int)
summary(train2$BsmtHalfBath) 
table(train2$BsmtHalfBath) # very centered in 0
sum(is.na(train2$BsmtHalfBath))
barplot(table(train2$BsmtHalfBath), main = "Distribution of Half Baths in the basement",xlab = "Number of Baths",col = "skyblue")
#train2$BsmtHalfBath<- factor(train2$BsmtHalfBath,labels=c("0","1","2 or more"), order = T, levels=c(0,1,2))
#plot(train2$BsmtHalfBath)

## FullBath - full baths above grade (int)
summary(train2$FullBath) 
table(train2$FullBath)
sum(is.na(train2$FullBath))
barplot(table(train2$FullBath), main = "Distribution of Baths above grade",xlab = "Number of Baths",col = "skyblue")
#train2$FullBath<- factor(train2$FullBath,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
#plot(train2$FullBath)

## HalfBath - Half baths above grade (int)
summary(train2$HalfBath) 
table(train2$HalfBath)
sum(is.na(train2$HalfBath))
barplot(table(train2$HalfBath), main = "Distribution of Half Baths above grade",xlab = "Number of Baths",col = "skyblue")
#train2$HalfBath<- factor(train2$HalfBath,labels=c("0","1","2 or more"), order = T, levels=c(0,1,2))
#plot(train2$HalfBath)

## BedroomAbvGr - Bedrooms above grade (does NOT include basement bedrooms) (factor?)
summary(train2$BedroomAbvGr) 
table(train2$BedroomAbvGr)
sum(is.na(train2$BedroomAbvGr))
barplot(table(train2$BedroomAbvGr), main = "Distribution of Bedrooms above grade",xlab = "Number of Bedrooms",col = "skyblue")
#train2$BedroomAbvGr<- factor(train2$BedroomAbvGr,labels=c("0","1","2","3","4","5 or more"), order = T, levels=c(0,1,2,3,4,5))
#plot(train2$BedroomAbvGr)

## KitchenAbvGr - Kitchens above grade (int)
summary(train2$KitchenAbvGr) 
table(train2$KitchenAbvGr)
sum(is.na(train2$KitchenAbvGr))
barplot(table(train2$KitchenAbvGr), main = "Distribution of Kitchens",xlab = "Number of Kitchens",col = "skyblue")
#train2$KitchenAbvGr<- factor(train2$KitchenAbvGr,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
#plot(train2$KitchenAbvGr) # very centered variable 

####---------


### SILVIA's half
## "TotRmsAbvGrd" - Total rooms above grade (does not include bathrooms) - continuous ratio variable
summary(train2$TotRmsAbvGrd)
table(train2$TotRmsAbvGrd)
sum(is.na(train2$TotRmsAbvGrd)) #0
Boxplot(train2$TotRmsAbvGrd) # nose
length(Boxplot(train2$TotRmsAbvGrd, id = list(n=Inf))) #30
sevout_TotRmsAbvGrd = (quantile(train2$TotRmsAbvGrd,0.25)+(3*((quantile(train2$TotRmsAbvGrd,0.75)-quantile(train2$TotRmsAbvGrd,0.25)))))
length(which(train2$TotRmsAbvGrd > sevout_TotRmsAbvGrd))
#train2$f.TotRmsAbvGrd <- ifelse(df$mileage <= 5728, 1, ifelse(df$mileage > 5728 & df$mileage <= 16395, 2, ifelse(df$mileage > 16395 & df$mileage <= 33102, 3, ifelse(df$mileage > 33102, 4,0))))
#df$f.mileage <- factor(df$f.mileage,labels=c("LowMileage","LowMidMileage","HighMidMileage","HighMileage"), order = T, levels=c(1,2,3,4))
#table(df$f.mileage)
analyze_outliers(train2,"TotRmsAbvGrd")


## "Fireplaces" - Number of fireplaces (int)
summary(train2$Fireplaces)
table(train2$Fireplaces)
sum(is.na(train2$Fireplaces)) #0
analyze_outliers(train2, "Fireplaces") 
#train2$Fireplaces<- factor(train2$Fireplaces,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
barplot(table(train2$Fireplaces), main = "Distribution of fireplaces",xlab = "Number of Fireplaces",col = "skyblue")

## "GarageCars" - Size of garage in car capacity (int)
summary(train2$GarageCars)
table(train2$GarageCars)
sum(is.na(train2$GarageCars)) #0
#train2$GarageCars <- factor(train2$GarageCars,labels=c("0","1","2","3","4 or more"), order = T, levels=c(0,1,2,3,4))
analyze_outliers(train2, "GarageCars") 
barplot(table(train2$GarageCars), main = "Distribution of Garage Cars",xlab = "Number of Garage Cars",col = "skyblue")


## "GarageArea" - Size of garage in square feet (cont)
numeric_description(train2$GarageArea, 30)
# not normal distribution
analyze_outliers(train2, "GarageArea")
train2 <- update_outliers_count(train2, "GarageArea")
sm<-summary(train2$GarageArea)
train2$f.GarageArea <- ifelse(train2$GarageArea <= sm["1st Qu."], 1, ifelse(train2$GarageArea > sm["1st Qu."] & train2$GarageArea < sm["Median"], 2, ifelse(train2$GarageArea >= sm["Median"] & train2$GarageArea <= sm["3rd Qu."], 3, ifelse(train2$GarageArea > sm["3rd Qu."], 4,0))))
train2$f.GarageArea <- factor(train2$f.GarageArea,labels=c("LowgarageArea","LowMidgarageArea","HighMidgarageArea","HighgarageArea"), order = T, levels=c(1,2,3,4))
table(train2$f.GarageArea)
#checking si los 0 significan que no tienen garage
none_cars <- which(train$GarageCars == 0);none_cars
none_area <- which(train$GarageArea == 0);none_area
none_finish <- which(train$GarageFinish=='No Garage');none_finish

## "WoodDeckSF" - Wood deck area in square feet - continuous ratio variable
numeric_description(train2$WoodDeckSF, 30)
# not normal distribution
table(train2$WoodDeckSF) # centered data in 0
analyze_outliers(train2, "WoodDeckSF")
train2 <- update_outliers_count(train2, "WoodDeckSF")
sm<-summary(train2$WoodDeckSF)
train2$f.WoodDeckSF <- ifelse(train2$WoodDeckSF <= sm["1st Qu."], 1, ifelse(train2$WoodDeckSF > sm["1st Qu."] & train2$WoodDeckSF < sm["Median"], 2, ifelse(train2$WoodDeckSF >= sm["Median"] & train2$WoodDeckSF <= sm["3rd Qu."], 3, ifelse(train2$WoodDeckSF > sm["3rd Qu."], 4,0))))
train2$f.WoodDeckSF <- factor(train2$f.WoodDeckSF,labels=c("LowwoodDeckSF","LowMidwoodDeckSF","HighMidwoodDeckSF","HighwoodDeckSF"), order = T, levels=c(1,2,3,4))
table(train2$f.WoodDeckSF)
# LowMidwoodDeckSF 0 ???

## "OpenPorchSF" - Open porch area in square feet - continuous ratio variable (continua)
#numeric_description(train2$OpenPorchSF, 30)
#analyze_outliers(train2,"OpenPorchSF")
#train2 <- update_outliers_count(train2, "OpenPorchSF")
summary(train2$OpenPorchSF)
table(train2$OpenPorchSF) # very centered data in 0
sum(is.na(train2$OpenPorchSF))
barplot(table(train2$OpenPorchSF), main = "Distribution of ",xlab = "Number of ",col = "skyblue")
sm<-summary(train2$OpenPorchSF)
## faria millor un factor amb tenen porch si/no o bé incluir al d'abaix un q sigui no tenen porch. 
train2$f.OpenPorchSF <- ifelse(train2$OpenPorchSF <= sm["1st Qu."], 1, ifelse(train2$OpenPorchSF > sm["1st Qu."] & train2$OpenPorchSF < sm["Median"], 2, ifelse(train2$OpenPorchSF >= sm["Median"] & train2$OpenPorchSF <= sm["3rd Qu."], 3, ifelse(train2$OpenPorchSF > sm["3rd Qu."], 4,0))))
train2$f.OpenPorchSF <- factor(train2$f.OpenPorchSF,labels=c("LowopenPorchSF","LowMidopenPorchSF","HighMidopenPorchSFF","HighopenPorchSF"), order = T, levels=c(1,2,3,4))
table(train2$f.OpenPorchSF)

## EnclosedPorch - Enclosed porch area in square feet (cont)
summary(train2$EnclosedPorch)
table(train2$EnclosedPorch) # very centered data in 0
sum(is.na(train2$EnclosedPorch))
hist(train2$EnclosedPorch, breaks = 30, freq = F)
# therefore we will create a factor 0 if they dont have enclosed porch area,1 if they do have.
train2$EnclosedPorch_binary<-0
train2$EnclosedPorch_binary[which(train2$EnclosedPorch!=0)]<-1
train2$EnclosedPorch_binary
table(train2$EnclosedPorch_binary)

## X3SsnPorch - Three season porch area in square feet (cont)
numeric_description(train2$X3SsnPorch, 30)
table(train2$X3SsnPorch) # very centered in 0 -> dont have three season porch
analyze_outliers(train2,"X3SsnPorch")
train2 <- update_outliers_count(train2, "X3SsnPorch")


## ScreenPorch - Screen porch area in square feet (cont)
numeric_description(train2$ScreenPorch, 30)
# not normally distributed
table(train2$ScreenPorch) # very centered in 0 -> dont have screen porch
analyze_outliers(train2,"ScreenPorch")
train2 <- update_outliers_count(train2, "ScreenPorch")

## PoolArea - Pool area in square feet (cont)
numeric_description(train2$PoolArea, 30)
# not normally distributed
table(train2$PoolArea) # very centered in 0 -> dont have pool
analyze_outliers(train2,"PoolArea")
train2 <- update_outliers_count(train2, "PoolArea")
# create a new variable : 0=no pool/1= pool
train2$Pool_binary<-0
train2$Pool_binary[which(train2$PoolArea!=0)]<-1
train2$Pool_binary
table(train2$Pool_binary) # not very usefull , the majority dont have pools

## MiscVal - $Value of miscellaneous feature (cont)
numeric_description(train2$MiscVal, 30)
# not normally distributed
table(train2$MiscVal) # very centered in 0 
analyze_outliers(train2,"MiscVal")
train2 <- update_outliers_count(train2, "MiscVal")

## YrSold - (int)
numeric_description(train2$YrSold, 30)
# not normally distributed
table(train2$YrSold) 
analyze_outliers(train2,"YrSold")
train2 <- update_outliers_count(train2, "YrSold")

### categorical variables

## Neighborhood 
sum(is.na(train2$Neighborhood)) #0
table(train2$Neighborhood)
barplot(table(train2$Neighborhood), main = "Distribution of Neighborhood",xlab = "Number of Neighborhood",col = "skyblue")
#pie(table(train2$Neighborhood), main = "Distribution of Neighborhood", col = rainbow(length(levels(train2$Neighborhood))))

## ExterQual - Evaluates the quality of the material on the exterior 
sum(is.na(train2$ExterQual)) #0
table(train2$ExterQual)
barplot(table(train2$ExterQual), main = "Distribution of the exterior material quality",xlab = "Qualification",col = "skyblue")

## BsmtQual - Evaluates the height of the basement
sum(is.na(train2$BsmtQual)) #0
table(train2$BsmtQual)
barplot(table(train2$BsmtQual), main = "Distribution of Basement height Quality",xlab = "Qualification",col = "skyblue")
#pie(table(train2$BsmtQual), main = "Distribution of BsmtQual", col = rainbow(length(levels(train2$BsmtQual))))

## KitchenQual - Kitchen quality
sum(is.na(train2$KitchenQual)) #0
table(train2$KitchenQual)
barplot(table(train2$KitchenQual), main = "Distribution of Kitchen Quality",xlab = "Qualification",col = "skyblue")

## GarageFinish - Interior finish of the garage
sum(is.na(train2$GarageFinish)) #0
table(train2$GarageFinish)
barplot(table(train2$GarageFinish), main = "Garage Finish Distribution",xlab = "Type",col = "skyblue")

## FireplaceQu - Fireplace quality
sum(is.na(train2$FireplaceQu)) #0
table(train2$FireplaceQu)
barplot(table(train2$FireplaceQu), main = "Fireplace quality Distribution",xlab = "Qualification",col = "skyblue")

## Foundation - Type of foundation
sum(is.na(train2$Foundation)) #0
table(train2$Foundation)
barplot(table(train2$Foundation), main = "Type of foundation Distribution",xlab = "Type",col = "skyblue")

## GarageType - Garage location
sum(is.na(train2$GarageType)) #0
table(train2$GarageType)
barplot(table(train2$GarageType), main = "Garage Location Distribution",xlab = "Garage Location",col = "skyblue")

## MSSubClass
sum(is.na(train2$MSSubClass)) #0
table(train2$MSSubClass) # +classes
#barplot(table(train2$MSSubClass), main = "Garage Location Distribution",xlab = "Garage Location",col = "skyblue")


## NA's
skim(train2)
sum(is.na(train2$LotFrontage))
sum(is.na(train2$MasVnrArea))
sum(is.na(train2$GarageYrBlt)) # what do with this ?

## impute
# exclude GarageYrBlt feature because it has no sense to impute this variable

mice_imp<-mice(train2[, !names(train2) %in% "GarageYrBlt"],method = "cart")

#densityplot(train2)
imputed_data<-complete(mice_imp)
# LotFrontage validation
summary(imputed_data$LotFrontage)
summary(train2$LotFrontage)
plot(density(train2$LotFrontage,na.rm=TRUE))
plot(density(imputed_data$LotFrontage,na.rm=TRUE))
# imputation doesnt change much the density nor summary

# MasVnrArea validation
summary(imputed_data$MasVnrArea)
summary(train2$MasVnrArea)
plot(density(train2$MasVnrArea,na.rm=TRUE))
plot(density(imputed_data$MasVnrArea,na.rm=TRUE))
# imputation doesnt change much the density nor summary

### data quality exploration
# ORDRE QUE SEGUEIX A L'EXEMPLE
max(imputed_data$univ_outl_count) #9
imputed_data[which(imputed_data$univ_outl_count == 9),]
#df_of_interest = imputed_data[,c(2,3,5,7,8,9,18)] -- canviar nums columnes
#cor_outl = cor(df_of_interest)
#require(corrplot)
#par(mfrow=c(1,1))
#corrplot(cor_outl, method = 'number')

raredata<-imputed_data[which(train2$univ_outl_count>2),]
## fer lo dels NAS!!!
num_cols <- num_cols[num_cols != "GarageYrBlt"]
cor_outl <- cor(imputed_data[,c(num_cols)])
require(corrplot)

######################-----
# modeling
# RSS function
rss <- function(fitted, actual){
  sum((fitted - actual)^2)
}
## Variables selection
# example comparison between models
display(m0 <- lm(SalePrice ~ 1, data = train2))
display(m1 <- lm(SalePrice ~OverallQual , data = train2))
summary(m0)
rss(fitted(m0), train2$SalePrice)
as.matrix(AIC(m0, m1))
anova(m0,m1) # per veure quin model és millor

# variable selection all together
mod.fow <- stats::step(lm(SalePrice ~ ., data = train2), trace = FALSE,
                       direction = "forward")
summary(mod.fow)

## selection of the variables: LotArea, OverallQual, OverallCond, MasVnrArea, BsmtFinSF2
## X1stFlrSF, FullBath, GarageCars, BsmtQualGd


######-----
#Multivariate outliers
require(chemometrics)
#num_cols <- num_cols[num_cols != "GarageYrBlt"]
num_cols <- num_cols[num_cols != "Id"]
res.out <- Moutlier(imputed_data[,c(num_cols)], quantile = 0.9995, col="green")
# da error
#Esto generalmente significa que hay colinealidad perfecta o casi perfecta entre algunas de las variables en tus datos.
#La colinealidad perfecta ocurre cuando hay una relación lineal exacta entre dos o más variables, lo que puede causar problemas numéricos durante el cálculo.
cor_matrix <- cor(imputed_data[, c(num_cols)])
corrplot(cor_matrix, method = "color")
# veiem que X1stFlrSF està molt correlacionat amb TotalBsmtSF
# tambe que GarageArea està correlacionat amb GarageCars
cols <- c("X1stFlrSF", "GarageArea") #les que menys associació tenen amb la target variable
df_of_interest <- imputed_data[, !(colnames(imputed_data) %in% cols)]
num_cols <- num_cols[! num_cols %in% cols ]
res.out <- Moutlier(df_of_interest[,c(num_cols)], quantile = 0.9995, col="green")
cor_matrix <- cor(df_of_interest[, c(num_cols)])
corrplot(cor_matrix, method = "color")
# les que queden no tenen tanta correlacio, no se per quina es