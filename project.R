###### SIM PROJECT

# Libraries
library(tidyverse)
library(skimr)
library(FactoMineR)
library(car)

# Clear space
rm(list=ls())
par(mfrow=c(1,1))

# Load data
#getwd()
#setwd("C:/Users/Silvia/OneDrive - Universitat Politècnica de Catalunya/Escritorio/UPC/MASTER DS/1A/SIM/assignment 1")
setwd("/Users/ali/Desktop/MASTER/SIM/PROJECT 1")
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
train$GarageYrBlt<-factor(train$GarageYrBlt) 
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

## "LotFrontage" Linear feet of street connected to property (num)
summary(train2$LotFrontage)
sum(is.na(train2$LotFrontage))
hist(train2$LotFrontage, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$LotFrontage), sd(train2$LotFrontage)), add = T)
shapiro.test(train2$LotFrontage) # not normal
analyze_outliers(train2, "LotFrontage")
train2 <- update_outliers_count(train2, "LotFrontage")

## "LotArea" Lot size in square feet (num)
summary(train2$LotArea)
sum(is.na(train2$LotArea))
shapiro.test(train2$LotArea)
hist(train2$LotArea, breaks = 20, freq = F)
curve(dnorm(x, mean(train2$LotArea), sd(train2$LotArea)), add = T)
analyze_outliers(train2, "LotArea")
train2 <- update_outliers_count(train2, "LotArea")

## "OverallQual"
summary(train2$OverallQual)
sum(is.na(train2$OverallQual))
hist(train2$OverallQual, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$OverallQual), sd(train2$OverallQual)), add = T)
shapiro.test(train2$OverallQual)
analyze_outliers(train2, "OverallQual")
train2 <- update_outliers_count(train2, "OverallQual")

## "YearBuilt"
summary(train2$YearBuilt)
hist(train2$YearBuilt, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$YearBuilt), sd(train2$YearBuilt)), add = T)
sum(is.na(train2$YearBuilt))
analyze_outliers(train2, "YearBuilt")
train2 <- update_outliers_count(train2, "YearBuilt")

## "YearRemodAdd"
summary(train2$YearRemodAdd)
hist(train2$YearRemodAdd, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$YearRemodAdd), sd(train2$YearRemodAdd)), add = T)
sum(is.na(train2$YearRemodAdd))
analyze_outliers(train2, "YearRemodAdd")
train2 <- update_outliers_count(train2, "YearRemodAdd")

## MasVnrArea - Masonry veneer area in square feet
summary(train2$MasVnrArea)
hist(train2$MasVnrArea, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$MasVnrArea), sd(train2$MasVnrArea)), add = T)
sum(is.na(train2$MasVnrArea))  # 8 NAs
shapiro.test(train$MasVnrArea) # Not normal
analyze_outliers(train2,"MasVnrArea")
train2 <- update_outliers_count(train2, "MasVnrArea")

## "MasVnrArea" - Masonry veneer area in square feet - continuous ratio variable
# mirar ?
# els 0 poden ser veritat
# comparar amb variable MasVnrType
# si el type != None i es 0 -> convertir en NA
train2$MasVnrArea[which(train2$MasVnrArea == 0)] <- NA
none_idx <- which(train$MasVnrType == 'None')
train2$MasVnrArea[none_idx] <- 0
Boxplot(train2$MasVnrArea)
sm<-summary(train2$MasVnrArea)
iqr <- sm["3rd Qu."]-sm["1st Qu."]; iqr
train2$f.MasVnrArea <- ifelse(train2$MasVnrArea <= sm["1st Qu."], 1, ifelse(train2$MasVnrArea > sm["1st Qu."] & train2$MasVnrArea < sm["Median"], 2, ifelse(train2$MasVnrArea >= sm["Median"] & train2$MasVnrArea <= sm["3rd Qu."], 3, ifelse(train2$MasVnrArea > sm["3rd Qu."], 4,0))))
train2$f.MasVnrArea <- factor(train2$f.MasVnrArea,labels=c("LowmasVnrArea","LowMidmasVnrArea","HighMidmasVnrArea","HighmasVnrArea"), order = T, levels=c(1,2,3,4))
table(train2$f.MasVnrArea)


## "BsmtFinSF1" - Type 1 finished square feet
summary(train2$BsmtFinSF1)
sum(is.na(train2$BsmtFinSF1))
hist(train2$BsmtFinSF1, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$BsmtFinSF1), sd(train2$BsmtFinSF1)), add = T)
shapiro.test(train2$BsmtFinSF1)
analyze_outliers(train2,"BsmtFinSF1")
train2 <- update_outliers_count(train2, "BsmtFinSF1")


# BsmtFinSF2 - Rating of basement finished area (if multiple types)
summary(train2$BsmtFinSF2)
sum(is.na(train2$BsmtFinSF2))
hist(train2$BsmtFinSF2, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$BsmtFinSF2), sd(train2$BsmtFinSF2)), add = T)
shapiro.test(train2$BsmtFinSF2)
analyze_outliers(train2,"BsmtFinSF2")
train2 <- update_outliers_count(train2, "BsmtFinSF2")

# "BsmtUnfSF"
summary(train2$BsmtUnfSF)
sum(is.na(train2$BsmtUnfSF))
hist(train2$BsmtUnfSF, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$BsmtUnfSF), sd(train2$BsmtUnfSF)), add = T)
shapiro.test(train2$BsmtUnfSF)
analyze_outliers(train2,"BsmtUnfSF")
train2 <- update_outliers_count(train2, "BsmtUnfSF")

# "TotalBsmtSF"
summary(train2$TotalBsmtSF)
sum(is.na(train2$TotalBsmtSF))
hist(train2$TotalBsmtSF, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$TotalBsmtSF), sd(train2$TotalBsmtSF)), add = T)
shapiro.test(train2$TotalBsmtSF)
analyze_outliers(train2,"TotalBsmtSF")
train2 <- update_outliers_count(train2, "TotalBsmtSF")


# X1stFlrSF - First Floor square feet
summary(train2$X1stFlrSF)
sum(is.na(train2$X1stFlrSF))
hist(train2$X1stFlrSF, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$X1stFlrSF), sd(train2$X1stFlrSF)), add = T)
shapiro.test(train2$X1stFlrSF)
analyze_outliers(train2,"X1stFlrSF")
train2 <- update_outliers_count(train2, "X1stFlrSF")

## X2ndFlrSF - Second floor square feet
summary(train2$X2ndFlrSF)
sum(is.na(train2$X2ndFlrSF))
hist(train2$X2ndFlrSF, breaks = 10, freq = F)
# doesnt make sense to have 0 square feet floor -> reconvert to NA/ doesnt have a second floor.
curve(dnorm(x, mean(train2$X2ndFlrSF), sd(train2$X2ndFlrSF)), add = T)
shapiro.test(train2$X2ndFlrSF)
analyze_outliers(train2,"X2ndFlrSF")
train2 <- update_outliers_count(train2, "X2ndFlrSF")
# create a new variable if has second floor = 1 / not second floor = 0. 
train2$secondfloor<-0
train2$secondfloor[which(train2$X2ndFlrSF!=0)]<-1
train2$secondfloor<-factor(train2$secondfloor)
table(train2$secondfloor) 


## LowQualFinSF - Low quality finished square feet (all floors)
summary(train2$LowQualFinSF)
table(train2$LowQualFinSF)# very centered in 0 
sum(is.na(train2$LowQualFinSF))
hist(train2$LowQualFinSF, breaks = 10, freq = F)
curve(dnorm(x, mean(train2$LowQualFinSF), sd(train2$LowQualFinSF)), add = T)
shapiro.test(train2$LowQualFinSF)
analyze_outliers(train2,"LowQualFinSF")
train2 <- update_outliers_count(train2, "LowQualFinSF")

## GrLivArea - Above grade (ground) living area square feet # m2 habitables per sobre del nivell 0
summary(train2$GrLivArea) 
sum(is.na(train2$GrLivArea))
hist(train2$GrLivArea, breaks = 20, freq = F)
curve(dnorm(x, mean(train2$GrLivArea), sd(train2$GrLivArea)), add = T)
shapiro.test(train2$GrLivArea)
analyze_outliers(train2,"GrLivArea")
train2 <- update_outliers_count(train2, "GrLivArea")

## BsmtFullBath - # full baths in the basement (factor?)
summary(train2$BsmtFullBath) 
table(train2$BsmtFullBath)
sum(is.na(train2$BsmtFullBath))
#train2$BsmtFullBath<- factor(train2$BsmtFullBath,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
#plot(train2$BsmtFullBath)

## BsmtHalfBath - # half baths in the basement (factor?)
summary(train2$BsmtHalfBath) 
table(train2$BsmtHalfBath)
sum(is.na(train2$BsmtHalfBath))
#train2$BsmtHalfBath<- factor(train2$BsmtHalfBath,labels=c("0","1","2 or more"), order = T, levels=c(0,1,2))
#plot(train2$BsmtHalfBath)

## FullBath - full baths above grade (factor?)
summary(train2$FullBath) 
table(train2$FullBath)
sum(is.na(train2$FullBath))
#train2$FullBath<- factor(train2$FullBath,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
#plot(train2$FullBath)

## HalfBath - Half baths above grade (factor?)
summary(train2$HalfBath) 
table(train2$HalfBath)
sum(is.na(train2$HalfBath))
#train2$HalfBath<- factor(train2$HalfBath,labels=c("0","1","2 or more"), order = T, levels=c(0,1,2))
#plot(train2$HalfBath)

## BedroomAbvGr - Bedrooms above grade (does NOT include basement bedrooms) (factor?)
summary(train2$BedroomAbvGr) 
table(train2$BedroomAbvGr)
sum(is.na(train2$BedroomAbvGr))
#train2$BedroomAbvGr<- factor(train2$BedroomAbvGr,labels=c("0","1","2","3","4","5 or more"), order = T, levels=c(0,1,2,3,4,5))
#plot(train2$BedroomAbvGr)

## KitchenAbvGr - Kitchens above grade (factor?)
summary(train2$KitchenAbvGr) 
table(train2$KitchenAbvGr)
sum(is.na(train2$KitchenAbvGr))
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

## "Fireplaces" - Number of fireplaces - continuous ratio variable (discreta/categorica)
summary(train2$Fireplaces)
table(train2$Fireplaces)
sum(is.na(train2$Fireplaces)) #0
Boxplot(train2$Fireplaces) # nose si te sentit el boxplot aqui
length(Boxplot(train2$Fireplaces, id = list(n=Inf))) #30
sevout_Fireplaces = (quantile(train2$Fireplaces,0.25)+(3*((quantile(train2$Fireplaces,0.75)-quantile(train2$Fireplaces,0.25)))))
length(which(train2$Fireplaces > sevout_Fireplaces))
train2$Fireplaces<- factor(train2$Fireplaces,labels=c("0","1","2","3 or more"), order = T, levels=c(0,1,2,3))
table(train2$Fireplaces)
barplot(table(train2$Fireplaces), main = "Distribution of fireplaces",xlab = "Number of Fireplaces",col = "skyblue")

## "GarageCars" - Size of garage in car capacity - continuous ratio variable (factor?)
summary(train2$GarageCars)
table(train2$GarageCars)
sum(is.na(train2$GarageCars)) #0
#train2$GarageCars <- factor(train2$GarageCars,labels=c("0","1","2","3","4 or more"), order = T, levels=c(0,1,2,3,4))
table(train2$GarageCars)
#barplot(train2$f.GarageCars)
analyze_outliers(train2, "GarageCars") 

## "GarageArea" - Size of garage in square feet - continuous ratio variable (continua)
summary(train2$GarageArea)
hist(train2$GarageArea, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$GarageArea), sd(train2$GarageArea)), add = T)
shapiro.test(train2$GarageArea) # not normally distributed -> p-value = 4.017e-15 
sum(is.na(train2$GarageArea)) #0
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
summary(train2$WoodDeckSF)
table(train2$WoodDeckSF) # centered data in 0
hist(train2$WoodDeckSF, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$WoodDeckSF), sd(train2$WoodDeckSF)), add = T)
shapiro.test(train2$WoodDeckSF) # not normally distributed -> p-value < 2.2e-16
sum(is.na(train2$WoodDeckSF)) #0
analyze_outliers(train2, "WoodDeckSF")
train2 <- update_outliers_count(train2, "WoodDeckSF")
sm<-summary(train2$WoodDeckSF)
train2$f.WoodDeckSF <- ifelse(train2$WoodDeckSF <= sm["1st Qu."], 1, ifelse(train2$WoodDeckSF > sm["1st Qu."] & train2$WoodDeckSF < sm["Median"], 2, ifelse(train2$WoodDeckSF >= sm["Median"] & train2$WoodDeckSF <= sm["3rd Qu."], 3, ifelse(train2$WoodDeckSF > sm["3rd Qu."], 4,0))))
train2$f.WoodDeckSF <- factor(train2$f.WoodDeckSF,labels=c("LowwoodDeckSF","LowMidwoodDeckSF","HighMidwoodDeckSF","HighwoodDeckSF"), order = T, levels=c(1,2,3,4))
table(train2$f.WoodDeckSF)
# LowMidwoodDeckSF 0 ???

## "OpenPorchSF" - Open porch area in square feet - continuous ratio variable (continua)
summary(train2$OpenPorchSF)
table(train2$OpenPorchSF) # very centered data in 0
hist(train2$OpenPorchSF, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$OpenPorchSF), sd(train2$OpenPorchSF)), add = T)
shapiro.test(train2$OpenPorchSF) # not normally distributed -> p-value < 2.2e-16
sum(is.na(train2$OpenPorchSF)) #0
analyze_outliers(train2,"OpenPorchSF")
train2 <- update_outliers_count(train2, "OpenPorchSF")
sm<-summary(train2$OpenPorchSF)
## faria millor un factor amb tenen porch si/no o bé incluir al d'abaix un q sigui no tenne porch. 
train2$f.OpenPorchSF <- ifelse(train2$OpenPorchSF <= sm["1st Qu."], 1, ifelse(train2$OpenPorchSF > sm["1st Qu."] & train2$OpenPorchSF < sm["Median"], 2, ifelse(train2$OpenPorchSF >= sm["Median"] & train2$OpenPorchSF <= sm["3rd Qu."], 3, ifelse(train2$OpenPorchSF > sm["3rd Qu."], 4,0))))
train2$f.OpenPorchSF <- factor(train2$f.OpenPorchSF,labels=c("LowopenPorchSF","LowMidopenPorchSF","HighMidopenPorchSFF","HighopenPorchSF"), order = T, levels=c(1,2,3,4))
table(train2$f.OpenPorchSF)

## EnclosedPorch - Enclosed porch area in square feet
summary(train2$EnclosedPorch)
table(train2$EnclosedPorch) # very centered data in 0
sum(is.na(train2$EnclosedPorch))
barplot(table(train2$EnclosedPorch), main = "Distribution of ",xlab = "Number of ",col = "skyblue")
# very centered data in 0
# therefore we will create a factor 0 if they dont have enclosed porch area,1 if they do have.
train2$EnclosedPorch_binary<-0
train2$EnclosedPorch_binary[which(train2$EnclosedPorch!=0)]<-1
train2$EnclosedPorch_binary
table(train2$EnclosedPorch_binary)

## X3SsnPorch - Three season porch area in square feet (int)
summary(train2$X3SsnPorch)
sum(is.na(train2$X3SsnPorch)) #0
table(train2$X3SsnPorch) # very centered in 0 -> dont have three season porch
barplot(table(train2$X3SsnPorch), main = "Distribution of ",xlab = "Number of ",col = "skyblue")
# very centered variable


## ScreenPorch - Screen porch area in square feet
summary(train2$ScreenPorch)
sum(is.na(train2$ScreenPorch)) #0
table(train2$ScreenPorch) # very centered in 0 -> dont have screen porch
hist(train2$ScreenPorch, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$ScreenPorch), sd(train2$ScreenPorch)), add = T)
shapiro.test(train2$ScreenPorch) # not normally distributed -> p-value < 2.2e-16
analyze_outliers(train2,"ScreenPorch")

## PoolArea - Pool area in square feet
summary(train2$PoolArea)
sum(is.na(train2$PoolArea)) #0
table(train2$PoolArea) # very centered in 0 -> dont have pool
hist(train2$PoolArea, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$PoolArea), sd(train2$PoolArea)), add = T)
shapiro.test(train2$PoolArea) # not normally distributed -> p-value < 2.2e-16
analyze_outliers(train2,"PoolArea")

## MiscVal - $Value of miscellaneous feature
summary(train2$MiscVal)
sum(is.na(train2$MiscVal)) #0
table(train2$MiscVal) # very centered in 0 -> dont have pool
hist(train2$MiscVal, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$MiscVal), sd(train2$MiscVal)), add = T)
shapiro.test(train2$MiscVal) # not normally distributed -> p-value < 2.2e-16
analyze_outliers(train2,"MiscVal")

## YrSold - 
summary(train2$YrSold)
sum(is.na(train2$YrSold)) #0
table(train2$YrSold) 
hist(train2$YrSold, breaks = 30, freq = F)
curve(dnorm(x, mean(train2$YrSold), sd(train2$YrSold)), add = T)
shapiro.test(train2$YrSold) # not normally distributed -> p-value < 2.2e-16
analyze_outliers(train2,"YrSold")

## Neighborhood (categorical)
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
## impute? 

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
