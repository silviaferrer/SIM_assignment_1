###### TEST DATA STANDARDIZATION

# Libraries
library(tidyverse)
library(skimr)
library(FactoMineR)

# Clear space
rm(list=ls())
par(mfrow=c(1,1))

# Load data
#getwd()
#setwd("C:/Users/Silvia/OneDrive - Universitat Politècnica de Catalunya/Escritorio/UPC/MASTER DS/1A/SIM/SIM_assignment_1")
setwd("/Users/ali/Desktop/MASTER/SIM/PROJECT 1")

test<-read.delim("test.csv", sep=',') 


# Preproces dataset

## recode incorrect NA's 
test$Alley[which(is.na(test$Alley))] <- "No alley access"
test$BsmtQual[which(is.na(test$BsmtQual))] <- "No Basement"
test$BsmtCond[which(is.na(test$BsmtCond))] <- "No Basement"
test$BsmtExposure[which(is.na(test$BsmtExposure))] <- "No Basement"
test$BsmtFinType1[which(is.na(test$BsmtFinType1))] <- "No Basement"
test$BsmtFinType2[which(is.na(test$BsmtFinType2))] <- "No Basement"
test$FireplaceQu[which(is.na(test$FireplaceQu))] <- "No Fireplace"
test$GarageType[which(is.na(test$GarageType))] <- "No Garage"
test$GarageFinish[which(is.na(test$GarageFinish))] <- "No Garage"
test$GarageCond[which(is.na(test$GarageCond))] <- "No Garage"
test$PoolQC[which(is.na(test$PoolQC))] <- "No Pool"
test$Fence[which(is.na(test$Fence))] <- "No Fence"
test$MiscFeature[which(is.na(test$MiscFeature))] <- "None"


## Recode character variables to factor
char_cols <- which(sapply(test, is.character))
test[, char_cols] <- lapply(test[, char_cols], as.factor) 

# Recode: numeric to factor
test$MSSubClass <- factor(test$MSSubClass)
test$MoSold <- factor(test$MoSold) # month 

# selection of 10 factor variables and numerical variables
#"Neighborhood" "ExterQual"    "BsmtQual"     "KitchenQual"  "GarageFinish"
#"FireplaceQu"  "Foundation"   "GarageType"   "MSSubClass"   "BsmtFinType1"
vect<- c("Neighborhood", "ExterQual", "BsmtQual", "KitchenQual", "GarageFinish", "FireplaceQu", "Foundation", "GarageType", "MSSubClass", "BsmtFinType1")
factor_test<-test[,c(vect)] # select the first 10 factor variables that are more related with the target
num_cols <- names(test)[sapply(test, is.numeric)]
test2 <- test[,c(num_cols, vect)]

## NEW FEATURES
# - EnclosedPorch_binary: 0 = NO enclosed porch area / 1 = they have
test2$EnclosedPorch_binary<-0
test2$EnclosedPorch_binary[which(test2$EnclosedPorch!=0)]<-1
test2$EnclosedPorch_binary<-as.factor(test2$EnclosedPorch_binary)

# - OpenPorch_binary: 0 = NO open porch area / 1 = they have
test2$OpenPorch_binary<-0
test2$OpenPorch_binary[which(test2$OpenPorchSF!=0)]<-1
test2$OpenPorch_binary<-as.factor(test2$OpenPorch_binary)

# - HasPorch_binary: 0 = NO porch area / 1 = they have (mixing both binary variables)
test2$HasPorch_binary<-0
test2$HasPorch_binary[which(test2$EnclosedPorch!=0 | test2$OpenPorchSF!=0)]<-1
test2$HasPorch_binary<-as.factor(test2$HasPorch_binary)

# - Pool_binary: 0 = no pool / 1 = pool
test2$Pool_binary<-0
test2$Pool_binary[which(test2$PoolArea!=0)]<-1
test2$Pool_binary<-as.factor(test2$Pool_binary)

# - secondfloor: 0 = not second floor / 1 = has second floor 
test2$secondfloor<-0
test2$secondfloor[which(test2$X2ndFlrSF!=0)]<-1
test2$secondfloor<-factor(test2$secondfloor)

# - univ_outl_count
test2$univ_outl_count <- 0 # initialize
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

columns_to_exclude <- c("Id","univ_outl_count")

# apply the function to each column
for (column_name in colnames(test2)) {
  if (!(column_name %in% columns_to_exclude) && is.numeric(test2[[column_name]])) {
    test2 <- update_outliers_count(test2, column_name)
  }
}

mice_imp<-mice(test2[, !names(test2) %in% "GarageYrBlt"],method = "cart")
imputed_test<-complete(mice_imp)

#plot_missing(imputed_data, missing_only = TRUE, group = list("Low" = 0.05, "Medium"=0.25, "High"=0.5,
#                                                    "Very High" =1), geom_label_args = list("size" = 2))



predictions <- predict(final_model, imputed_test)
predictions <- exp(predictions)
