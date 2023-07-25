library(MASS)
install.packages("splines")
library(splines)
train <- read.csv("train.csv")

train$MSZoning <- as.factor(train$MSZoning)
train$LotFrontage[is.na(train$LotFrontage)] <- round(mean(train$LotFrontage, na.rm = TRUE)) #replace NA's with mean of rest of data
train$Street <- as.factor(train$Street)
train$Alley[is.na(train$Alley)] <- "No Alley Access"
train$Alley <- as.factor(train$Alley)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)
train$Utilities <- as.factor(train$Utilities)
train$LotConfig <- as.factor(train$LotConfig)
train$LandSlope <- as.factor(train$LandSlope)
train$Neighborhood <- as.factor(train$Neighborhood)
train$Condition1 <- as.factor(train$Condition1)
train$Condition2 <- as.factor(train$Condition2)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$MasVnrType[is.na(train$MasVnrType)] <- "None"
train$MasVnrType <- as.factor(train$MasVnrType)
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$ExterQual <- as.factor(train$ExterQual)
train$ExterCond <- as.factor(train$ExterCond)
train$Foundation <- as.factor(train$Foundation)
train$BsmtQual[is.na(train$BsmtQual)] <- "NB" # NB = 'No Basement'
train$BsmtQual <- as.factor(train$BsmtQual) 
train$BsmtCond[is.na(train$BsmtCond)] <- "NB" # NB = 'No Basement'
train$BsmtCond <- as.factor(train$BsmtCond) 
train$BsmtExposure[is.na(train$BsmtExposure)] <- "NB" # NB = 'No Basement'
train$BsmtExposure <- as.factor(train$BsmtExposure) 
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "NB" # NB = 'No Basement'
train$BsmtFinType1 <- as.factor(train$BsmtFinType1) 
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "NB" # NB = 'No Basement'
train$BsmtFinType2 <- as.factor(train$BsmtFinType2) 
train$Heating <- as.factor(train$Heating) 
train$HeatingQC <- as.factor(train$HeatingQC) 
train$CentralAir<- as.factor(train$CentralAir) 
train <- train[!is.na(train$Electrical),] # Remove a row :(
train$Electrical<- as.factor(train$Electrical) 
train$KitchenQual<- as.factor(train$KitchenQual) 
train$Functional<- as.factor(train$Functional) 
train$FireplaceQu[is.na(train$FireplaceQu)] <- "NF" # NF = No Fireplace
train$FireplaceQu <- as.factor(train$FireplaceQu)
train$GarageType[is.na(train$GarageType)] <- "NG" # NG = No Garage
train$GarageType <- as.factor(train$GarageType)
# ASK ABOUT GARAGE YEAR BUILT
train$GarageFinish[is.na(train$GarageFinish)] <- "NG" # NG = No Garage
train$GarageFinish <- as.factor(train$GarageFinish)
train$GarageQual[is.na(train$GarageQual)] <- "NG" # NG = No Garage
train$GarageQual <- as.factor(train$GarageQual)
train$GarageCond[is.na(train$GarageCond)] <- "NG" # NG = No Garage
train$GarageCond <- as.factor(train$GarageCond)
train$PavedDrive <- as.factor(train$PavedDrive)
train$PoolQC[is.na(train$PoolQC)] <- "NP" # NG = No Pool
train$PoolQC <- as.factor(train$PoolQC)
train$Fence[is.na(train$Fence)] <- "NF" # NG = No Fence
train$Fence <- as.factor(train$Fence)
train$MiscFeature[is.na(train$MiscFeature)] <- "None" 
train$MiscFeature <- as.factor(train$MiscFeature)
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)

train <- subset(train, select = -c(GarageYrBlt, Id))

test <- read.csv("test_new.csv")

test$MSZoning <- as.factor(test$MSZoning)
test$LotFrontage[is.na(test$LotFrontage)] <- round(mean(test$LotFrontage, na.rm = TRUE)) #replace NA's with mean of rest of data
test$Street <- as.factor(test$Street)
test$Alley[is.na(test$Alley)] <- "No Alley Access"
test$Alley <- as.factor(test$Alley)
test$LotShape <- as.factor(test$LotShape)
test$LandContour <- as.factor(test$LandContour)
test <- test[!is.na(test$Utilities),] # Remove a row :(
test$Utilities <- factor(test$Utilities, levels=levels(train$Utilities))
test$LotConfig <- as.factor(test$LotConfig)
test$LandSlope <- as.factor(test$LandSlope)
test$Neighborhood <- as.factor(test$Neighborhood)
test$Condition1 <- as.factor(test$Condition1)
test$Condition2 <- factor(test$Condition2, levels=levels(train$Condition2))
test$BldgType <- as.factor(test$BldgType)
test$HouseStyle <- factor(test$HouseStyle, levels=levels(train$HouseStyle))
test$RoofStyle <- as.factor(test$RoofStyle)
test$RoofMatl <- factor(test$RoofMatl, levels=levels(train$RoofMatl))
test$Exterior1st <- factor(test$Exterior1st, levels=levels(train$Exterior1st))
test$Exterior2nd <- factor(test$Exterior2nd, levels=levels(train$Exterior2nd))
test$MasVnrType[is.na(test$MasVnrType)] <- "None"
test$MasVnrType <- as.factor(test$MasVnrType)
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$ExterQual <- as.factor(test$ExterQual)
test$ExterCond <- as.factor(test$ExterCond)
test$Foundation <- as.factor(test$Foundation)
test$BsmtQual[is.na(test$BsmtQual)] <- "NB" # NB = 'No Basement'
test$BsmtQual <- as.factor(test$BsmtQual) 
test$BsmtCond[is.na(test$BsmtCond)] <- "NB" # NB = 'No Basement'
test$BsmtCond <- as.factor(test$BsmtCond) 
test$BsmtExposure[is.na(test$BsmtExposure)] <- "NB" # NB = 'No Basement'
test$BsmtExposure <- as.factor(test$BsmtExposure) 
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "NB" # NB = 'No Basement'
test$BsmtFinType1 <- as.factor(test$BsmtFinType1) 
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "NB" # NB = 'No Basement'
test$BsmtFinType2 <- as.factor(test$BsmtFinType2) 
test$Heating <- factor(test$Heating, levels=levels(train$Heating))
test$HeatingQC <- as.factor(test$HeatingQC) 
test$CentralAir<- as.factor(test$CentralAir) 
test$Electrical <- factor(test$Electrical, levels=levels(train$Electrical))
test <- test[!is.na(test$BsmtFullBath),] # Remove a row :(
test <- test[!is.na(test$KitchenQual),] # Remove a row :(
test$KitchenQual<- as.factor(test$KitchenQual) 
test <- test[!is.na(test$Functional),] # Remove a row :(
test$Functional<- as.factor(test$Functional) 
test$FireplaceQu[is.na(test$FireplaceQu)] <- "NF" # NF = No Fireplace
test$FireplaceQu <- as.factor(test$FireplaceQu)
test$GarageType[is.na(test$GarageType)] <- "NG" # NG = No Garage
test$GarageType <- as.factor(test$GarageType)
# ASK ABOUT GARAGE YEAR BUILT
test$GarageFinish[is.na(test$GarageFinish)] <- "NG" # NG = No Garage
test$GarageFinish <- as.factor(test$GarageFinish)
test$GarageQual[is.na(test$GarageQual)] <- "NG" # NG = No Garage
test$GarageQual <- factor(test$GarageQual, levels=levels(train$GarageQual))
test$GarageCond[is.na(test$GarageCond)] <- "NG" # NG = No Garage
test$GarageCond <- as.factor(test$GarageCond)
test$PavedDrive <- as.factor(test$PavedDrive)
test$PoolQC[is.na(test$PoolQC)] <- "NP" # NG = No Pool
test$PoolQC <- factor(test$PoolQC, levels=levels(train$PoolQC))
test$Fence[is.na(test$Fence)] <- "NF" # NG = No Fence
test$Fence <- as.factor(test$Fence)
test$MiscFeature[is.na(test$MiscFeature)] <- "None" 
test$MiscFeature <- factor(test$MiscFeature, levels=levels(train$MiscFeature))
test <- test[!is.na(test$SaleType),] # Remove a row :(
test$SaleType <- as.factor(test$SaleType)
test$SaleCondition <- as.factor(test$SaleCondition)

test <- subset(test, select = -c(GarageYrBlt, Id))

