##############################
######### Clean Data #########
##############################

data = read.csv('train.csv', stringsAsFactors=FALSE)
# summary(data)
# # Mean of lot frontage = 70.05
# 
# Replacing all NA's
data$LotFrontage[is.na(data$LotFrontage)] <- 70.05
data$Alley[is.na(data$Alley)] <- "NoAlley"
data$BsmtQual[is.na(data$BsmtQual)] <- 'NoBsmt'
data$BsmtCond[is.na(data$BsmtCond)] <- 'NoBsmt'
data$BsmtExposure[is.na(data$BsmtExposure)] <- 'NoBsmt'
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- 'NoBsmt'
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- 'NoBsmt'
data$FireplaceQu[is.na(data$FireplaceQu)] <- 'NoFirePlace'
data$GarageType[is.na(data$GarageType)] <- 'NoGarage'
data$GarageFinish[is.na(data$GarageFinish)] <- 'NoGarage'
data$GarageQual[is.na(data$GarageQual)] <- 'NoGarage'
data$GarageCond[is.na(data$GarageCond)] <- 'NoGarage'
data$GarageYrBlt[is.na(data$GarageYrBlt)] <- tail(names(sort(table(data$GarageYrBlt))), 2)[1]
data$PoolQC[is.na(data$PoolQC)] <- 'NoPool'
data$Fence[is.na(data$Fence)] <- 'NoFence'
data$MiscFeature[is.na(data$MiscFeature)] <- 'NoMisc'
data = na.exclude(data)

write.csv(data,"train2.csv")
data = read.csv("train2.csv", stringsAsFactors = TRUE)
# # Check for any NA's in the data frame
sapply(data, function(x) sum(is.na(x)))
 
 

#################################
##### Clean Test Data############
#################################

test=read.csv("test.csv", stringsAsFactors = FALSE)
test$LotFrontage[is.na(test$LotFrontage)] <- 70.05
test$Alley[is.na(test$Alley)] <- "NoAlley"
test$BsmtQual[is.na(test$BsmtQual)] <- 'NoBsmt'
test$BsmtCond[is.na(test$BsmtCond)] <- 'NoBsmt'
test$BsmtExposure[is.na(test$BsmtExposure)] <- 'NoBsmt'
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 'NoBsmt'
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 'NoBsmt'
test$FireplaceQu[is.na(test$FireplaceQu)] <- 'NoFirePlace'
test$GarageType[is.na(test$GarageType)] <- 'NoGarage'
test$GarageFinish[is.na(test$GarageFinish)] <- 'NoGarage'
test$GarageQual[is.na(test$GarageQual)] <- 'NoGarage'
test$GarageCond[is.na(test$GarageCond)] <- 'NoGarage'
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- tail(names(sort(table(test$GarageYrBlt))), 2)[1]
test$PoolQC[is.na(test$PoolQC)] <- 'NoPool'
test$Fence[is.na(test$Fence)] <- 'NoFence'
test$MiscFeature[is.na(test$MiscFeature)] <- 'NoMisc'
test = na.exclude(test)

# Save the clean data and reload the data
write.csv(test,"test2.csv")
test=read.csv("test2.csv", stringsAsFactors = TRUE)


sapply(test, function(x) sum(is.na(x)))


# Evaluation metric function
# Cite: https://www.kaggle.com/myonin/house-prices-advanced-regression-techniques/prediction-of-house-prices-3-methods
# I got the RMSE function code from above link
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}




################################
#### Applying Random Forest ####
################################

library(randomForest)
# Train
data.rf = randomForest(SalePrice~. ,data=data)
# Predict
data.rf.predict = predict(data.rf, test)
# RMSE
RMSE <- RMSE(data.rf.predict, test$SalePrice)
RMSE
# 0.06218295



##############################
##### Variable selection #####
##############################
varImpPlot(data.rf)
# Train
data.rf = randomForest(SalePrice~OverallQual+Neighborhood+GrLivArea+GarageCars+ExterQual+TotalBsmtSF ,data=data)
# Predict
data.rf.predict = predict(data.rf, test)
# RMSE
RMSE <- RMSE(data.rf.predict, test$SalePrice)
RMSE
# 0.09142286




# Tried SVM but couldn't make it work
# ################################
# ######## Applying SVM ##########
# ################################
# 
# library(e1071)
# data.svm = svm(SalePrice~. ,data)
# data.svm.preditResub <- predict(data.svm, data)
# points(data$SalePrice, data.svm.preditResub, col = "red")
# length(data$SalePrice)
# length(data.svm.preditResub)
# 
# 
