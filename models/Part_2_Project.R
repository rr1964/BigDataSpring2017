############################################
##########Various Methods. Part 2 of Project.


#
# This function performs 10-fold crossvalidation for regression
#
# It assumes that the predictor variables are in the (nxp)-matrix 
# (or data frame) x and the response is in the n-vector y 
#
# The fitting method must use a formula interface and must 
# have a "predict" function
# optional arguments can be passed to "method" and "predict.method" using "..."
#
tenfold <- function(x, y, nfold=10, method, ...){
  myfun = match.fun(method)
  n = nrow(x)
  if(length(y) != n) stop("Error: x and y do not contain the same number of observations")
  #
  # ifold chooses a "fold" for each observation
  # if ifold[i]=1 then observation i is in fold 1
  # if ifold[i]=2 then observation i is in fold 2
  # etc
  #
  
  
  
  ifold = sample(rep(1:nfold, length=n))
  yhat = y
  for( j in 1:nfold ){
    #
    # the obs in the j-th fold are in the test set,
    # the rest are in the training set: 
    #
    intrain = (1:n)[ifold != j]
    intest = (1:n)[ifold == j]
    trainx = x[intrain,]
    testx  = x[intest,]
    trainy = y[intrain]
    testy = y[intest]
    mytrain = data.frame(cbind(trainx, yy=trainy))
    mytest = data.frame(cbind(testx, yy=testy))
    
    totalData <- rbind(mytrain, mytest)
    for (f in 1:length(names(totalData))) {
      levels(mytrain[, f]) <- levels(totalData[, f])
    }
    
    fitted = myfun(yy ~ ., data=mytrain, ...)
    yhat[intest] = predict(fitted, newdata=mytest, ...)
  }
  z = (y - yhat)^2 
  mse = mean(z)
  return(list(MSE = mse, Yhat = yhat))
}
#

library(ISLR)
library(rpart)
library(randomForest)
library(gbm)
library(nnet)
library(dplyr)
library(data.table)
library(caret)

####Import the data sans the first column (ID)
train <- fread("C:/Users/Yuanzhi Li/Desktop/Big Data/train.csv", na = "NA")[,-1]
test <- fread("C:/Users/Yuanzhi Li/Desktop/Big Data/test.csv", na = "NA")[,-1]

table(train$Utilities)
####train[,1]

summary(train)
# Mean of lot frontage = 70.05
length(train$"1stFlrSF")
# Replacing all NA's that are not just missing values (i.e. NA actually has a meaning, usually "no_____")
train$LotFrontage[is.na(train$LotFrontage)] <- 70.05
train$Alley[is.na(train$Alley)] <- "NoAlley"
train$BsmtQual[is.na(train$BsmtQual)] <- 'NoBsmt'
train$BsmtCond[is.na(train$BsmtCond)] <- 'NoBsmt'
train$BsmtExposure[is.na(train$BsmtExposure)] <- 'NoBsmt'
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'NoBsmt'
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 'NoBsmt'
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'NoFirePlace'
train$GarageType[is.na(train$GarageType)] <- 'NoGarage'
train$GarageFinish[is.na(train$GarageFinish)] <- 'NoGarage'
train$GarageQual[is.na(train$GarageQual)] <- 'NoGarage'
train$GarageCond[is.na(train$GarageCond)] <- 'NoGarage'
train$PoolQC[is.na(train$PoolQC)] <- 'NoPool'
train$Fence[is.na(train$Fence)] <- 'NoFence'
train$MiscFeature[is.na(train$MiscFeature)] <- 'NoMisc'


cleanTrain <-train

######Us the following code to detect NA values remaining in the dataset. 
countNA <- function(column)
{
  totalNA = sum(is.na(column))
  
  return(totalNA)
}


NAcountVect = apply(train,MARGIN = 2, countNA)
NAcountVect = apply(cleanTrain,MARGIN = 2, countNA)

sort(NAcountVect, decreasing = TRUE)
NAcountVect[NAcountVect > 0]
summary(cleanTrain$LotFrontage)

##The mean and the median are almost identical. It seems reasonable to impute with the mean. 
boxplot(cleanTrain$LotFrontage)

######Since the data for Masonry Veneer Area is heavily skewed, we will impute the 8 NA with the median. 
######This also corresponds with our decision to treat these NA as indicating "no masonry veneer."
summary(cleanTrain$MasVnrArea)
boxplot(cleanTrain$MasVnrArea)
cleanTrain$MasVnrArea[is.na(cleanTrain$MasVnrArea)] = 0

###We find that the 8 NA values for masonry correspond to each other (both are missing at the same time).
sum(is.na(train$MasVnrArea) * is.na(cleanTrain$MasVnrType)) ###equals 8.

table(cleanTrain$MasVnrType)
###It seems most reasonable to treat the NA values for Masonry veneer type as "none."
VnrTypeNA = is.na(cleanTrain$MasVnrType)
cleanTrain$MasVnrType[VnrTypeNA] = "None"

###The missing basemnet values all correspond with one another. 
sum(is.na(train$BsmtCond) * is.na(train$BsmtQual) * is.na(train$BsmtExposure) * is.na(train$BsmtFinType1) * is.na(train$BsmtFinType2))


table(train$Electrical)
###By far and large, the houses have Standard Breaker as their electrical wiring type. The single missing value is changed to "SBrkr".
which(is.na(cleanTrain$Electrical))
cleanTrain$Electrical[1380] = "SBrkr" 
table(cleanTrain$Electrical)


#########Handing the Garage Year Built NA
######### There are 81 houses without garages. 
######### The only sort of reasonable way to impute the NAs for Garage Year was just to put in the year the house was built.
###### We also considered just leaving these NA as is.

GarageYrNA = which(is.na(cleanTrain$GarageYrBlt))
cleanTrain$GarageYrBlt[GarageYrNA] = cleanTrain$YearBuilt[GarageYrNA]


#######Fitting the linear model using elastic net.

#####Set up the model and denote the factor variables. 
attach(cleanTrain)
names(cleanTrain)
dim(cleanTrain)

length(`1stFlrSF`)


sum(sapply(cleanTrain, function(x){ sum(is.na(x))}))


##########



test <- fread("C:/Users/Yuanzhi Li/Desktop/Big Data/test.csv", na = "NA")[,-1]

####################################################################################################
####################################################################################################

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
test$PoolQC[is.na(test$PoolQC)] <- 'NoPool'
test$Fence[is.na(test$Fence)] <- 'NoFence'
test$MiscFeature[is.na(test$MiscFeature)] <- 'NoMisc'


cleanTest <-test

######Use the following code to detect NA values remaining in the dataset. 
countNA <- function(column)
{
  totalNA = sum(is.na(column))
  
  return(totalNA)
}


NAcountVect = apply(test,MARGIN = 2, countNA)
NAcountVect = apply(cleanTest,MARGIN = 2, countNA)

sort(NAcountVect, decreasing = TRUE)

##Some junk to clean up.
NAcountVect[NAcountVect > 0]




######Since the data for Masonry Veneer Area is heavily skewed, we will impute the 8 NA with the median. 
######This also corresponds with our decision to treat these NA as indicating "no masonry veneer."
summary(cleanTest$MasVnrArea)
boxplot(cleanTest$MasVnrArea)
cleanTest$MasVnrArea[is.na(cleanTest$MasVnrArea)] = 0



table(cleantest$MasVnrType)
###It seems most reasonable to treat the NA values for Masonry veneer type as "none."
VnrTypeNA = is.na(cleanTest$MasVnrType)
cleanTest$MasVnrType[VnrTypeNA] = "None"

###MSZoning
table(cleanTest$MSZoning)
which(is.na(cleanTest$MSZoning))
MSzoneNA = which(is.na(cleanTest$MSZoning))
cleanTest$MSZoning[MSzoneNA] = "RL"
sum(table(cleanTest$MSZoning))

####Utilities
table(cleanTest$Utilities)###Every house with a value has all utilities. Assume they ALL do. 
which(is.na(cleanTest$Utilities))
UtilNA = which(is.na(cleanTest$Utilities))
cleanTest$Utilities[UtilNA] = "AllPub"
sum(table(cleanTest$Utilities))

####Exterior1st
table(cleanTest$Exterior1st)###
which(is.na(cleanTest$Exterior1st))
Exterior1stNA = which(is.na(cleanTest$Exterior1st))
cleanTest$Exterior1st[Exterior1stNA] = cleanTest$Exterior1st[sample(c(1:1459),1)]##Randomly shove something in there.
sum(table(cleanTest$Exterior1st))

####Exterior2nd
table(cleanTest$Exterior2nd)###
which(is.na(cleanTest$Exterior2nd))
Exterior2ndNA = which(is.na(cleanTest$Exterior2nd))
cleanTest$Exterior2nd[Exterior2ndNA] = cleanTest$Exterior2nd[sample(c(1:1459),1)]##Randomly shove something in there.
sum(table(cleanTest$Exterior2nd))

####KitchenQual
table(cleanTest$KitchenQual)###
which(is.na(cleanTest$KitchenQual))
KitchenQualNA = which(is.na(cleanTest$KitchenQual))
cleanTest$KitchenQual[KitchenQualNA] = cleanTest$KitchenQual[sample(c(1:1459),1)]##Randomly shove something in there.
sum(table(cleanTest$KitchenQual))

####SaleType
table(cleanTest$SaleType)###
which(is.na(cleanTest$SaleType))
SaleTypeNA = which(is.na(cleanTest$SaleType))
cleanTest$SaleType[SaleTypeNA] = cleanTest$SaleType[sample(c(1:1459),1)]##Randomly shove something in there.
sum(table(cleanTest$SaleType))

####Functional
table(cleanTest$Functional)###
which(is.na(cleanTest$Functional))
FunctionalNA = which(is.na(cleanTest$Functional))
cleanTest$Functional[FunctionalNA] = cleanTest$Functional[sample(c(1:1459),2)]##Randomly shove something in there.
sum(table(cleanTest$Functional))


####BAsement Stuff. 
which(is.na(cleanTest$BsmtHalfBath))###661 for all the missing basement values.  729 for the bathrooms as well. 

cleanTest$BsmtFinSF1[661] =0
cleanTest$BsmtFinSF2[661] =0
cleanTest$BsmtUnfSF[661] =0
cleanTest$TotalBsmtSF[661] =0
cleanTest$BsmtFullBath[661] =0
cleanTest$BsmtFullBath[729] =0
cleanTest$BsmtHalfBath[661] =0
cleanTest$BsmtHalfBath[729] =0


#########Handing the Garage Year Built NA
######### There are some houses without garages. 
######### The only sort of reasonable way to impute the NAs for Garage Year was just to put in the year the house was built.
###### We also considered just leaving these NA as is.

GarageYrNA = which(is.na(cleanTest$GarageYrBlt))
cleanTest$GarageYrBlt[GarageYrNA] = cleanTest$YearBuilt[GarageYrNA]

####Other Garage Stuff. 
which(is.na(cleanTest$GarageCars))###1117 

cleanTest$GarageFinish[1117]###No Garage.
cleanTest$GarageCars[1117] =0
cleanTest$GarageArea[1117] =0

sum(sapply(cleanTest, function(x){ sum(is.na(x))}))

####################################################################################################



modelMat = model.matrix(log(SalePrice)~as.factor(MSSubClass)+as.factor(MSZoning)+LotFrontage+LotArea+as.factor(Street)+as.factor(Alley)+
                   as.factor(LotShape)+as.factor(LandContour)+as.factor(Utilities)+as.factor(LotConfig)+
                   as.factor(LandSlope)+as.factor(Neighborhood)+as.factor(Condition1)+as.factor(Condition2)+
                   as.factor(BldgType)+as.factor(HouseStyle)+as.factor(OverallQual)+as.factor(OverallCond)+
                   YearBuilt+YearRemodAdd+as.factor(RoofStyle)+as.factor(RoofMatl)+as.factor(Exterior1st)+
                   as.factor(Exterior2nd)+as.factor(MasVnrType)+MasVnrArea+as.factor(ExterQual)+as.factor(ExterCond)+
                   as.factor(Foundation)+as.factor(BsmtQual)+as.factor(BsmtCond)+as.factor(BsmtExposure)+as.factor(BsmtFinType1)+BsmtFinSF1+
                   as.factor(BsmtFinType2)+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+as.factor(Heating)+as.factor(HeatingQC)+
                   as.factor(CentralAir)+as.factor(Electrical)+`1stFlrSF`+`2ndFlrSF`+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+
                   BedroomAbvGr+KitchenAbvGr+as.factor(KitchenQual)+TotRmsAbvGrd+as.factor(SaleType)+Fireplaces+as.factor(FireplaceQu)+
                   as.factor(GarageType)+GarageYrBlt+as.factor(GarageFinish)+GarageCars+GarageArea+as.factor(GarageQual)+as.factor(GarageCond)+
                   as.factor(PavedDrive)+	WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea+
                   as.factor(PoolQC)+as.factor(Fence)+as.factor(MiscFeature)+
                   MiscVal+MoSold+YrSold+as.factor(SaleType)+as.factor(SaleCondition))


set.seed(7919)

out2 = tenfold(x=X, y=cleanTrain$SalePrice, nfold=10, method="rpart")

out3 = tenfold(x=X, y=log(cleanTrain$SalePrice), nfold=10, method="randomForest")

out4 = tenfold(x=X, y=(cleanTrain$SalePrice), nfold=10, method="gbm", n.trees=1000)

out5 = tenfold(x=X, y=cleanTrain$SalePrice, nfold=10, method="nnet", size=100, linout=TRUE, maxit=500)



######Raw untuned fit. Not very accurate. (Kaggle error of 0.22)
housing.gbm=gbm(log(SalePrice)~as.factor(MSSubClass)+as.factor(MSZoning)+LotFrontage+LotArea+as.factor(Street)+as.factor(Alley)+
                  as.factor(LotShape)+as.factor(LandContour)+as.factor(Utilities)+as.factor(LotConfig)+
                  as.factor(LandSlope)+as.factor(Neighborhood)+as.factor(Condition1)+as.factor(Condition2)+
                  as.factor(BldgType)+as.factor(HouseStyle)+as.factor(OverallQual)+as.factor(OverallCond)+
                  YearBuilt+YearRemodAdd+as.factor(RoofStyle)+as.factor(RoofMatl)+as.factor(Exterior1st)+
                  as.factor(Exterior2nd)+as.factor(MasVnrType)+MasVnrArea+as.factor(ExterQual)+as.factor(ExterCond)+
                  as.factor(Foundation)+as.factor(BsmtQual)+as.factor(BsmtCond)+as.factor(BsmtExposure)+as.factor(BsmtFinType1)+BsmtFinSF1+
                  as.factor(BsmtFinType2)+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+as.factor(Heating)+as.factor(HeatingQC)+
                  as.factor(CentralAir)+as.factor(Electrical)+`1stFlrSF`+`2ndFlrSF`+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+
                  BedroomAbvGr+KitchenAbvGr+as.factor(KitchenQual)+TotRmsAbvGrd+as.factor(SaleType)+Fireplaces+as.factor(FireplaceQu)+
                  as.factor(GarageType)+GarageYrBlt+as.factor(GarageFinish)+GarageCars+GarageArea+as.factor(GarageQual)+as.factor(GarageCond)+
                  as.factor(PavedDrive)+	WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea+
                  as.factor(PoolQC)+as.factor(Fence)+as.factor(MiscFeature)+
                  MiscVal+MoSold+YrSold+as.factor(SaleType)+as.factor(SaleCondition),distribution="gaussian",n.trees=5000,data=cleanTrain)

#table(lichenLO$LobaOreg,round(predict(Loreg.gbm,type="response",n.trees=5000)+0.0000001))
#class.sum(lichenLO$LobaOreg,predict(Loreg.gbm,type="response",n.trees=5000))




fitControl = trainControl(method = "cv", number = 10 )

#
# tune gbm
#
gbmGrid = expand.grid(.interaction.depth = c(1,5,9,13), .n.trees = c(250,500,750,1000), .shrinkage = c(0.01, 0.05, 0.1, 0.2 ), .n.minobsinnode = 10)

gbmFit = train( log(SalePrice)~as.factor(MSSubClass)+as.factor(MSZoning)+LotFrontage+LotArea+as.factor(Street)+as.factor(Alley)+
                  as.factor(LotShape)+as.factor(LandContour)+as.factor(Utilities)+as.factor(LotConfig)+
                  as.factor(LandSlope)+as.factor(Neighborhood)+as.factor(Condition1)+as.factor(Condition2)+
                  as.factor(BldgType)+as.factor(HouseStyle)+as.factor(OverallQual)+as.factor(OverallCond)+
                  YearBuilt+YearRemodAdd+as.factor(RoofStyle)+as.factor(RoofMatl)+as.factor(Exterior1st)+
                  as.factor(Exterior2nd)+as.factor(MasVnrType)+MasVnrArea+as.factor(ExterQual)+as.factor(ExterCond)+
                  as.factor(Foundation)+as.factor(BsmtQual)+as.factor(BsmtCond)+as.factor(BsmtExposure)+as.factor(BsmtFinType1)+BsmtFinSF1+
                  as.factor(BsmtFinType2)+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+as.factor(Heating)+as.factor(HeatingQC)+
                  as.factor(CentralAir)+as.factor(Electrical)+`1stFlrSF`+`2ndFlrSF`+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+
                  BedroomAbvGr+KitchenAbvGr+as.factor(KitchenQual)+TotRmsAbvGrd+as.factor(SaleType)+Fireplaces+as.factor(FireplaceQu)+
                  as.factor(GarageType)+GarageYrBlt+as.factor(GarageFinish)+GarageCars+GarageArea+as.factor(GarageQual)+as.factor(GarageCond)+
                  as.factor(PavedDrive)+	WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea+
                  as.factor(PoolQC)+as.factor(Fence)+as.factor(MiscFeature)+
                  MiscVal+MoSold+YrSold+as.factor(SaleType)+as.factor(SaleCondition) , method="gbm", tuneGrid = gbmGrid, trControl = fitControl, data=cleanTrain)
gbmFit
# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were n.trees = 750, interaction.depth = 13, shrinkage = 0.05
# and n.minobsinnode = 10. 

####Further hand tuning found n.trees = 500, interaction.depth = 13, shrinkage = 0.015
# and n.minobsinnode = 15 to be an overall improvement over the caret tuning. RMSE = 0.1301. 

housing.gbm.trained=gbm(log(SalePrice)~as.factor(MSSubClass)+as.factor(MSZoning)+LotFrontage+LotArea+as.factor(Street)+as.factor(Alley)+
                          as.factor(LotShape)+as.factor(LandContour)+as.factor(Utilities)+as.factor(LotConfig)+
                          as.factor(LandSlope)+as.factor(Neighborhood)+as.factor(Condition1)+as.factor(Condition2)+
                          as.factor(BldgType)+as.factor(HouseStyle)+as.factor(OverallQual)+as.factor(OverallCond)+
                          YearBuilt+YearRemodAdd+as.factor(RoofStyle)+as.factor(RoofMatl)+as.factor(Exterior1st)+
                          as.factor(Exterior2nd)+as.factor(MasVnrType)+MasVnrArea+as.factor(ExterQual)+as.factor(ExterCond)+
                          as.factor(Foundation)+as.factor(BsmtQual)+as.factor(BsmtCond)+as.factor(BsmtExposure)+as.factor(BsmtFinType1)+BsmtFinSF1+
                          as.factor(BsmtFinType2)+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+as.factor(Heating)+as.factor(HeatingQC)+
                          as.factor(CentralAir)+as.factor(Electrical)+`1stFlrSF`+`2ndFlrSF`+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+
                          BedroomAbvGr+KitchenAbvGr+as.factor(KitchenQual)+TotRmsAbvGrd+as.factor(SaleType)+Fireplaces+as.factor(FireplaceQu)+
                          as.factor(GarageType)+GarageYrBlt+as.factor(GarageFinish)+GarageCars+GarageArea+as.factor(GarageQual)+as.factor(GarageCond)+
                          as.factor(PavedDrive)+	WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea+
                          as.factor(PoolQC)+as.factor(Fence)+as.factor(MiscFeature)+
                          MiscVal+MoSold+YrSold+as.factor(SaleType)+as.factor(SaleCondition),
                          n.minobsinnode = 16,distribution="gaussian",interaction.depth = 11,shrinkage = 0.035,n.trees=480,data=cleanTrain)

####Doing some CV fitting where I bootstrap in the response by using the current best prediction on the test set.

cleanTestWithEstPrice = cbind(logPricesGBM, cleanTest)
cv.gbm.xvalpr=rep(0,nrow(cleanTestWithEstPrice))
xvs=rep(1:10,length=nrow(cleanTestWithEstPrice))
xvs=sample(xvs)
for(i in 1:10){
  train=cleanTestWithEstPrice[xvs!=i,]
  test=cleanTestWithEstPrice[xvs==i,]
  #print(c("Price " , length(train$logPricesGBM)))
  #print(length(as.factor(train$MSZoning)))
  glub=gbm(logPricesGBM~as.factor(MSZoning)+LotFrontage+LotArea+as.factor(Street)+as.factor(Alley)+
             as.factor(LotShape)+as.factor(LandContour)+as.factor(Utilities)+as.factor(LotConfig)+
             as.factor(LandSlope)+as.factor(Neighborhood)+as.factor(Condition1)+as.factor(Condition2)+
             as.factor(BldgType)+as.factor(HouseStyle)+as.factor(OverallQual)+as.factor(OverallCond)+
             YearBuilt+YearRemodAdd+as.factor(RoofStyle)+as.factor(RoofMatl)+as.factor(Exterior1st)+
             as.factor(Exterior2nd)+as.factor(MasVnrType)+MasVnrArea+as.factor(ExterQual)+as.factor(ExterCond)+
             as.factor(Foundation)+as.factor(BsmtQual)+as.factor(BsmtCond)+as.factor(BsmtExposure)+as.factor(BsmtFinType1)+BsmtFinSF1+
             as.factor(BsmtFinType2)+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+as.factor(Heating)+as.factor(HeatingQC)+
             as.factor(CentralAir)+as.factor(Electrical)+`1stFlrSF`+`2ndFlrSF`+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+
             BedroomAbvGr+KitchenAbvGr+as.factor(KitchenQual)+TotRmsAbvGrd+as.factor(SaleType)+Fireplaces+as.factor(FireplaceQu)+
             as.factor(GarageType)+GarageYrBlt+as.factor(GarageFinish)+GarageCars+GarageArea+as.factor(GarageQual)+as.factor(GarageCond)+
             as.factor(PavedDrive)+	WoodDeckSF+OpenPorchSF+EnclosedPorch+`3SsnPorch`+ScreenPorch+PoolArea+
             as.factor(PoolQC)+as.factor(Fence)+as.factor(MiscFeature)+
             MiscVal+MoSold+YrSold+as.factor(SaleType)+as.factor(SaleCondition) ,distribution="gaussian",
           interaction.depth=13,n.trees=750, shrinkage=0.05,data=train)
  cv.gbm.xvalpr[xvs==i]=predict(glub,newdata=test,type="response",n.trees=750)
}

#cv.gbm.xvalpr

logPricesGBM = predict(housing.gbm.trained,type="response",n.trees=480, newdata=cleanTest)


FinalPrice = exp(logPricesGBM)
FinalPrice = exp(cv.gbm.xvalpr)

write.csv(FinalPrice, file = "PredictedPrices1.csv")
###Best is noted in Kaggle (the settings are noted) 

