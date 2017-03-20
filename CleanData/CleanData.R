data = read.csv('train.csv', stringsAsFactors=FALSE)
summary(data)
# Mean of lot frontage = 70.05

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
data$PoolQC[is.na(data$PoolQC)] <- 'NoPool'
data$Fence[is.na(data$Fence)] <- 'NoFence'
data$MiscFeature[is.na(data$MiscFeature)] <- 'NoMisc'

# Check for any NA's in the data frame
sapply(data, function(x) sum(is.na(x)))
