require(knitr)              ## dynamic report generation in R
require(DT)                 ## display data in html tables
require(ggplot2)            ## plotting 
require(gridExtra)          ## arrange visualizations using grid 
require(dplyr)              ## easy data wrangling on small data frames
require(data.table)         ## fast data wrangling and analysis
require(funModeling)        ## table with counts on missing values. Do not load before psych or caret. It will mask some stuff.
require(psych)              ## descriptive statistics, skewness and kurtosis
require(caret)              ## (near) zero variance, train and predict
require(caretEnsemble)      ## ensemble modelling
require(xgboost)
require(glmnet)


train.raw <- read.csv("train.csv",stringsAsFactors = FALSE,na.strings=c("NA","N/A","null"))

test.raw <- read.csv("test.csv", stringsAsFactors = FALSE,na.strings=c("NA","N/A","null"))

ncol(train.raw) #82
ncol(test.raw) #81
nrow(train.raw[!complete.cases(train.raw),]) #1460 (All)


train.raw$DataPartition = "Train"
test.raw$DataPartition = "Test"
test.raw$SalePrice = as.integer(NA)


summary(train.raw)
ggplot(train.raw,aes(x=SalePrice,y=GarageArea)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(train.raw,aes(x=SalePrice,y=MSZoning)) + geom_point() +  geom_smooth(method='lm',formula=y~x) 

ggplot(train.raw, aes(x = GLA, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of SalePrice") +
  ylab("Count of houses") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))


#Factor Research 
colnames(train.raw)
unique(train.raw$MSSubClass)
#60  20  70  50 190  45  90 120  30  85  80 160  75 180  40

unique(train.raw$MSZoning)
#"RL"      "RM"      "C (all)" "FV"      "RH"  
#RL - Residential Low Density
#RM - Residential Medium Density
#Residential High Density

unique(train.raw$RoofStyle)

#predict $/sf
BsmtFinSF1 + BsmtFinSF2 + GrLivArea
  X1stFlrSF + X2ndFlrSF
  
  
  
  
  
  
  
  
  
  
  
  
  
#############################################################################################
#############################################################################################
#############################################################################################
library(tree)
library(ISLR)  
library(randomForest)
  
head(house_preProc.RdATA)
head(subTrain)
#Fitting an initial random forest to the training subset.
set.seed(0)
rf.subTrain = randomForest(SalePrice ~ ., data = subTrain, importance = TRUE)
#subset=train
rf.subTrain
importance(rf.subTrain)
varImpPlot(rf.subTrain) 


#Creating a saturated model (a model with all variables included).
mlr.subTrain = lm(SalePrice ~ ., data = subTrain)

summary(mlr.subTrain) 

# library(car)
# scatterplot(SalePrice ~ YearBuilt, data=subTrain,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
# scatterplot(SalePrice ~ LotFrontage, data=subTrain,  xlab="LotFrontage", ylab="Sale Price", grid=FALSE)


#MSSubClass30 is signifcant
#MSSubClass160 is significant
# MSZoningFV            5.628e-01  7.128e-02   7.895 8.28e-15 ***
#   MSZoningRH            4.831e-01  6.920e-02   6.982 5.59e-12 ***
#   MSZoningRL            5.043e-01  6.183e-02   8.157 1.12e-15 ***
#   MSZoningRM            4.456e-01  5.816e-02   7.661 4.68e-14 ***
# LotFrontage          -7.085e-04  2.775e-04  -2.553 0.010844 *  
#   LotArea               2.403e-06  5.891e-07   4.079 4.91e-05 ***
# LotShapeIR3          -1.767e-01  5.540e-02  -3.190 0.001472 ** 
#   LandContourBnk       -8.868e-02  2.066e-02  -4.292 1.96e-05 ***

# LotConfigCulDSac      3.920e-02  1.741e-02   2.252 0.024565 *  
#   LotConfigFR2         -4.453e-02  2.144e-02  -2.076 0.038130 * 

# LandSlopeMod          3.997e-02  2.343e-02   1.706 0.088305 .  
# LandSlopeSev         -1.172e-01  5.939e-02  -1.974 0.048671 * 




plot(mlr.subTrain) #Assessing the assumptions of the model.

library(car) #Companion to applied regression.
influencePlot(mlr.subTrain)

vif(mlr.subTrain) #Assessing the variance inflation factors for the variables
#in our model.

#Added variable plots for assessing the contribution of each additional variable.
avPlots(mlr.subTrain) #Distinct patterns are indications of good contributions
#to the model; absent patterns usually are pointers to
#variables that could be dro





#######################################################################################
#######################################################################################
#######################################################################################
library(glmnet)
#Elastic Net
#Values of lambda over which to check.
grid = 10^seq(5, -2, length = 100)
x = model.matrix(SalePrice ~ ., subTrain)
y = subTrain$SalePrice
en.models = glmnet(x, y, alpha = .5, lambda = grid)
plot(en.models, xvar = "lambda", label = TRUE, main = "EN Regression")

cv.lasso.out = cv.glmnet(x, y,lambda = grid, alpha = .5, nfolds = 10)
plot(cv.lasso.out, main = "EN Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
summary(cv.lasso.out)

coef(cv.lasso.out)


#################
test.subTrain = subTrain %>% filter(.,subTrain$GrLivArea < 4000)
#4 not 5 removed
test.subTrain$Totalfinsf = test.subTrain$GrLivArea + test.subTrain$BsmtFinSF1 + test.subTrain$BsmtFinSF2
test.subTrain$Totalfullbath = test.subTrain$BsmtFullBath + test.subTrain$FullBath
test.subTrain$Totalhalfbath = test.subTrain$BsmtHalfBath + test.subTrain$HalfBath
#87.38

drops = c("Alley","LandSlope","LandContour","BsmtCond","Exterior1st","Exterior2nd",
          "BldgType","HouseStyle","LotFrontage","Exterior2nd",
          "Electrical","GarageCond","MiscFeature","Fence","YrSold",
          "PavedDrive","Functional",
          "BsmtFullBath","BsmtHalfBath","FullBath","HalfBath",
          "GrLivArea","X1stFlrSF","X2ndFlrSF")

test.subTrain=test.subTrain[ , !(names(test.subTrain) %in% drops)]
dim(test.subTrain)
#combine total sf
#combine bathrooms

set.seed(0)
rf.test.subTrain = randomForest(SalePrice ~ ., data = test.subTrain, importance = TRUE)
#subset=train
rf.subTrain
importance(rf.test.subTrain)
varImpPlot(rf.test.subTrain) 





