#Machine Learning Project#
getwd()
rm(list = ls())
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(tidyr)
library(DT)
library(plotly)
library(ggrepel)
library(missForest)
library(multcomp)
library(mice)
train <- read.csv("train.csv",stringsAsFactors = FALSE)
#test <- read_csv("test.csv")


###########New Fields#################
#total # of Half bathrooms
train = train %>%  mutate(TotalHalfBath = train$HalfBath + train$BsmtHalfBath)

#total # of Full bathrooms
train = train %>%  mutate(TotalFullBath = train$FullBath + train$BsmtFullBath)

#total SF (Finished)
train = train %>%  mutate(TotalFinSF = train$GrLivArea + train$BsmtFinSF1 + train$BsmtFinSF2 + train$LowQualFinSF)
train = train %>% mutate(., TotalFinSF = log(TotalFinSF))

#Avg Size Rooms Abv Ground
train = train %>%  mutate(AbvGrndRoomSize = (train$GrLivArea/ (train$TotRmsAbvGrd)))
                          


#split it into 1 story, 1.5 stories, 2 stories, split, projects
train = train %>% mutate(MSSubClass = ifelse(MSSubClass %in% c(20), '1 story (new styles)', 
                                             ifelse(MSSubClass %in% c(30,40), '1 story older',
                                                    ifelse(MSSubClass %in% c(45,50), '1.5 stories',
                                                           ifelse(MSSubClass %in% c(60, 75), '2 story newer',
                                                                  ifelse(MSSubClass %in% c(70), '2 story older',
                                                                         ifelse(MSSubClass %in% c(80,85), 'split',
                                                                                ifelse(MSSubClass %in% c(90), 'duplex', 'projects'))))))))
#split zoning into medium-high, low, floating, and miscelanneoius
train = train %>% mutate(MSZoning = ifelse(MSZoning %in% c('RH', 'RM'), 'medium-high density',
                                           ifelse(MSZoning == 'RL', 'low density',
                                                  ifelse(MSZoning == 'FV', 'floating', 'misc'))))
#lot frontage
# LF_impute <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
# 
# counter = 1
# for (i in 1:1460) {
#   if (is.na(train$LotFrontage[i]) == TRUE) {
#     train$LotFrontage[i] = as.numeric(rowMeans(LF_impute$imp$LotFrontage))[counter]
#     counter = counter + 1
#   }
# }
#lot area - logged
train = train %>% mutate(LotArea = log(LotArea))
#summary(train$LotArea)
#street - dropped
train = train %>% dplyr::select(-Street)
#street - dropped
train = train %>% dplyr::select(-Alley)
#lot shape- as is
#land contour- as is
#utilities
train = train %>% dplyr::select(-Utilities)
#lot config - as is
#land slope - as is
#neighborhood - as is
#condition 1 - near positive fature, near railrod, near street, normal
train = train %>% mutate(Condition1 = ifelse(Condition1 %in% c('Artery', 'Feedr'), 'near street',
                                             ifelse(Condition1 %in% c('RRAe', 'RRAn', 'RRNe', 'RRNn'),'near railroad',
                                                    ifelse(Condition1 %in% c('PosA', 'PosN'), 'near positive feature', 'Normal'
                                                    ))))
#condition 2 - drop
train = train %>% dplyr::select(-Condition2)
#building type- as is
#housestyle
train = train %>% mutate(HouseStyle = ifelse(HouseStyle %in% c('1.5Fin', '1.5Unf'), '1.5 stories',
                                             ifelse(HouseStyle %in% c('1Story'), '1 story',
                                                    ifelse(HouseStyle %in% c('2.5Fin', '2.5Unf', '2Story'), '2 stories', 'foyer'))))
#overalqual as it
#overalcond as is
#year remolded
train = train %>% mutate(YearRemodAdd = ifelse(YearRemodAdd == YearBuilt, 1, 0))
#year build- diff logged
train = train %>% mutate(YearBuilt = log((YrSold - YearBuilt)+1))
######MHUNG CODE######
#leaving RoofStyle as is
#RoofMatl
train <- train %>%
  mutate(RoofMatl = 
           ifelse(RoofMatl %in% c('ClyTile', 'CompShg'), 'Shingle',
                  ifelse(RoofMatl %in% c('Membrane'), 'Membrane',
                         ifelse(RoofMatl %in% c('Metal', 'Roll'), 'Metal',
                                ifelse(RoofMatl %in% c('WdShake', 'WdShngl'), 'Wood', 'Gravel & Tar')))))
#Exterior1st
train <- train %>%
  mutate(Exterior1st = 
           ifelse(Exterior1st %in% c('AsbShng', 'AsphShn'), 'Shingle',
                  ifelse(Exterior1st %in% c('BrkComm', 'BrkFace'), 'Brick',
                         ifelse(Exterior1st %in% c('CBlock', 'CemntBd', 'PreCast'), 'Concrete',
                                ifelse(Exterior1st %in% c('HdBoard', 'Plywood'), 'Ply',
                                       ifelse(Exterior1st %in% c('ImStucc', 'VinylSd'), 'Synthetic',
                                              ifelse(Exterior1st %in% c('Other'), 'Other',
                                                     ifelse(Exterior1st %in% c('Stone'), 'Stone', 'Stucco'))))))))
#Exterior2nd
train <- train %>%
  mutate(Exterior2nd = 
           ifelse(Exterior2nd %in% c('AsbShng', 'AsphShn'), 'Shingle',
                  ifelse(Exterior2nd %in% c('BrkComm', 'BrkFace'), 'Brick',
                         ifelse(Exterior2nd %in% c('CBlock', 'CemntBd', 'PreCast'), 'Concrete',
                                ifelse(Exterior2nd %in% c('HdBoard', 'Plywood'), 'Ply',
                                       ifelse(Exterior2nd %in% c('ImStucc', 'VinylSd'), 'Synthetic',
                                              ifelse(Exterior2nd %in% c('Other'), 'Other',
                                                     ifelse(Exterior2nd %in% c('Stone'), 'Stone', 'Stucco'))))))))
#MasVnrType
#table(train$MasVnrType)
#table(train$MasVnrType)
train <- train %>%
  mutate(MasVnrType = 
           ifelse(MasVnrType %in% c('BrkCmn', 'BrkFace'), 'Brick',
                  ifelse(MasVnrType %in% c('CBlock'), 'CBlock',
                         ifelse(MasVnrType %in% c('None'), 'None', 'Stone'))))

train$MasVnrType[is.na(train$MasVnrType)] <- "None"

#sum(is.na(train$MasVnrType))
#MasVnrArea as is
train = train %>% mutate(MasVnrArea = ifelse(is.na(MasVnrArea)== TRUE, 0, MasVnrArea ))
train = train %>% mutate(MasVnrArea = log(MasVnrArea + 1))
#ExterQual
train <- train %>%
  mutate(ExterQual = 
           ifelse(ExterQual == 'Ex', '4',
                  ifelse(ExterQual == 'Gd', '3',
                         ifelse(ExterQual == 'TA', '2',
                                ifelse(ExterQual == 'Fa', '1', '0')))))
#ExterCond
train <- train %>%
  mutate(ExterCond = 
           ifelse(ExterCond == 'Ex', '4',
                  ifelse(ExterCond == 'Gd', '3',
                         ifelse(ExterCond == 'TA', '2',
                                ifelse(ExterCond == 'Fa', '1', '0')))))
#Foundation as is
#BsmtQual
train <- train %>%
  mutate(BsmtQual = 
           ifelse(BsmtQual == 'Ex', '5',
                  ifelse(BsmtQual == 'Gd', '4',
                         ifelse(BsmtQual == 'TA', '3',
                                ifelse(BsmtQual == 'Fa', '2', 
                                       ifelse(BsmtQual == 'Po', '1', '0'))))))
train$BsmtQual[is.na(train$BsmtQual)] <- 0
train$BsmtQual <- as.numeric(train$BsmtQual)
#BsmtCond
train <- train %>%
  mutate(BsmtCond = 
           ifelse(BsmtCond == 'Ex', '5',
                  ifelse(BsmtCond == 'Gd', '4',
                         ifelse(BsmtCond == 'TA', '3',
                                ifelse(BsmtCond == 'Fa', '2', 
                                       ifelse(BsmtCond == 'Po', '1', '0'))))))
train$BsmtCond[is.na(train$BsmtCond)] <- 0
train$BsmtCond <- as.numeric(train$BsmtCond)
#BsmtExposure
train <- train %>%
  mutate(BsmtExposure = 
           ifelse(BsmtExposure == 'Gd', '4',
                  ifelse(BsmtExposure == 'Av', '3',
                         ifelse(BsmtExposure == 'Mn', '2',
                                ifelse(BsmtExposure == 'No', '1', '0')))))
train$BsmtExposure[is.na(train$BsmtExposure)] <- 0
train$BsmtExposure <- as.numeric(train$BsmtExposure)
#BsmtFinType1
train <- train %>%
  mutate(BsmtFinType1 = 
           ifelse(BsmtFinType1 == 'GLQ', '6',
                  ifelse(BsmtFinType1 == 'ALQ', '5',
                         ifelse(BsmtFinType1 == 'BLQ', '4',
                                ifelse(BsmtFinType1 == 'Rec', '3',
                                       ifelse(BsmtFinType1 == 'LwQ', '2',
                                              ifelse(BsmtFinType1 == 'Unf', '1', '0')))))))

train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 0

#BsmtFinSF1
train = train %>% mutate(BsmtFinSF1 = log(BsmtFinSF1 + 1))
#BsmtFinType2
train <- train %>%
  mutate(BsmtFinType2 = 
           ifelse(BsmtFinType2 == 'GLQ', '6',
                  ifelse(BsmtFinType2 == 'ALQ', '5',
                         ifelse(BsmtFinType2 == 'BLQ', '4',
                                ifelse(BsmtFinType2 == 'Rec', '3',
                                       ifelse(BsmtFinType2 == 'LwQ', '2',
                                              ifelse(BsmtFinType2 == 'Unf', '1', '0')))))))
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 0
train$BsmtFinType2 <- as.numeric(train$BsmtFinType2)
#BsmtFinSF2
train = train %>% mutate(BsmtFinSF2 = log(BsmtFinSF2 + 1))
#BsmtUnfSF
train = train %>% mutate(BsmtUnfSF = log(BsmtUnfSF + 1))
#TotalBsmtSF
train = train %>% mutate(TotalBsmtSF = log(TotalBsmtSF + 1))
#Heating (think we should leave as is after more research)
#HeatingQC
train <- train %>%
  mutate(HeatingQC = 
           ifelse(HeatingQC == 'Ex', '4',
                  ifelse(HeatingQC == 'Gd', '3',
                         ifelse(HeatingQC == 'TA', '2',
                                ifelse(HeatingQC == 'Fa', '1', '0')))))
train$HeatingQC[is.na(train$HeatingQC)] <- 0
train$HeatingQC <- as.numeric(train$HeatingQC)
#CentralAir as-is

# read in whole training set
# Electrical =>
train <- train %>%
  mutate(Electrical = 
           ifelse(is.na(Electrical)==TRUE,'None',Electrical))

# X1stFlrSF => log
train = train %>% mutate(., X1stFlrSF = log(X1stFlrSF + 1))
# X2ndFlrSF => log(+1)
train = train %>% mutate(., X2ndFlrSF = log(X2ndFlrSF + 1))
# LowQualFinSF As-is
train = train %>% mutate(., GrLivArea = log(LowQualFinSF+1))
# GrLivArea => log
train = train %>% mutate(., GrLivArea = log(GrLivArea))
# BsmtFullBath => As-is
# BsmtHalfBath => As-is
# FullBath => As-is
# HalfBath => As-is
# BedroomAbvGR => As-is
# KitchenAbvGr => As-is
# KitchenQual => ordinal ; Po = 0, Fa = 1, TA = 2, Gd = 3, Ex = 4
train = train %>% mutate(KitchenQual = ifelse(is.na(KitchenQual)==TRUE,0,
                                              ifelse(KitchenQual =='Po',1,
                                                     ifelse(KitchenQual == 'Fa', 2,
                                                            ifelse(KitchenQual == 'TA', 3,
                                                                   ifelse(KitchenQual == 'Gd', 4,5))))))
# TotalRmsAbvGrd => As-is
# Functional Columns => Reducing the total number of groups; Min = Min1 | Min2, Maj = Maj1 | Maj2
# Remaining groups are: Typ, Min, Mod, Maj, Sev, Sal
train = train %>% mutate(KitchenQual = ifelse(is.na(Functional)==TRUE,0,
                                              ifelse(Functional =='Po',1,
                                                     ifelse(Functional == 'Fa', 2,
                                                            ifelse(Functional == 'TA', 3,
                                                                   ifelse(Functional == 'Gd', 4,5))))))
# Fireplaces => As-is
# FireplaceQu => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
train = train %>% mutate(FireplaceQu = ifelse(is.na(FireplaceQu)==TRUE,0,
                                              ifelse(FireplaceQu =='Po',1,
                                                     ifelse(FireplaceQu == 'Fa', 2,
                                                            ifelse(FireplaceQu == 'TA', 3,
                                                                   ifelse(FireplaceQu == 'Gd', 4,5))))))
# GarageType => Reducing the number of groups; 2Types, Attached = Attchd | Basment | BuiltIn, CarPort, Detchd, No Garage = NA
train = train %>% mutate(GarageType = ifelse(is.na(GarageType)==TRUE,0,
                                             ifelse(GarageType =='Po',1,
                                                    ifelse(GarageType == 'Fa', 2,
                                                           ifelse(GarageType == 'TA', 3,
                                                                  ifelse(GarageType == 'Gd', 4,5))))))
# Adding new field: New_Field_1 = log(YrSold - GarageYrBuilt) | I don't know why we want this 
#### There are NA values here and I'm not sure the proper way of imputing them
train[is.na(train$GarageYrBlt),]<-0
train = train %>% mutate(., YrSold_Garage_Difference = log((YrSold - GarageYrBlt)+1))
# GarageFinish => update NA to No Garage
train = train %>% mutate(GarageFinish = ifelse(is.na(GarageFinish)==TRUE,0,
                                               ifelse(GarageFinish =='Po',1,
                                                      ifelse(GarageFinish == 'Fa', 2,
                                                             ifelse(GarageFinish == 'TA', 3,
                                                                    ifelse(GarageFinish == 'Gd', 4,5))))))
# GarageCars => update NA to No Garage
train = train %>% mutate(GarageCars = ifelse(is.na(GarageCars)==TRUE,0,GarageCars))
# GarageArea => log + 1
train = train %>% mutate(., GarageArea = log(GarageArea + 1))
# GarageQual => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
train = train %>% mutate(GarageQual = ifelse(is.na(GarageQual)==TRUE,0,
                                             ifelse(GarageQual =='Po',1,
                                                    ifelse(GarageQual == 'Fa', 2,
                                                           ifelse(GarageQual == 'TA', 3,
                                                                  ifelse(GarageQual == 'Gd', 4,5))))))
train = train %>% mutate(GarageCond = ifelse(is.na(GarageCond)==TRUE,0,
                                             ifelse(GarageCond =='Po',1,
                                                    ifelse(GarageCond == 'Fa', 2,
                                                           ifelse(GarageCond == 'TA', 3,
                                                                  ifelse(GarageCond == 'Gd', 4,5))))))
######RTIWARI CODE######
###########################Paved Drive Way (Ordinal) - As-Is##############################
#table(train$PavedDrive)
#ggplot(train, aes(x = train$PavedDrive,y = train$SalePrice)) + geom_boxplot() + coord_flip()
#paved_anova = train %>% 
#  dplyr::group_by(.,train$PavedDrive) %>% 
#  summarise(
#    count = n(),
#    mean = mean(SalePrice, na.rm = TRUE),
#    sd = sd(SalePrice, na.rm = TRUE))
#res.aov <- aov(SalePrice ~ PavedDrive, data = train)
#TukeyHSD(res.aov)
#pairwise.t.test(train$SalePrice, train$PavedDrive,
#               p.adjust.method = "BH", pool.sd = FALSE)
############################Wood Deck SF (transformation)#########################
# summary(train$WoodDeckSF)
# ggplot(train, aes(WoodDeckSF)) +
#   geom_histogram(binwidth = 50)
# 
# ggplot(train, aes(log(train$WoodDeckSF+1))) +
#   geom_histogram(binwidth = 50)
#Log +1 Transformation
train = train %>% mutate(WoodDeckSF = log(WoodDeckSF+1))
#Deck (Yes,No)
train = train %>% mutate(hasDeck = ifelse(WoodDeckSF == 0, 'No', 'Yes'))
############################Open Porch SF (transformation)#########################
# summary(train$OpenPorchSF)
# ggplot(train, aes(OpenPorchSF)) +
#   geom_histogram(binwidth = 50)
# 
# ggplot(train, aes(log(train$OpenPorchSF+1))) +
#   geom_histogram(binwidth = 2)
#Log +1 Transformation
train = train %>% mutate(OpenPorchSF = log(OpenPorchSF+1))
#hasOpenPorch (Yes,No)
train = train %>% mutate(hasOpenPorch = ifelse(OpenPorchSF == 0, 'No', 'Yes'))
############################Encolsed Porch(transformation)#########################
#summary(train$EnclosedPorch)
#Log +1 Transformation
train = train %>% mutate(EnclosedPorch = log(EnclosedPorch+1))
#hasEnclosedPorch (Yes,No)
train = train %>% mutate(hasEnlosedPorch = ifelse(EnclosedPorch == 0, 'No', 'Yes'))
############################Three Season Porch#########################
#table(train$`3SsnPorch`)
train = train %>% mutate(hasX3SsnPorch = ifelse(train$X3SsnPorch == 0, 'No', 'Yes'))
#res.aov <- aov(SalePrice ~ hasX3SsnPorch, data = train)
#TukeyHSD(res.aov)
train = train %>% dplyr::select(-X3SsnPorch)
############################Screen Porch#########################
#table(train$ScreenPorch)
train = train %>% mutate(hasScreenPorch = ifelse(ScreenPorch == 0, 'No', 'Yes'))
# res.aov <- aov(SalePrice ~ hasScreenPorch, data = train)
# TukeyHSD(res.aov)
#train$hasX3SsnPorch=NULL
train = train %>% mutate(ScreenPorch = log(ScreenPorch+1))
############################Pool Area#########################
#summary(log(train$PoolArea+1))
#ggplot(train, aes(PoolArea)) +
#  geom_histogram(binwidth = 50)

############################Pool QC#########################
# Ex    Excellent
# Gd    Good
# TA    Average/Typical
# Fa    Fair
# NA    No Pool
#table(train$PoolQC)
train = train %>% mutate(hasPool = ifelse(train$PoolArea == 0 ,0 , 1))
train = train %>% dplyr::select(-PoolQC)
train = train %>% dplyr::select(-PoolArea)
############################Fence#########################
#table(train$Fence)
# Fence (Ordinal): Fence quality
# 
# GdPrv Good Privacy
# MnPrv Minimum Privacy
# GdWo  Good Wood
# MnWw  Minimum Wood/Wire
# NA    No Fence
#train$Fence =NULL
#table(train$Fence)
train = train %>% mutate(hasFence = ifelse(is.na(train$Fence)==TRUE,0,1))
train = train %>% dplyr::select(-Fence)
#########################################Misc Feature####################################
#table(train$MiscFeature)
# Gar2 Othr Shed TenC 
# 2    2   49    1 
train = train %>% dplyr::select(-MiscFeature)
########################################Misc Value#####################################
#table(train$MiscVal)
train = train %>% mutate(MiscVal = log(MiscVal+1))
#ggplot(train, aes(MiscVal)) +
#  geom_histogram(binwidth = 2)
#####################################Mo Sold############################################
# ggplot(data= train) +
#   aes(x = train$MoSold,
#       y = train$SalePrice)+
#   stat_summary(fun.y=mean, geom="bar")
###################################Year Sold########################################
# ggplot(data= train) +
#   aes(x = train$YrSold,
#       y = train$SalePrice)+
#   stat_summary(aes(fill = factor(YrSold)),fun.y=mean, geom="bar")
###################################Sale Type########################################
#table(train$SaleType)
# WD    Warranty Deed - Conventional
# CWD   Warranty Deed - Cash
# VWD   Warranty Deed - VA Loan
# New   Home just constructed and sold
# COD   Court Officer Deed/Estate
# Con   Contract 15% Down payment regular terms
# ConLw Contract Low Down payment and low interest
# ConLI Contract Low Interest
# ConLD Contract Low Down
# Oth   Other
#group sale type
train = train %>% mutate(SaleType = ifelse(SaleType %in% c('WD','CWD','VWD'), 'Warranty Deed', 
                                           ifelse(SaleType %in% c('New'), 'New Home',
                                                  ifelse(SaleType %in% c('COD'), 'Court Deed/Estate',
                                                         ifelse(SaleType %in% c('ConLD', 'ConLI','ConLw'), 'Low Interest/Down Payment',
                                                                ifelse(SaleType %in% c('Con'), 'Regular Terms', 'Other'))))))
#colSums(is.na(train))
###################################Sale Condition########################################
#ggplot(train, aes(x = train$SaleCondition,y = train$SalePrice)) + geom_boxplot() + coord_flip()
# condition_anova = train %>% 
#   dplyr::group_by(.,train$SaleCondition) %>% 
#   summarise(
#     count = n(),
#     mean = mean(SalePrice, na.rm = TRUE),
#     sd = sd(SalePrice, na.rm = TRUE))
# 
# condition_anova
########################################################Sale Price (confirm transformation)
#train = train %>% mutate(SalePrice = log((SalePrice)+1))



write.csv(train,file="clean_train.csv")

