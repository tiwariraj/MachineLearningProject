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

test <- read.csv("test.csv",stringsAsFactors = FALSE)
#train <- read.csv("train.csv",stringsAsFactors = FALSE)

###########New Fields#################
#total # of Half bathrooms
test = test %>%  mutate(TotalHalfBath = test$HalfBath + test$BsmtHalfBath)
test$TotalHalfBath[is.na(test$TotalHalfBath)] <- 0

#total # of Full bathrooms
test = test %>%  mutate(TotalFullBath = test$FullBath + test$BsmtFullBath)
test$TotalFullBath[is.na(test$TotalFullBath)] <- 0

#total SF (Finished)
test = test %>%  mutate(TotalFinSF = test$GrLivArea + test$BsmtFinSF1 + test$BsmtFinSF2 + test$LowQualFinSF)
test$TotalFinSF[is.na(test$TotalFinSF)] <- 0
test = test %>% mutate(., TotalFinSF = log(TotalFinSF+1))

#Avg Size Rooms Abv Ground
test = test %>%  mutate(AbvGrndRoomSize = (test$GrLivArea/ (test$TotRmsAbvGrd)))

#split it into 1 story, 1.5 stories, 2 stories, split, projects
test = test %>% mutate(MSSubClass = ifelse(MSSubClass %in% c(20), '1-Story (Newer)', 
                                           ifelse(MSSubClass %in% c(30,40), '1-Story (Older)',
                                                  ifelse(MSSubClass %in% c(45,50), '1-1/2 Story',
                                                         ifelse(MSSubClass %in% c(60, 75), '2-story (Newer)',
                                                                ifelse(MSSubClass %in% c(70), '2-story (Older)',
                                                                       ifelse(MSSubClass %in% c(80,85), 'Split',
                                                                              ifelse(MSSubClass %in% c(90), 'Duplex', 'Projects'))))))))
#split zoning into medium-high, low, floating, and miscelanneoius
test = test %>% mutate(MSZoning = ifelse(MSZoning %in% c('RH', 'RM'), 'Medium-High Density',
                                           ifelse(MSZoning == 'RL', 'Low Density',
                                                  ifelse(MSZoning == 'FV', 'Floating', 'Other'))))
test <- test %>%
  mutate(MSZoning = 
           ifelse(is.na(MSZoning)==TRUE,'Other',MSZoning))

#lot area - logged
test = test %>% mutate(LotArea = log(LotArea))
#summary(test$LotArea)
#street - dropped
test = test %>% dplyr::select(-Street)
#street - dropped
test = test %>% dplyr::select(-Alley)
#lot shape- as is
#land contour- as is
#utilities
test = test %>% dplyr::select(-Utilities)
#lot config - as is
#land slope - as is
#neighborhood - as is
#condition 1 - near positive fature, near railrod, near street, normal
test = test %>% mutate(Condition1 = ifelse(Condition1 %in% c('Artery', 'Feedr'), 'Near Street',
                                             ifelse(Condition1 %in% c('RRAe', 'RRAn', 'RRNe', 'RRNn'),'Near Railroad',
                                                    ifelse(Condition1 %in% c('PosA', 'PosN'), 'Near Positive Feature', 'Normal'
                                                    ))))
#condition 2 - drop
test = test %>% dplyr::select(-Condition2)
#building type- as is
#housestyle
test = test %>% mutate(HouseStyle = ifelse(HouseStyle %in% c('1.5Fin', '1.5Unf'), '1-1/2-story',
                                           ifelse(HouseStyle %in% c('1Story'), '1-story',
                                                  ifelse(HouseStyle %in% c('2.5Fin', '2.5Unf', '2Story'), '2-story', 'Split'))))
#overalqual as it
#overalcond as is
#year remolded
test = test %>% mutate(YearRemodAdd = ifelse(YearRemodAdd == YearBuilt, 1, 0))
#year build- diff logged
test = test %>% mutate(YearBuilt = log((YrSold - YearBuilt)+1))
######MHUNG CODE######
#leaving RoofStyle as is
#RoofMatl
test <- test %>%
  mutate(RoofMatl = 
           ifelse(RoofMatl %in% c('ClyTile', 'CompShg'), 'Shingle',
                  ifelse(RoofMatl %in% c('Membrane'), 'Membrane',
                         ifelse(RoofMatl %in% c('Metal', 'Roll'), 'Metal',
                                ifelse(RoofMatl %in% c('WdShake', 'WdShngl'), 'Wood', 'Gravel & Tar')))))
#Exterior1st
test <- test %>%
  mutate(Exterior1st = 
           ifelse(Exterior1st %in% c('AsbShng', 'AsphShn'), 'Shingle',
                  ifelse(Exterior1st %in% c('BrkComm', 'BrkFace'), 'Brick',
                         ifelse(Exterior1st %in% c('CBlock', 'CemntBd', 'PreCast'), 'Concrete',
                                ifelse(Exterior1st %in% c('HdBoard', 'Plywood'), 'Ply',
                                       ifelse(Exterior1st %in% c('ImStucc', 'VinylSd'), 'Synthetic',
                                              ifelse(Exterior1st %in% c('Other'), 'Other',
                                                     ifelse(Exterior1st %in% c('Stone'), 'Stone', 'Stucco'))))))))
#Exterior2nd
test <- test %>%
  mutate(Exterior2nd = 
           ifelse(Exterior2nd %in% c('AsbShng', 'AsphShn'), 'Shingle',
                  ifelse(Exterior2nd %in% c('BrkComm', 'BrkFace'), 'Brick',
                         ifelse(Exterior2nd %in% c('CBlock', 'CemntBd', 'PreCast'), 'Concrete',
                                ifelse(Exterior2nd %in% c('HdBoard', 'Plywood'), 'Ply',
                                       ifelse(Exterior2nd %in% c('ImStucc', 'VinylSd'), 'Synthetic',
                                              ifelse(Exterior2nd %in% c('Other'), 'Other',
                                                     ifelse(Exterior2nd %in% c('Stone'), 'Stone', 'Stucco'))))))))
#MasVnrType
#table(test$MasVnrType)
#table(test$MasVnrType)
test <- test %>%
  mutate(MasVnrType = 
           ifelse(MasVnrType %in% c('BrkCmn', 'BrkFace'), 'Brick',
                  ifelse(MasVnrType %in% c('CBlock'), 'CBlock',
                         ifelse(MasVnrType %in% c('None'), 'None', 'Stone'))))

test$MasVnrType[is.na(test$MasVnrType)] <- "None"

#sum(is.na(test$MasVnrType))
#MasVnrArea as is
test = test %>% mutate(MasVnrArea = ifelse(is.na(MasVnrArea)== TRUE, 0, MasVnrArea ))
test = test %>% mutate(MasVnrArea = log(MasVnrArea + 1))
#ExterQual
test <- test %>%
  mutate(ExterQual = 
           ifelse(ExterQual == 'Ex', '4',
                  ifelse(ExterQual == 'Gd', '3',
                         ifelse(ExterQual == 'TA', '2',
                                ifelse(ExterQual == 'Fa', '1', '0')))))
#ExterCond
test <- test %>%
  mutate(ExterCond = 
           ifelse(ExterCond == 'Ex', '4',
                  ifelse(ExterCond == 'Gd', '3',
                         ifelse(ExterCond == 'TA', '2',
                                ifelse(ExterCond == 'Fa', '1', '0')))))
#Foundation as is
#BsmtQual
test <- test %>%
  mutate(BsmtQual = 
           ifelse(BsmtQual == 'Ex', '5',
                  ifelse(BsmtQual == 'Gd', '4',
                         ifelse(BsmtQual == 'TA', '3',
                                ifelse(BsmtQual == 'Fa', '2', 
                                       ifelse(BsmtQual == 'Po', '1', '0'))))))
test$BsmtQual[is.na(test$BsmtQual)] <- 0
test$BsmtQual <- as.numeric(test$BsmtQual)
#BsmtCond
test <- test %>%
  mutate(BsmtCond = 
           ifelse(BsmtCond == 'Ex', '5',
                  ifelse(BsmtCond == 'Gd', '4',
                         ifelse(BsmtCond == 'TA', '3',
                                ifelse(BsmtCond == 'Fa', '2', 
                                       ifelse(BsmtCond == 'Po', '1', '0'))))))
test$BsmtCond[is.na(test$BsmtCond)] <- 0
test$BsmtCond <- as.numeric(test$BsmtCond)
#BsmtExposure
test <- test %>%
  mutate(BsmtExposure = 
           ifelse(BsmtExposure == 'Gd', '4',
                  ifelse(BsmtExposure == 'Av', '3',
                         ifelse(BsmtExposure == 'Mn', '2',
                                ifelse(BsmtExposure == 'No', '1', '0')))))
test$BsmtExposure[is.na(test$BsmtExposure)] <- 0
test$BsmtExposure <- as.numeric(test$BsmtExposure)
#BsmtFinType1
test <- test %>%
  mutate(BsmtFinType1 = 
           ifelse(BsmtFinType1 == 'GLQ', '6',
                  ifelse(BsmtFinType1 == 'ALQ', '5',
                         ifelse(BsmtFinType1 == 'BLQ', '4',
                                ifelse(BsmtFinType1 == 'Rec', '3',
                                       ifelse(BsmtFinType1 == 'LwQ', '2',
                                              ifelse(BsmtFinType1 == 'Unf', '1', '0')))))))

test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 0

#BsmtFinSF1
test = test %>% mutate(BsmtFinSF1 = log(BsmtFinSF1 + 1))
test = test %>% mutate(BsmtFinSF1 = ifelse(is.na(BsmtFinSF1)== TRUE, 0, BsmtFinSF1 ))

#BsmtFinType2
test <- test %>%
  mutate(BsmtFinType2 = 
           ifelse(BsmtFinType2 == 'GLQ', '6',
                  ifelse(BsmtFinType2 == 'ALQ', '5',
                         ifelse(BsmtFinType2 == 'BLQ', '4',
                                ifelse(BsmtFinType2 == 'Rec', '3',
                                       ifelse(BsmtFinType2 == 'LwQ', '2',
                                              ifelse(BsmtFinType2 == 'Unf', '1', '0')))))))
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 0
test$BsmtFinType2 <- as.numeric(test$BsmtFinType2)

#BsmtFinSF2
test = test %>% mutate(BsmtFinSF2 = log(BsmtFinSF2 + 1))
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0

#BsmtUnfSF
test = test %>% mutate(BsmtUnfSF = log(BsmtUnfSF + 1))
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0

#TotalBsmtSF
test = test %>% mutate(TotalBsmtSF = log(TotalBsmtSF + 1))
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0

#Heating (think we should leave as is after more research)
#HeatingQC
test <- test %>%
  mutate(HeatingQC = 
           ifelse(HeatingQC == 'Ex', '4',
                  ifelse(HeatingQC == 'Gd', '3',
                         ifelse(HeatingQC == 'TA', '2',
                                ifelse(HeatingQC == 'Fa', '1', '0')))))

test$HeatingQC[is.na(test$HeatingQC)] <- 0
test$HeatingQC <- as.numeric(test$HeatingQC)
#CentralAir as-is

# read in whole testing set
# Electrical =>
test <- test %>%
  mutate(Electrical = 
           ifelse(is.na(Electrical)==TRUE,'None',Electrical))

# X1stFlrSF => log
test = test %>% mutate(., X1stFlrSF = log(X1stFlrSF + 1))
# X2ndFlrSF => log(+1)
test = test %>% mutate(., X2ndFlrSF = log(X2ndFlrSF + 1))
# LowQualFinSF As-is
test = test %>% mutate(., LowQualFinSF = log(LowQualFinSF+1))
# GrLivArea => log
test = test %>% mutate(., GrLivArea = log(GrLivArea))
# BsmtFullBath => As-is
test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0

# BsmtHalfBath => As-is
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0
# FullBath => As-is

# HalfBath => As-is

# BedroomAbvGR => As-is

# KitchenAbvGr => As-is

# KitchenQual => ordinal ; Po = 0, Fa = 1, TA = 2, Gd = 3, Ex = 4
test = test %>% mutate(KitchenQual = ifelse(is.na(KitchenQual)==TRUE,0,
                                            ifelse(KitchenQual =='Po',1,
                                                   ifelse(KitchenQual == 'Fa', 2,
                                                          ifelse(KitchenQual == 'TA', 3,
                                                                 ifelse(KitchenQual == 'Gd', 4,5))))))
# TotalRmsAbvGrd => As-is
# Functional Columns => Reducing the total number of groups; Min = Min1 | Min2, Maj = Maj1 | Maj2
# Remaining groups are: Typ, Min, Mod, Maj, Sev, Sal
test = test %>% mutate(Functional = ifelse(is.na(Functional)==TRUE,'Unknown',
                                             ifelse(Functional %in% c('Typ'),'Typical',
                                                    ifelse(Functional %in% c('Min1', 'Min2'), 'Minor',
                                                           ifelse(Functional %in% c('Maj1', 'Maj2'), 'Major',
                                                                  ifelse(Functional == 'Sev', 'Severe',
                                                                         ifelse(Functional == 'Sal', 'Salvage')))))))
# Fireplaces => As-is
# FireplaceQu => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
test = test %>% mutate(FireplaceQu = ifelse(is.na(FireplaceQu)==TRUE,0,
                                            ifelse(FireplaceQu =='Po',1,
                                                   ifelse(FireplaceQu == 'Fa', 2,
                                                          ifelse(FireplaceQu == 'TA', 3,
                                                                 ifelse(FireplaceQu == 'Gd', 4,5))))))
# GarageType => Reducing the number of groups; 2Types, Attached = Attchd | Basment | BuiltIn, CarPort, Detchd, No Garage = NA
test = test %>% mutate(GarageType = ifelse(is.na(GarageType)==TRUE,0,
                                           ifelse(GarageType =='Po',1,
                                                  ifelse(GarageType == 'Fa', 2,
                                                         ifelse(GarageType == 'TA', 3,
                                                                ifelse(GarageType == 'Gd', 4,5))))))
# Adding new field: New_Field_1 = log(YrSold - GarageYrBuilt) | I don't know why we want this 
#### There are NA values here and I'm not sure the proper way of imputing them
test <- test %>%
  mutate(GarageYrBlt = 
           ifelse(is.na(GarageYrBlt)==TRUE,0,GarageYrBlt))

test = test %>% mutate(., YrSold_Garage_Difference = log((YrSold - GarageYrBlt)+1))
test <- test %>%
  mutate(YrSold_Garage_Difference = 
           ifelse(is.na(YrSold_Garage_Difference)==TRUE,0,YrSold_Garage_Difference))
test$YrSold_Garage_Difference[is.infinite(test$YrSold_Garage_Difference)] <- 0

# GarageFinish => update NA to No Garage
test = test %>% mutate(GarageFinish = ifelse(is.na(GarageFinish)==TRUE,0,
                                             ifelse(GarageFinish =='Po',1,
                                                    ifelse(GarageFinish == 'Fa', 2,
                                                           ifelse(GarageFinish == 'TA', 3,
                                                                  ifelse(GarageFinish == 'Gd', 4,5))))))
# GarageCars => update NA to No Garage
test = test %>% mutate(GarageCars = ifelse(is.na(GarageCars)==TRUE,0,GarageCars))
# GarageArea => log + 1
test = test %>% mutate(., GarageArea = log(GarageArea + 1))
test <- test %>%
  mutate(GarageArea = 
           ifelse(is.na(GarageArea)==TRUE,0,GarageArea))
# GarageQual => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
test = test %>% mutate(GarageQual = ifelse(is.na(GarageQual)==TRUE,0,
                                           ifelse(GarageQual =='Po',1,
                                                  ifelse(GarageQual == 'Fa', 2,
                                                         ifelse(GarageQual == 'TA', 3,
                                                                ifelse(GarageQual == 'Gd', 4,5))))))
test = test %>% mutate(GarageCond = ifelse(is.na(GarageCond)==TRUE,0,
                                           ifelse(GarageCond =='Po',1,
                                                  ifelse(GarageCond == 'Fa', 2,
                                                         ifelse(GarageCond == 'TA', 3,
                                                                ifelse(GarageCond == 'Gd', 4,5))))))
######RTIWARI CODE######
###########################Paved Drive Way (Ordinal) - As-Is##############################
#table(test$PavedDrive)
#ggplot(test, aes(x = test$PavedDrive,y = test$SalePrice)) + geom_boxplot() + coord_flip()
#paved_anova = test %>% 
#  dplyr::group_by(.,test$PavedDrive) %>% 
#  summarise(
#    count = n(),
#    mean = mean(SalePrice, na.rm = TRUE),
#    sd = sd(SalePrice, na.rm = TRUE))
#res.aov <- aov(SalePrice ~ PavedDrive, data = test)
#TukeyHSD(res.aov)
#pairwise.t.test(test$SalePrice, test$PavedDrive,
#               p.adjust.method = "BH", pool.sd = FALSE)
############################Wood Deck SF (transformation)#########################
# summary(test$WoodDeckSF)
# ggplot(test, aes(WoodDeckSF)) +
#   geom_histogram(binwidth = 50)
# 
# ggplot(test, aes(log(test$WoodDeckSF+1))) +
#   geom_histogram(binwidth = 50)
#Log +1 Transformation
test = test %>% mutate(WoodDeckSF = log(WoodDeckSF+1))
#Deck (Yes,No)
test = test %>% mutate(hasDeck = ifelse(WoodDeckSF == 0, 'No', 'Yes'))
############################Open Porch SF (transformation)#########################
# summary(test$OpenPorchSF)
# ggplot(test, aes(OpenPorchSF)) +
#   geom_histogram(binwidth = 50)
# 
# ggplot(test, aes(log(test$OpenPorchSF+1))) +
#   geom_histogram(binwidth = 2)
#Log +1 Transformation
test = test %>% mutate(OpenPorchSF = log(OpenPorchSF+1))
#hasOpenPorch (Yes,No)
test = test %>% mutate(hasOpenPorch = ifelse(OpenPorchSF == 0, 'No', 'Yes'))
############################Encolsed Porch(transformation)#########################
#summary(test$EnclosedPorch)
#Log +1 Transformation
test = test %>% mutate(EnclosedPorch = log(EnclosedPorch+1))
#hasEnclosedPorch (Yes,No)
test = test %>% mutate(hasEnlosedPorch = ifelse(EnclosedPorch == 0, 'No', 'Yes'))
############################Three Season Porch#########################
#table(test$`3SsnPorch`)
test = test %>% mutate(hasX3SsnPorch = ifelse(test$X3SsnPorch == 0, 'No', 'Yes'))
#res.aov <- aov(SalePrice ~ hasX3SsnPorch, data = test)
#TukeyHSD(res.aov)
test = test %>% dplyr::select(-X3SsnPorch)
############################Screen Porch#########################
#table(test$ScreenPorch)
test = test %>% mutate(hasScreenPorch = ifelse(ScreenPorch == 0, 'No', 'Yes'))
# res.aov <- aov(SalePrice ~ hasScreenPorch, data = test)
# TukeyHSD(res.aov)
#test$hasX3SsnPorch=NULL
test = test %>% mutate(ScreenPorch = log(ScreenPorch+1))
############################Pool Area#########################
#summary(log(test$PoolArea+1))
#ggplot(test, aes(PoolArea)) +
#  geom_histogram(binwidth = 50)

############################Pool QC#########################
# Ex    Excellent
# Gd    Good
# TA    Average/Typical
# Fa    Fair
# NA    No Pool
#table(test$PoolQC)
test = test %>% mutate(hasPool = ifelse(test$PoolArea == 0 ,0 , 1))
test = test %>% dplyr::select(-PoolQC)

test = test %>% dplyr::select(-PoolArea)
############################Fence#########################
#table(test$Fence)
# Fence (Ordinal): Fence quality
# 
# GdPrv Good Privacy
# MnPrv Minimum Privacy
# GdWo  Good Wood
# MnWw  Minimum Wood/Wire
# NA    No Fence
#test$Fence =NULL
#table(test$Fence)
test = test %>% mutate(hasFence = ifelse(is.na(test$Fence)==TRUE,0,1))
test = test %>% dplyr::select(-Fence)
#########################################Misc Feature####################################
#table(test$MiscFeature)
# Gar2 Othr Shed TenC 
# 2    2   49    1 
test = test %>% dplyr::select(-MiscFeature)
########################################Misc Value#####################################
#table(test$MiscVal)
test = test %>% mutate(MiscVal = log(MiscVal+1))
#ggplot(test, aes(MiscVal)) +
#  geom_histogram(binwidth = 2)
#####################################Mo Sold############################################
# ggplot(data= test) +
#   aes(x = test$MoSold,
#       y = test$SalePrice)+
#   stat_summary(fun.y=mean, geom="bar")
###################################Year Sold########################################
# ggplot(data= test) +
#   aes(x = test$YrSold,
#       y = test$SalePrice)+
#   stat_summary(aes(fill = factor(YrSold)),fun.y=mean, geom="bar")
###################################Sale Type########################################
#table(test$SaleType)
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
test = test %>% mutate(SaleType = ifelse(SaleType %in% c('WD','CWD','VWD'), 'Warranty Deed', 
                                           ifelse(SaleType %in% c('New'), 'New Home',
                                                  ifelse(SaleType %in% c('COD'), 'CourtDeed_Estate',
                                                         ifelse(SaleType %in% c('ConLD', 'ConLI','ConLw'), 'LowInterest_DownPayment',
                                                                ifelse(SaleType %in% c('Con'), 'Regular Terms', 'Other'))))))
#colSums(is.na(test))
###################################Sale Condition########################################
#ggplot(test, aes(x = test$SaleCondition,y = test$SalePrice)) + geom_boxplot() + coord_flip()
# condition_anova = test %>% 
#   dplyr::group_by(.,test$SaleCondition) %>% 
#   summarise(
#     count = n(),
#     mean = mean(SalePrice, na.rm = TRUE),
#     sd = sd(SalePrice, na.rm = TRUE))
# 
# condition_anova
########################################################Sale Price (confirm transformation)
#test = test %>% mutate(SalePrice = log((SalePrice)+1))


#lot frontage
LF_impute <- mice(test, m=5, maxit = 50, method = 'pmm', seed = 500)

counter = 1
for (i in 1:1459) {
  if (is.na(test$LotFrontage[i]) == TRUE) {
    test$LotFrontage[i] = as.numeric(rowMeans(LF_impute$imp$LotFrontage))[counter]
    counter = counter + 1
  }
}


write.csv(test,file="clean_test.csv")


