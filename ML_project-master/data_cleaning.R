library(dplyr)
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(plotly)
library(ggrepel)
library(missForest)
library(multcomp)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

table(train$MSSubClass)
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

#lot area - logged
train = train %>% mutate(LotArea = log(LotArea))
summary(train$LotArea)

#street - dropped
train$Street=NULL

#street - dropped
train$Alley = NULL

#lot shape- as is

#land contour- as is

#utilities
train$Utilities= NULL

#lot config - as is

#land slope - as is

#neighborhood - as is

#condition 1 - near positive fature, near railrod, near street, normal

train = train %>% mutate(Condition1 = ifelse(Condition1 %in% c('Artery', 'Feedr'), 'near street',
                                             ifelse(Condition1 %in% c('RRAe', 'RRAn', 'RRNe', 'RRNn'),'near railroad',
                                                    ifelse(Condition1 %in% c('PosA', 'PosN'), 'near positive feature', 'Normal'
                                                    ))))

#condition 2 - drop
train$Condition2 = NULL

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
train <- train %>%
  mutate(MasVnrType = 
           ifelse(MasVnrType %in% c('BrkCmn', 'BrkFace'), 'Brick',
                  ifelse(MasVnrType %in% c('CBlock'), 'CBlock',
                         ifelse(MasVnrType %in% c('None'), 'None', 'Stone'))))

#MasVnrArea as is

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

train$BsmtFinType1 <- as.numeric(train$BsmtFinType1)

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

train$HeatingQC <- as.numeric(train$HeatingQC)

#CentralAir as-is

######TWILLIAMS CODE######

# Electrical => As-is

# X1stFlrSF => log
train = train %>% mutate(., LogFirstFlrSF = log(X1stFlrSF))

# X2ndFlrSF => log(+1)
train = train %>% mutate(., LogSecFlrSF = log(X2ndFlrSF + 1))

# LowQualFinSF As-is

# GrLivArea => log
train = train %>% mutate(., LogGrLivArea = log(GrLivArea))

# BsmtFullBath => As-is
# BsmtHalfBath => As-is
# FullBath => As-is
# HalfBath => As-is
# BedroomAbvGR => As-is
# KitchenAbvGr => As-is

# KitchenQual => ordinal ; Po = 0, Fa = 1, TA = 2, Gd = 3, Ex = 4
kitchen_quality_converter = function(x){
  if(x == 'Po'){
    return(0)
  } else if(x == 'Fa'){
    return(1)
  } else if(x == 'TA'){
    return(2)
  } else if(x == 'Gd'){
    return(3)
  } else if(x == 'Ex'){
    return(4)
  }
}
train$OrdKitchenQual = sapply(train$KitchenQual, kitchen_quality_converter)

# TotalRmsAbvGrd => As-is

# Functional Columns => Reducing the total number of groups; Min = Min1 | Min2, Maj = Maj1 | Maj2
# Remaining groups are: Typ, Min, Mod, Maj, Sev, Sal
functional_converter = function(x){
  if( x == 'Min1' | x == 'Min2'){
    return('Min')
  } else if( x == 'Maj1' | x == 'Maj2'){
    return('Maj')
  } else {
    return(x)
  }
}
train$GroupedFuncitonal = sapply(train$Functional, functional_converter)

# Fireplaces => As-is

# FireplaceQu => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
fireplaceQu_converter = function(x, na.rm = TRUE){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'Po'){
    return(1)
  } else if(x == 'Fa'){
    return(2)
  } else if(x == 'TA'){
    return(3)
  } else if(x == 'Gd'){
    return(4)
  } else if(x == 'Ex'){
    return(5)
  }
}
train$OrdFireplaceQu = sapply(train$FireplaceQu, fireplaceQu_converter)

# GarageType => Reducing the number of groups; 2Types, Attached = Attchd | Basment | BuiltIn, CarPort, Detchd, None = NA
garagetype_converter = function(x, na.rm = TRUE){
  if(is.na(x) == TRUE){
    return('No Garage')
  } else if(x == 'Attchd' | x == 'Basment' | x == 'BuiltIn'){
    return('Attached')
  } else{
    return(x)
  }
}

train$GroupedGarageType = sapply(train$GarageType, garagetype_converter)

# Adding new field: New_Field_1 = log(YrSold - GarageYrBuilt) | I don't know why we want this 
#### There are NA values here and I'm not sure the proper way of imputing them
train = train %>% mutate(., New_Field_1 = log((YrSold - GarageYrBlt)+1))

# GarageFinish => update NA to No Garage
garagena_converter = function(x){
  if(is.na(x) == TRUE){
    return('No Garage')
  } else{
    return(x)
  }
}
train$GarageFinish = sapply(train$GarageFinish, garagena_converter)
# GarageCars => update NA to No Garage
train$GarageCars = sapply(train$GarageCars, garagena_converter)

# GarageArea => log + 1
train = train %>% mutate(., LogGarageArea = log(GarageArea + 1))

# GarageQual => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
garagequal_converter = function(x){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'Po'){
    return(1)
  } else if(x == 'Fa'){
    return(2)
  } else if(x == 'TA'){
    return(3)
  } else if(x == 'Gd'){
    return(4)
  } else if(x == 'Ex'){
    return(5)
  }
}
train$OrdGarageQual = sapply(train$GarageQual, garagequal_converter)

# GarageCond => ordinal; NA = 0, Po = 1, Fa = 2, TA = 3, Gd = 4, Ex = 5
garagecond_converter = function(x){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'Po'){
    return(1)
  } else if(x == 'Fa'){
    return(2)
  } else if(x == 'TA'){
    return(3)
  } else if(x == 'Gd'){
    return(4)
  } else if(x == 'Ex'){
    return(5)
  }
}
train$OrdGarageCond = sapply(train$GarageCond, garagecond_converter)

######RTIWARI CODE######

###########################Paved Drive Way (Ordinal) - As-Is##############################
table(train$PavedDrive)
ggplot(train, aes(x = train$PavedDrive,y = train$SalePrice)) + geom_boxplot() + coord_flip()

paved_anova = train %>% 
  dplyr::group_by(.,train$PavedDrive) %>% 
  summarise(
    count = n(),
    mean = mean(SalePrice, na.rm = TRUE),
    sd = sd(SalePrice, na.rm = TRUE))

res.aov <- aov(SalePrice ~ PavedDrive, data = train)
TukeyHSD(res.aov)

#pairwise.t.test(train$SalePrice, train$PavedDrive,
#               p.adjust.method = "BH", pool.sd = FALSE)



############################Wood Deck SF (transformation)#########################
summary(train$WoodDeckSF)
ggplot(train, aes(WoodDeckSF)) +
  geom_histogram(binwidth = 50)

ggplot(train, aes(log(train$WoodDeckSF+1))) +
  geom_histogram(binwidth = 50)

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
summary(train$EnclosedPorch)
#Log +1 Transformation
train = train %>% mutate(EnclosedPorch = log(EnclosedPorch+1))
#hasEnclosedPorch (Yes,No)
train = train %>% mutate(hasEnlosedPorch = ifelse(EnclosedPorch == 0, 'No', 'Yes'))


############################Three Season Porch#########################
table(train$X3SsnPorch)
#street - dropped
train = train %>% mutate(hasX3SsnPorch = ifelse(X3SsnPorch == 0, 'No', 'Yes'))

res.aov <- aov(SalePrice ~ hasX3SsnPorch, data = train)
TukeyHSD(res.aov)
#train$hasX3SsnPorch=NULL

train$X3SsnPorch=NULL


############################Screen Porch#########################
table(train$ScreenPorch)
train = train %>% mutate(hasScreenPorch = ifelse(ScreenPorch == 0, 'No', 'Yes'))

res.aov <- aov(SalePrice ~ hasScreenPorch, data = train)
TukeyHSD(res.aov)
#train$hasX3SsnPorch=NULL

train = train %>% mutate(ScreenPorch = log(ScreenPorch+1))



############################Pool Area#########################
#summary(log(train$PoolArea+1))
#ggplot(train, aes(PoolArea)) +
#  geom_histogram(binwidth = 50)

train$PoolArea=NULL


############################Pool QC#########################
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool
poolQc_converter = function(x, na.rm = TRUE){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'Fa'){
    return(1)
  } else if(x == 'TA'){
    return(2)
  } else if(x == 'Gd'){
    return(3)
  } else if(x == 'Ex'){
    return(4)
  }
}
train$PoolQC = sapply(train$PoolQC, poolQc_converter)
table(train$PoolQC)
train = train %>% mutate(hasPool = ifelse(train$PoolQC == 0 ,0,1))
train$PoolQC=NULL


############################Fence#########################
table(train$Fence)
# Fence (Ordinal): Fence quality
# 
# GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence
fence_converter = function(x, na.rm = TRUE){
  if(is.na(x) == TRUE){
    return(0)
  } else if(x == 'MnWw'){
    return(1)
  } else if(x == 'GdWo'){
    return(2)
  } else if(x == 'MnPrv'){
    return(3)
  } else if(x == 'GdPrv'){
    return(4)
  }
}
train$Fence = sapply(train$Fence, fence_converter)
table(train$Fence)
#train = train %>% mutate(hasFence = ifelse(train$Fence == 0 ,0,1))
#train$Fence=NULL

#########################################Misc Feature####################################
table(train$MiscFeature)
# Gar2 Othr Shed TenC 
# 2    2   49    1 
train$MiscFeature=NULL

########################################Misc Value#####################################
table(train$MiscVal)

train = train %>% mutate(MiscVal = log(MiscVal+1))

#ggplot(train, aes(MiscVal)) +
#  geom_histogram(binwidth = 2)

#####################################Mo Sold############################################

ggplot(data= train) +
  aes(x = train$MoSold,
      y = train$SalePrice)+
  stat_summary(fun.y=mean, geom="bar")

###################################Year Sold########################################
ggplot(data= train) +
  aes(x = train$YrSold,
      y = train$SalePrice)+
  stat_summary(aes(fill = factor(YrSold)),fun.y=mean, geom="bar")

###################################Sale Type########################################
table(train$SaleType)
# WD 	Warranty Deed - Conventional
# CWD	Warranty Deed - Cash
# VWD	Warranty Deed - VA Loan
# New	Home just constructed and sold
# COD	Court Officer Deed/Estate
# Con	Contract 15% Down payment regular terms
# ConLw	Contract Low Down payment and low interest
# ConLI	Contract Low Interest
# ConLD	Contract Low Down
# Oth	Other

#group sale type
train = train %>% mutate(SaleType = ifelse(SaleType %in% c('WD','CWD','VWD'), 'Warranty Deed', 
                                           ifelse(SaleType %in% c('New'), 'New Home',
                                                  ifelse(SaleType %in% c('COD'), 'Court Deed/Estate',
                                                         ifelse(SaleType %in% c('ConLD', 'ConLI','ConLw'), 'Low Interest/Down Payment',
                                                                ifelse(SaleType %in% c('Con'), 'Regular Terms', 'Other'))))))

###################################Sale Condition########################################
ggplot(train, aes(x = train$SaleCondition,y = train$SalePrice)) + geom_boxplot() + coord_flip()

condition_anova = train %>% 
  dplyr::group_by(.,train$SaleCondition) %>% 
  summarise(
    count = n(),
    mean = mean(SalePrice, na.rm = TRUE),
    sd = sd(SalePrice, na.rm = TRUE))

condition_anova


########################################################Sale Price (confirm transformation)
#train = train %>% mutate(SalePrice = log((SalePrice)+1))