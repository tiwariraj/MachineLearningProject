###NYCDSA Bootcamp 12, Machine Learning Project###
###Raj Tiwari###

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(plotly)
library(ggrepel)
library(missForest)
library(multcomp)

#setwd("~/NYCDSA/Bootcamp/Projects/Machine Learning/Data")
train = read.csv("train.csv",stringsAsFactors = FALSE)
test = read.csv("test.csv",stringsAsFactors = FALSE)         

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
