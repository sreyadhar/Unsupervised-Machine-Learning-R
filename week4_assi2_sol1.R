# # -*- coding: utf-8 -*-
# """Week4 Assi2 Sol1.ipynb
# 
# Automatically generated by Colaboratory.
# 
# Original file is located at
#     https://colab.research.google.com/drive/1ewQwyRSZvPfGahiLcEwm1oUjFk3-zrCO
# """

###########################################################################
## Week-4, Homework-2, Sol-2 
## Sreya Dhar 
## Created: Feb 25, 2021
## Edited: Mar 03, 2021
###########################################################################

rm(list=ls())
setwd("C:/File E/EAS 507 Statistical Mining II/Week-4/HW-2")


## installing all the libaries in R kernel

# install.packages("arules")
# install.packages("Hmisc")
# install.packages("funModeling")
# install.packages("PerformanceAnalytics")
# install.packages("corrplot")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("repr")
# # install.packages("ggstatsplot")
# install.packages("psych")
# install.packages("gplots")
# install.packages("rsample")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("RColorBrewer")
# install.packages("partykit")
# install.packages("party")

## importing the libraries in R kernel

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(partykit)
library(party)
library(arules)
library(MASS)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(funModeling) 
library(tidyverse)
library(tidyr)
library(PerformanceAnalytics)
library(corrplot)
library(repr)
# library(ggstatsplot)
library(psych)
library("gplots")
library(rsample)

# """**Data Processing or Exploratory Data Analysis on 'Auto' Dataset**"""

load('marketing.RData')

head(marketing)
# View(marketing)

glimpse(marketing) ## overall view of Auto dataset

sapply(marketing, class)

describe(marketing) ## description of the overall dataset,

profiling_num(marketing)

status(marketing)

summary(marketing)

### Step 1 - generate a reference set ####
set.seed(1234)
N = dim(marketing)[1]
ref_store <- c()
for (i in 1:14){
  variable <- marketing[ ,i]
  uni <- na.omit(unique(variable))
  temp <- sample(uni, N, replace = TRUE)
  ref_store <- cbind(ref_store, temp)
}
colnames(ref_store) <- colnames(marketing)[1:14]

### Step 2 - combine data and reference set ####
combo <- rbind(marketing[,1:14], ref_store)
dim(combo)

### Step 3 - create a response variable ####
Y_dats <- rep(1, N)
Y_ref <- rep(0, N)
YY <- c(Y_dats, Y_ref)
YY
# tail(YY)

### combining the predictor variables with response variable ###
market_combo <- data.frame(combo, YY)
dim(market_combo)
head(market_combo)

### alter the numerical variables to ordinal/categorical ones: ###
### source:: https://web.stanford.edu/~hastie/ElemStatLearn/   ###

### income ##
unique(market_combo$Income)
lessthan10k <- which(market_combo$Income == 1)
tento15k <- which(market_combo$Income == 2)
fifteento20k <- which(market_combo$Income == 3)
twentyto25k <- which(market_combo$Income == 4)
twentyfiveto30k <- which(market_combo$Income == 5)
thirtyto40k <- which(market_combo$Income == 6)
fortyto50 <- which(market_combo$Income == 7)
fiftyto75 <- which(market_combo$Income == 8)
morethan75 <- which(market_combo$Income == 9)

market_combo$Income[lessthan10k] <- "lessthan10k"
market_combo$Income[tento15k] <- "tento15k"
market_combo$Income[fifteento20k] <- "fifteento20k"
market_combo$Income[twentyto25k] <- "twentyto25k"
market_combo$Income[twentyfiveto30k] <- "twentyfiveto30k"
market_combo$Income[thirtyto40k] <- "thirtyto40k"
market_combo$Income[fortyto50] <- "fortyto50"
market_combo$Income[fiftyto75] <- "fiftyto75"
market_combo$Income[morethan75] <- "morethan75"
market_combo$Income <- as.factor(market_combo$Income)

### Sex ###
unique(market_combo$Sex)
male <- which(market_combo$Sex == 1)
market_combo$Sex[male] <- "Male"
market_combo$Sex[-male] <- "Female"
market_combo$Sex <- as.factor(market_combo$Sex)

### Marital ##
unique(market_combo$Marital)
married <- which(market_combo$Marital == 1)
liveIn <- which(market_combo$Marital == 2)
devorce <- which(market_combo$Marital == 3)
widow <- which(market_combo$Marital == 4)
single <- which(market_combo$Marital == 5)

market_combo$Marital[married] <- "married"
market_combo$Marital[liveIn] <- "liveIn"
market_combo$Marital[devorce] <- "divorce"
market_combo$Marital[single] <- "single"
market_combo$Marital[widow] <- "widow"
market_combo$Marital <- as.factor(market_combo$Marital)

### Age ##
thru14to17 <- which(market_combo$Age == 1)
thru18to24 <- which(market_combo$Age == 2)
thru25to34 <- which(market_combo$Age == 3)
thru35to44 <- which(market_combo$Age == 4)
thru45to54 <- which(market_combo$Age == 5)
thru55to64 <- which(market_combo$Age == 6)
over65     <- which(market_combo$Age == 7)

market_combo$Age[thru14to17] <- "thru14to17"
market_combo$Age[thru18to24] <- "thru18to24"
market_combo$Age[thru25to34] <- "thru25to34"
market_combo$Age[thru35to44] <- "thru35to44"
market_combo$Age[thru45to54] <- "thru45to54"
market_combo$Age[thru55to64] <- "thru55to64"
market_combo$Age[over65] <- "over65"
market_combo$Age <- as.factor(market_combo$Age)

### Education ##
grade8orless <- which(market_combo$Edu == 1)
grade9to11 <- which(market_combo$Edu == 2)
highschool <- which(market_combo$Edu == 3)
associatecol <- which(market_combo$Edu == 4)
undergrad <- which(market_combo$Edu == 5)
gradstudy <- which(market_combo$Edu == 6)

market_combo$Edu[grade8orless] <- "grade8orless"
market_combo$Edu[grade9to11] <- "grade9to11"
market_combo$Edu[highschool] <- "highschool"
market_combo$Edu[associatecol] <- "associatecol"
market_combo$Edu[undergrad] <- "undergrad"
market_combo$Edu[gradstudy] <- "gradstudy"
market_combo$Edu <- as.factor(market_combo$Edu)

### Occupation ###

# OCCUPATION
#              1. Professional/Managerial
#              2. Sales Worker
#              3. Factory Worker/Laborer/Driver
#              4. Clerical/Service Worker
#              5. Homemaker
#              6. Student, HS or College
#              7. Military
#              8. Retired
#              9. Unemployed
 
Professional <- which(market_combo$Occupation == 1)
Sales <- which(market_combo$Occupation == 2)
Factory <- which(market_combo$Occupation == 3)
Service <- which(market_combo$Occupation == 4)
Homemaker <- which(market_combo$Occupation == 5)
Student <- which(market_combo$Occupation == 6)
Military <- which(market_combo$Occupation == 7)
Retired <- which(market_combo$Occupation == 8)
Unemployed <- which(market_combo$Occupation == 9)


market_combo$Occupation[Professional] <- "Professional"
market_combo$Occupation[Sales] <- "Sales"
market_combo$Occupation[Factory] <- "Factory"
market_combo$Occupation[Service] <- "Service"
market_combo$Occupation[Homemaker] <- "Homemaker"
market_combo$Occupation[Student] <- "Student"
market_combo$Occupation[Military] <- "Military"
market_combo$Occupation[Retired] <- "Retired"
market_combo$Occupation[Unemployed] <- "Unemployed"
market_combo$Occupation <- as.factor(market_combo$Occupation)

### Lived	###
#  7    HOW LONG HAVE YOU LIVED IN THE SAN FRAN./OAKLAND/SAN JOSE AREA?
#              1. Less than one year
#              2. One to three years
#              3. Four to six years
#              4. Seven to ten years
#              5. More than ten years

lessThanYear <- which(market_combo$Lived == 1)
oneToThree <- which(market_combo$Lived == 2)
fourToSix <- which(market_combo$Lived == 3)
sevenToTen <- which(market_combo$Lived == 4)
moreThanTen <- which(market_combo$Lived == 5)


market_combo$Lived[lessThanYear] <- "lessThanYear"
market_combo$Lived[oneToThree] <- "oneToThree"
market_combo$Lived[fourToSix] <- "fourToSix"
market_combo$Lived[sevenToTen] <- "sevenToTen"
market_combo$Lived[moreThanTen] <- "moreThanTen"

market_combo$Lived <- as.factor(market_combo$Lived)

### Dual_Income	###
    # DUAL INCOMES (IF MARRIED)
    #          1. Not Married
    #          2. Yes
    #          3. No

Not_Married <- which(market_combo$Dual_Income == 1)
Yes <- which(market_combo$Dual_Income == 2)
No <- which(market_combo$Dual_Income == 3)

market_combo$Dual_Income[Not_Married] <- "Not_Married"
market_combo$Dual_Income[Yes] <- "Yes"
market_combo$Dual_Income[No] <- "No"
market_combo$Dual_Income <- as.factor(market_combo$Dual_Income)

### Household	###

# PERSONS IN YOUR HOUSEHOLD
#              1. One
#              2. Two
#              3. Three
#              4. Four
#              5. Five
#              6. Six
#              7. Seven
#              8. Eight
#              9. Nine or more

# unique(market_combo$Household)
One <- which(market_combo$Household == 1)
Two <- which(market_combo$Household == 2)
Three <- which(market_combo$Household == 3)
Four <- which(market_combo$Household == 4)
Five <- which(market_combo$Household == 5)
Six <- which(market_combo$Household == 6)
Seven <- which(market_combo$Household == 7)
Eight <- which(market_combo$Household == 8)
Nine_or_more <- which(market_combo$Household == 9)

market_combo$Household[One] <- "One"
market_combo$Household[Two] <- "Two"
market_combo$Household[Three] <- "Three"
market_combo$Household[Four] <- "Four"
market_combo$Household[Five] <- "Five"
market_combo$Household[Six] <- "Six"
market_combo$Household[Seven] <- "Seven"
market_combo$Household[Eight] <- "Eight"
market_combo$Household[Nine_or_more] <- "Nine_or_more"
market_combo$Household <- as.factor(market_combo$Household)

### Householdu18 ###

  #  PERSONS IN HOUSEHOLD UNDER 18
  #            0. None
  #            1. One
  #            2. Two
  #            3. Three
  #            4. Four
  #            5. Five
  #            6. Six
  #            7. Seven
  #            8. Eight
  #            9. Nine or more

# unique(market_combo$Householdu18)
zero <- which(market_combo$Householdu18 == 0)
One <- which(market_combo$Householdu18 == 1)
Two <- which(market_combo$Householdu18 == 2)
Three <- which(market_combo$Householdu18 == 3)
Four <- which(market_combo$Householdu18 == 4)
Five <- which(market_combo$Householdu18 == 5)
Six <- which(market_combo$Householdu18 == 6)
Seven <- which(market_combo$Householdu18 == 7)
Eight <- which(market_combo$Householdu18 == 8)
Nine_or_more <- which(market_combo$Householdu18 == 9)

market_combo$Householdu18[zero] <- "zero"
market_combo$Householdu18[One] <- "One"
market_combo$Householdu18[Two] <- "Two"
market_combo$Householdu18[Three] <- "Three"
market_combo$Householdu18[Four] <- "Four"
market_combo$Householdu18[Five] <- "Five"
market_combo$Householdu18[Six] <- "Six"
market_combo$Householdu18[Seven] <- "Seven"
market_combo$Householdu18[Eight] <- "Eight"
market_combo$Householdu18[Nine_or_more] <- "Nine_or_more"
market_combo$Householdu18 <- as.factor(market_combo$Householdu18)

unique(market_combo$Householdu18)

###	Status###

# HOUSEHOLDER STATUS
#              1. Own
#              2. Rent
#              3. Live with Parents/Family

Own <- which(market_combo$Status == 1)
Rent <- which(market_combo$Status == 2)
with_Family <- which(market_combo$Status == 3)

market_combo$Status[Own] <- "Own"
market_combo$Status[Rent] <- "Rent"
market_combo$Status[with_Family] <- "with_Family"
market_combo$Status <- as.factor(market_combo$Status)

###	Home_Type	###

# TYPE OF HOME
#              1. House
#              2. Condominium
#              3. Apartment
#              4. Mobile Home
#              5. Other

House <- which(market_combo$Home_Type == 1)
Condominium <- which(market_combo$Home_Type == 2)
Apartment <- which(market_combo$Home_Type == 3)
Mobile_Home <- which(market_combo$Home_Type == 4)
Other <- which(market_combo$Home_Type == 5)



market_combo$Home_Type[House] <- "House"
market_combo$Home_Type[Condominium] <- "Condominium"
market_combo$Home_Type[Apartment] <- "Apartment"
market_combo$Home_Type[Mobile_Home] <- "Mobile_Home"
market_combo$Home_Type[Other] <- "Other"
market_combo$Home_Type <- as.factor(market_combo$Home_Type)

### Ethnic ###

# ETHNIC CLASSIFICATION
#              1. American Indian
#              2. Asian
#              3. Black
#              4. East Indian
#              5. Hispanic
#              6. Pacific Islander
#              7. White
#              8. Other

Am_Indian <- which(market_combo$Ethnic == 1)
Asian <- which(market_combo$Ethnic == 2)
Black <- which(market_combo$Ethnic == 3)
EastIndian <- which(market_combo$Ethnic == 4)
Hispanic <- which(market_combo$Ethnic == 5)
PacificIslander <- which(market_combo$Ethnic == 6)
White <- which(market_combo$Ethnic == 7)
Other <- which(market_combo$Ethnic == 8)



market_combo$Ethnic[Am_Indian] <- "Am_Indian"
market_combo$Ethnic[Asian] <- "Asian"
market_combo$Ethnic[Black] <- "Black"
market_combo$Ethnic[EastIndian] <- "EastIndian"
market_combo$Ethnic[Hispanic] <- "Hispanic"
market_combo$Ethnic[PacificIslander] <- "PacificIslander"
market_combo$Ethnic[White] <- "White"
market_combo$Ethnic[Other] <- "Other"
market_combo$Ethnic <- as.factor(market_combo$Ethnic)

###	Language ###

# WHAT LANGUAGE IS SPOKEN MOST OFTEN IN YOUR HOME?
#              1. English
#              2. Spanish
#              3. Other


English <- which(market_combo$Language == 1)
Spanish <- which(market_combo$Language == 2)
Other <- which(market_combo$Language == 3)

market_combo$Language[English] <- "English"
market_combo$Language[Spanish] <- "Spanish"
market_combo$Language[Other] <- "Other"
market_combo$Language <- as.factor(market_combo$Language)
market_combo$YY <- as.factor(market_combo$YY)

head(market_combo)

summary(market_combo)

### Step 4 - Fit a classification tree ####
model.control <- rpart.control(minsplit = 5, xval = 5)
model = rpart(YY~., method="class", data= market_combo, control =model.control)
summary(model)

printcp(model) # display the results

options(repr.plot.width=4, repr.plot.height=4, repr.plot.res = 200)
plotcp(model) # visualize cross-validation results

options(repr.plot.width=4, repr.plot.height=4, repr.plot.res = 200)
plot(model$cptable[,4], main = "Cp for model selection", ylab = "cv error", type='l', ylim=c(0.1,1.2))
points(model$cptable[,4], col="red", cex=1,pch=20)

min_cp = which.min(model$cptable[,4])

#### variable importance plot from rpart #####

varimp_rpart<- data.frame( imp= model$variable.importance)
varimp_rpart_plot <- varimp_rpart %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange("Importance"=imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

options(repr.plot.width=5, repr.plot.height=5, repr.plot.res = 200)

ggplot2::ggplot(varimp_rpart_plot) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() + labs(title = "Variable Importance from rpart tree")
  theme_bw()

# plot tree
options(repr.plot.width=8, repr.plot.height=8, repr.plot.res = 200)
plot(model, uniform=TRUE,
     main="Classification Tree for Marketing Dataset")
text(model,  cex=1.2)   

# text(model,  use.n=TRUE, all=TRUE, cex=1.2)

options(repr.plot.width=8, repr.plot.height=8, repr.plot.res = 200)
# Visualize the decision tree with rpart.plot
rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE, extra = 'auto', cex=0.7)

rparty_tree <- as.party(model)
rparty_tree

options(repr.plot.width=20, repr.plot.height=7, repr.plot.res = 200)
plot(rparty_tree)
# 
# prune_rpart<- prune(model, cp=model$cptable[min_cp, 1])
#  # pruning the tree
# options(repr.plot.width=6, repr.plot.height=6, repr.plot.res = 200)
# plot(prune_rpart, uniform=TRUE, branch=0.6)
# text(prune_rpart, all=TRUE, use.n=TRUE)
# 
# options(repr.plot.width=8, repr.plot.height=8, repr.plot.res = 200)
# # Visualize the decision tree with rpart.plot
# rpart.plot(prune_rpart, box.palette="RdBu", shadow.col="gray", nn=TRUE, extra = 'auto', cex=0.7)

### end ###
