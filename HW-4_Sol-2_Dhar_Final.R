#####################################################
# Author: Sreya Dhar
# Created: 04/18/2021
# Edited:  04/28/2021
# Purpose: HW-4-Sol-2
#####################################################
rm(list = ls())
graphics.off()

## uploading libraries ##
library(ggplot2)
library(PerformanceAnalytics)
library(funModeling)
library(dplyr)
library(tidyverse)
library(tidyr)
library(corrplot)
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(plotrix)

#Loading dataset
titanic = read.csv("C:/File E/EAS 507 Statistical Mining II/Week-11/HW-4/titanic_train.csv")
str(titanic)
names(titanic)

### EDA and Data Preprocessing ### 
data = subset(titanic, select = -c(PassengerId, Ticket, Fare, Cabin) )
colSums(is.na(data))
# reordering the levels for Embarker varibale
data$Embarked=factor(data$Embarked,levels = c("C","Q","S"))
ggplot(data, aes(x=Embarked, fill=Embarked)) +
    geom_density()+
    theme_bw()

f =  which(is.na(data$Embarked))
data$Embarked[c(f)] <- 'C'

# renaming the row names
row.names(data)=data$Name

data = subset(data, select = -c(Name) )

### Data Imputation ###
indic = which(is.na(data$Age))
counts <- table(data$Age)
median_Age = round(median(data$Age[-c(indic)]))
data[["Age"]]=replace(data$Age,c(indic),median_Age)
data[["Age"]]=ifelse(data$Age<18,"Child","Adult")
data$Age=factor(data$Age,levels = c("Child","Adult"))

# converting the numerical variables to categorical ones ###
data$Survived=factor(data$Survived,levels= c(0,1))
data[["Survived"]]=ifelse(data$Survived==0,"Not Survived","Survived")
data$Survived=factor(data$Survived,levels= c("Not Survived","Survived"))
data$Pclass=factor(data$Pclass,levels = c(1:3))
data$SibSp=factor(data$SibSp,levels = c(0:5,8))
data$Parch=factor(data$Parch,levels = c(0:6))

### converted data for BN ###
str(data)
glimpse(data)
status(data)
hist(data)


# 3D Piecharts for EDA
perc_surv=round((table(data$Survived)/nrow(data))*100)
lab_surv=paste(names(table(data$Survived)),"(",perc_surv,"%)")
pie3D(table(data$Survived),labels = lab_surv,main = "Pie Chart: Survival Rate",
    col = c('red','yellow'),cex=1.0, explode=0.1)


per_pcl=round((table(data$Pclass)/nrow(data))*100)
lab_pcl=paste(names(table(data$Pclass)),"(",per_pcl,"%)")
pie3D(table(data$Pclass),labels = lab_pcl,main = "Pie Chart: Passenger Ticket Class",
    col = c('red','yellow','green'),cex=1.0, explode=0.1)

 
per_sex=round((table(data$Sex)/nrow(data))*100)
lab_sex=paste(names(table(data$Sex)),"(",per_sex,"%)")
pie3D(table(data$Sex),labels = lab_sex,main = "Pie Chart: Gender",
    col = c('yellow','green'),cex=1.0, explode=0.1)

 
per_age=round((table(data$Age)/nrow(data))*100)
lab_age=paste(names(table(data$Age)),"(",per_age,"%)")
pie3D(table(data$Age),labels = lab_age,main = "Pie Chart: Age Group of Passengers",
    col = c('red','green'),cex=1.0)

per_SibSp=round((table(data$SibSp)/nrow(data))*100)
lab_SibSp=paste(names(table(data$SibSp)),"(",per_SibSp,"%)")
pie(table(data$SibSp),labels = lab_SibSp,main = "Pie Chart: #of Spouse/children on ship",
    col = rainbow(7),cex=0.7)
# Simple Bar Plot
counts <- table(data$SibSp)
barplot(counts, main="SibSp Distribution",
        xlab="Number of Spouse/sibling on Board")

 
per_par=round((table(data$Parch)/nrow(data))*100)
lab_par=paste(names(table(data$Parch)),"(",per_par,"%)")
pie(table(data$Parch),labels = lab_par,main = "Pie Chart: #of Parents/Children on Ship",
    col = rainbow(7),cex=0.6)

# Simple Bar Plot
counts <- table(data$Parch)
barplot(counts, main="Parch Distribution",
        xlab="Number of Parents/Children on Board")

 
per_emb=round((table(data$Embarked)/nrow(data))*100)
n=c('Cherbourg','Queenstown','Southampton')
lab_emb=paste(n,"(",per_emb,"%)")
pie3D(table(data$Embarked),labels = lab_emb,main = "Pie Chart: Port of Embarkation/Destination",
    col = c('green','yellow','orange'),cex=1.0, explode=0.1 )

### BN :: Prediction of Survival ###
dag = hc(data,score = "k2")
dag
# dag = hc(data)
graphviz.plot(dag,highlight = list(nodes,arcs)) #optimal DAG structure
nodes(dag)
arcs(dag)

bn_cpt_fit= bn.fit(dag,data = data,method = "bayes")
bn_cpt_fit$Survived

### bn compilation : cpt for survival ###
grain_bn = as.grain(bn_cpt_fit)
com_grn = compile(grain_bn)
com_grn = propagate(com_grn)
summary(com_grn)
plot(com_grn)
com_evd_m=setEvidence(com_grn,nodes = c("Pclass","Age","Sex"),states = c("1","Adult","female"))
com_evd_m$cptlist$Survived
com_evd_m=setEvidence(com_grn,nodes = c("Pclass","Age","Sex"),states = c("3","Adult","male"))
com_evd_m$cptlist$Survived

### end ###
