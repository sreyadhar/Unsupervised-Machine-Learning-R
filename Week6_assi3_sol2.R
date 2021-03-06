# # -*- coding: utf-8 -*-
# """Week6 Assi3 Sol2.ipynb
# 
# Automatically generated by Colaboratory.
# 
# Original file is located at
#     https://colab.research.google.com/drive/1ewQwyRSZvPfGahiLcEwm1oUjFk3-zrCO
# """

###########################################################################
## Week-6, Homework-3, Sol-2 
## Sreya Dhar 
## Created: Mar 15, 2021
## Edited: Mar 29, 2021
###########################################################################

rm(list=ls())
setwd("C:/File E/EAS 507 Statistical Mining II/Week-6/HW-3")


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
library(ISLR)
library(ggdendro)

########################### (a) ###########################################
dd = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow = 4))

hcl <- hclust(dd, method = "complete")
# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hcl)
# Default plot

plot(hcl, type = "rectangle", ylab = "Height")
plot(hcd, type = "rectangle", ylab = "Height")
# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")

# Change edge color
plot(hcd,  ylab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))


########################### (b) ###########################################

hcl_s <- hclust(dd, method = "single")
# Convert hclust into a dendrogram and plot
hcd_s <- as.dendrogram(hcl_s)
plot(hcl_s, type = "rectangle", ylab = "Height")
# Change edge color
plot(hcd_s,  ylab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))

########################### (e) ###########################################
hcl_ <- hclust(dd, method = "complete")
plot(hcl_,  ylab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1), labels = c(2,1,4,3))

####################################################### end ###################################################################
