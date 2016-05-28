###################################
## Data transportation project
## Proyecto Oshin
## MEM - 3Q 
## May 28th
## Rodrigo Donoso / Rodrigo Oviedo
####################################
#Module 1: Creating data set
setwd("~/Desktop/Oshin")
INM <- read.csv("~/Desktop/Oshin/TR_INMUEBLES.csv")
CENTROS <- read.csv("~/Desktop/Oshin/TR_CENTROS.csv")
INM_CONAFE <- read.csv("~/Desktop/Oshin/TR_CONAFE.csv")

library(plyr)
#Overview
str(INM)
str(CENTROS)
str(CONAFE)
table(INM$ENT == INM$ENT_ADMON) #There are 94 (out of 149,613) schools with ENT dif ENT_ADMON

#Choosing columns
Col_INM <- c(1,2,3,4,5,145,8,55,56,57,58,59,60,61,62,63,
             115,116,117,118,119,120,121,122,123,126,129,
             134,135,136,137,138,28,30,32,33,34,35) #General description: ID + Localization + Numb students + Capacity + kind + Educational places + basic services)

Col_CENTROS <- c(1,2,9,10,11,12,50,224)

INM2 <- INM[,Col_INM]
CENTROS2 <- CENTROS[,Col_CENTROS]

#join
data <-join(INM2,CENTROS2, type="left", by="ID_INM")
data <- data[,c(1,39,2,3,4,5,44,6,45,40,41,42,43,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,7,33,34,35,36,37,38)]
str(data)
#data = General descriotion ok
#------------------------------------------------------------------------------------------------------------------------------------------------

