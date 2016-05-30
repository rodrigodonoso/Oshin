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
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 2: Physical condition - Educational spaces: fissures, leaks, glass, door, electricity.
#Fissures
columns <- c(55:63,66:74)
fis <- INM[,columns]  
#Step1: 999 a NA
table(fis==999) 
fis[fis == 999] <- NA 
summary(fis) #OJO QUE HAY VARIABLES CON 99 = SOSPECHOSO
table(fis$P42==99) 
table(fis$P45==99) 
table(fis$P50==99) 

#Step 2: Measuring total damages per school
#Specific damage and Total fissured educational spaces
Totals <- fis[,c(1:9)]
Fissures <- fis[,c(10:18)]
tod_fissures <- Fissures/Totals #matrix with specific fissure damages 
colnames(tod_fissures) <- c("fiss_aula","fiss_taller","fiss_acomp","fiss_medios","fiss_lab","fiss_encicl","fiss_ofdir","fiss_ofadm","fiss_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_fiss_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)
#Step 3: Checking results 
head(Fissures)
head(Totals)
head(tod_fissures)
head(todos_fissures)
head(todos_recintos)
head(todos_fiss_porc)
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 2: Physical condition - Infraestructure status
#Leaks
columns <- c(55:63,76:84)
fis <- INM[,columns]  
#Step1: 999 a NA
table(fis==999) 
fis[fis == 999] <- NA 
summary(fis) #OJO QUE HAY VARIABLES CON 99 = SOSPECHOSO

#Step 2: Measuring total damages per school
#Specific damage and Total fissured educational spaces
Totals <- fis[,c(1:9)]
Fissures <- fis[,c(10:18)]
tod_leaks <- Fissures/Totals #matrix with specific damages 
colnames(tod_leaks) <- c("leak_aula","leak_taller","leak_acomp","leak_medios","leak_lab","leak_encicl","leak_ofdir","leak_ofadm","leak_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_leaks_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 2: Physical condition - Infraestructure status
#Broken glass
columns <- c(55:63,86:94)
fis <- INM[,columns]  
#Step1: 999 a NA
table(fis==999) 
fis[fis == 999] <- NA 
summary(fis) #OJO QUE HAY VARIABLES CON 99 = SOSPECHOSO

#Step 2: Measuring total damages per school
#Specific damage and Total fissured educational spaces
Totals <- fis[,c(1:9)]
Fissures <- fis[,c(10:18)]
tod_glass <- Fissures/Totals #matrix with specific damages 
colnames(tod_glass) <- c("glass_aula","glass_taller","glass_acomp","glass_medios","glass_lab","glass_encicl","glass_ofdir","glass_ofadm","glass_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_glass_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 2: Physical condition - Infraestructure status
#No door
columns <- c(55:63,96:104)
fis <- INM[,columns]  
#Step1: 999 a NA
table(fis==999) 
fis[fis == 999] <- NA 
summary(fis) #OJO QUE HAY VARIABLES CON 99 = SOSPECHOSO

#Step 2: Measuring total damages per school
#Specific damage and Total fissured educational spaces
Totals <- fis[,c(1:9)]
Fissures <- fis[,c(10:18)]
tod_door <- Fissures/Totals #matrix with specific damages 
colnames(tod_door) <- c("door_aula","door_taller","door_acomp","door_medios","door_lab","door_encicl","door_ofdir","door_ofadm","door_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_door_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 2: Physical condition - Infraestructure status
#No electricity
columns <- c(55:63,106:114)
fis <- INM[,columns]  
#Step1: 999 a NA
table(fis==999) 
fis[fis == 999] <- NA 
summary(fis) #OJO QUE HAY VARIABLES CON 99 = SOSPECHOSO

#Step 2: Measuring total damages per school
#Specific damage and Total fissured educational spaces
Totals <- fis[,c(1:9)]
Fissures <- fis[,c(10:18)]
tod_elect <- Fissures/Totals #matrix with specific damages 
colnames(tod_door) <- c("elect_aula","elect_taller","elect_acomp","elect_medios","elect_lab","elect_encicl","elect_ofdir","elect_ofadm","elect_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_elect_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 3: Physical condition - Basic services spaces: fissures, leaks, glass, door, electricity.

#TEST1
#TEST2



