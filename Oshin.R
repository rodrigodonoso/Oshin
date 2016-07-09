###################################
## Data transportation project
## Proyecto Oshin
## MEM - 3Q 
## May 28th
## Rodrigo Donoso / Rodrigo Oviedo
####################################
#Module 1: Creating data set
#Identification and localization
rm(list=ls()) 
setwd("~/Desktop/Oshin")
INM <- read.csv("~/Desktop/Oshin/TR_INMUEBLES.csv")
CENTROS <- read.csv("~/Desktop/Oshin/TR_CENTROS.csv")
INM_CONAFE <- read.csv("~/Desktop/Oshin/TR_CONAFE.csv")

#Step 1: cleaning and setting the dataset. It will have 5 modules.

library(plyr)
#Overview
str(INM)
str(CENTROS)
str(INM_CONAFE)
table(INM$ENT == INM$ENT_ADMON) #There are 94 (out of 149,613) schools with ENT dif ENT_ADMON

#Choosing columns
Col_INM <- c(1,2,3,4,5,145,8,55,56,57,58,59,60,61,62,63,
             115,116,117,118,119,120,121,122,123,126,129,
             134,135,136,137,138,28,30,32,33,34,35) #General description: ID + Localization + Numb students + Capacity + kind + Educational places + basic services)

Col_CENTROS <- c(1,2,9,10,11,12,50,224)
INM2 <- INM[,Col_INM]
CENTROS2 <- CENTROS[,Col_CENTROS]
colnames(CENTROS2) <- c("ID_INM","CLAVE_CT","Nivel","Modalidad","Turno","Control","N_Alum_inscr","N_Alum_censados") #changing name of column
colnames(INM2) <- c("ID_INM","ENT","MUNIC","LOC","AGEB","N_Alum_capac","Tipo_inm",
                    "N_Aulas","N_Tall","N_comp","N_medios","N_lab","N_encicl","N_ofdir",
                    "N_ofadm","N_dorm","Uso_mult","Bibl","Audit","Audiovis","sala_maest","area_gym","alberc","cancha_dep","jueg_inf",
                    "patio","bodega","cocina","comedor","enfermeria","techo_canch","techo_patio","fuenta_AP",
                    "fuente_ELEC","Cisterna","Letrina","Bano","Drenaje") #changing name of column


#Editing variables1
CENTROS2[,"Control"] <- ifelse(CENTROS2$Control==1, "Publico", "Privado")
Columns3 <- c(17:32)
INM2[,Columns3] <- ifelse(INM2[,Columns3]==1,"1","0")

#join
data <-join(INM2,CENTROS2, type="left", by="ID_INM")
data <- data[,c(1,39,2,3,4,5,44,6,45,40,41,42,43,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,7,33,34,35,36,37,38)]
str(data)
#data = General descriotion ok
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
colnames(tod_fissures) <- c("fiss_aula_porc","fiss_taller_porc","fiss_acomp_porc","fiss_medios_porc","fiss_lab_porc","fiss_encicl_porc","fiss_ofdir_porc","fiss_ofadm_porc","fiss_dorm_porc") #changing name of columns
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
colnames(tod_elect) <- c("elect_aula","elect_taller","elect_acomp","elect_medios","elect_lab","elect_encicl","elect_ofdir","elect_ofadm","elect_dorm") #changing name of columns
todos_recintos <- rowSums(Totals,na.rm=T) #Sum todos los recintos por escuela
todos_fissures <- rowSums(Fissures,na.rm=T)
todos_elect_porc <- todos_fissures/todos_recintos #One column with total damage % (sum of all educ spaces)

#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 3: Physical condition - Basic services spaces: fissures, leaks, glass, door, electricity.
#Choosing columns
basics <- INM[,c(38,41,42,43,44,45,46,47,48,49,50,51,52,53,54)] #total, fissures, leaks, glass, door, electricity, no water
basics[basics == 999] <- NA 
#Damage in bathrooms
basics$bath_fiss_porc <- basics$P28/ basics$P25
basics$bath_leaks_porc <- basics$P29/ basics$P25
basics$bath_glass_porc <- basics$P30/ basics$P25
basics$bath_door_porc <- basics$P31/ basics$P25
basics$bath_elect_porc <- basics$P32/ basics$P25
basics$bath_no_water_porc <- basics$P33/ basics$P25
#Damage in tazas
basics$tazas_desperf_porc <- basics$P35/basics$P34
#Damage in mingi
basics$mingi_desperf_porc <- basics$P37/basics$P36
#Damage in lavam
basics$lavam_desperf_porc <- basics$P39/basics$P38
#Damage in bebed
basics$bebed_desperf_porc <- basics$P41/basics$P40

basics2 <- basics[,c(16:25)]
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 4: General description: Cero Perimetral + Proteccion Civil
#Choosing columns
Gen_descr <- INM[,c(22,23,25,26,27)] #General description: Cerco perimetral (completo, incom), mat cerco, mat paredes, mat techo, mat piso.
Prot_civil <- INM[,c(146,147,148,149,150,155)] #Extintor, señaletica, rutas evac, salid emer, zona seg, albergue.
Gen_descr[Gen_descr == 9] <- NA
Prot_civil[Prot_civil == 9] <- NA 
colnames(Gen_descr) <- c("Cerco_per","Mat_cerco","Mat_paredes","Mat_techo","Mat_piso") #changing name of columns
colnames(Prot_civil) <- c("Ext","Senalet","rutas_evac","salida_emerg","zona_seg","albergue") #changing name of columns

#Editing variables 2
Columns4 <- c(1:6)
Prot_civil[,Columns4] <- ifelse(Prot_civil[,Columns4]==1,"1","0")

#gathering damages_Module2
ID_INM <- INM2[,1]
test <- cbind(ID_INM,todos_recintos,todos_fiss_porc,todos_leaks_porc,
              todos_glass_porc,todos_door_porc,todos_elect_porc,tod_fissures,tod_leaks,tod_glass,tod_door,tod_elect,basics2,Gen_descr,Prot_civil)
data2 <-join(data,test, type="left", by="ID_INM")
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Module 5: Equipamiento
Columns_2 <- c(102,105,108,117,120,123,124,163,171,172) #Pizarrones, alum sin silla, apoyarse, escr maestro, silla maestro, proyectores, enciclomedias,internet, compus, compus q sirvemn
Equip <- CENTROS[,Columns_2]
Equip[Equip == 99999] <- NA 
colnames(Equip) <- c("Pizarr_falta","N_alum_s/silla","N_alum_s/mesa","N__mesa_maest_falta","N_silla_maest_falta",
                     "proy_malos","encicl_malos","Internety/n","N_PC_falta","N_PC_buenos")

data3 <-cbind(data2,Equip)
write.csv(data3,file="dataset.csv") #Final dataset to elaborate the model

#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) 
setwd("~/Desktop/Oshin")
data <- read.csv("~/Desktop/Oshin/dataset.csv")

#Model: Analisis in three levels by [region / municipality] / School
#1. General Diagnosis: Antes de priorizar, mostrar cómo están las escuelas y su situación a nivel país / regiones.
  #capacity vs students
  #Nivel, modalidad, turno, control
  #Tipo inm
  #Mat paredes, techo y piso.
  #Fuente AP, elect, Sewage
  #Level of damage todos: fissures, leaks, glass, door, elect, 
  #Level of damage bath: fissures, leaks, glass, door, elect, no water
  #Equipamiento: Pizarra, silla, mesa (alum y prof), proyect, internet y pc.
  
#2. Ranking of priorities: 
  #Priorización por region: "distribute a bunch of money"
    #Indice de pobreza = Situacion histórica de la escuela = Tipo inm / AP / Elect / Sewage / Mat techo / Mat paredes / Mat Piso /Cerco (interesante mirar clusters)
    #Nivel de daño: (1) restrictivo funcionamiento y (2) no restrictivo funcionamiento    
    #Tamaño: num alumnos y? 
    #%capaidad utilizada y ojalá matrícula histórica
    #Control: público / privado
    #Equipamiento
  
rm(list=ls()) 
data <- read.csv("dataset.csv")

# INDICE PROBREZA
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

# Points system to determine "poverty index"
pointsIP<-data[,c(1:6,39:46,107:111)]

pointsIP$Tipo_inm<-as.numeric(pointsIP$Tipo_inm)
pointsIP$fuenta_AP<-as.numeric(pointsIP$fuenta_AP)
pointsIP$fuente_ELEC<-as.numeric(pointsIP$fuente_ELEC)
pointsIP$Cisterna<-as.numeric(pointsIP$Cisterna)
pointsIP$Letrina<-as.numeric(pointsIP$Letrina)
pointsIP$Bano<-as.numeric(pointsIP$Bano)
pointsIP$Cerco_per<-as.numeric(pointsIP$Cerco_per)
pointsIP$Mat_cerco<-as.numeric(pointsIP$Mat_cerco)
pointsIP$Mat_paredes<-as.numeric(pointsIP$Mat_paredes)
pointsIP$Mat_techo<-as.numeric(pointsIP$Mat_techo)
pointsIP$Mat_piso<-as.numeric(pointsIP$Mat_piso)

# Points according to type of construction
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==1||pointsIP$Tipo_inm==6||pointsIP$Tipo_inm==7||pointsIP$Tipo_inm==8]<-10
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==2]<-8
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==3]<-6
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==4]<-3
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==5]<-0
pointsIP$Tipo_inm2[pointsIP$Tipo_inm==9]<-NA

# Points according to source of drinkable water
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==1]<-10
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==2]<-8
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==3]<-6
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==4]<-3
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==5]<-NA 
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==6]<-0
pointsIP$fuenta_AP2[pointsIP$fuenta_AP==9]<-NA

# Points according to source of electricity
pointsIP$fuente_ELEC2[pointsIP$fuente_ELEC==1||pointsIP$fuente_ELEC==2]<-5
pointsIP$fuente_ELEC2[pointsIP$fuente_ELEC==3]<-3
pointsIP$fuente_ELEC2[pointsIP$fuente_ELEC==4]<-NA 
pointsIP$fuente_ELEC2[pointsIP$fuente_ELEC==5]<-0
pointsIP$fuente_ELEC2[pointsIP$fuente_ELEC==9]<-NA

# Points according to existence of water tower
pointsIP$Cisterna2[pointsIP$Cisterna==1]<-5
pointsIP$Cisterna2[pointsIP$Cisterna==2]<-0
pointsIP$Cisterna2[pointsIP$Cisterna==9]<-NA

# Points according to existence of latrines
pointsIP$Letrina2[pointsIP$Letrina==1]<-0
pointsIP$Letrina2[pointsIP$Letrina==2]<-5
pointsIP$Letrina2[pointsIP$Letrina==9]<-NA

# Points according to existence of bathrooms
pointsIP$Bano2[pointsIP$Bano==1]<-10
pointsIP$Bano2[pointsIP$Bano==2]<-0
pointsIP$Bano2[pointsIP$Bano==9]<-NA

# Points according to existence of drains
pointsIP$Drenaje2[pointsIP$Drenaje==1]<-10
pointsIP$Drenaje2[pointsIP$Drenaje==2]<-0
pointsIP$Drenaje2[pointsIP$Drenaje==9]<-NA

# Points according to existence of "muro perimetral" 
pointsIP$Cerco_per2[pointsIP$Cerco_per==1]<-10
pointsIP$Cerco_per2[pointsIP$Cerco_per==2]<-5
pointsIP$Cerco_per2[pointsIP$Cerco_per==3]<-0
pointsIP$Cerco_per2[pointsIP$Cerco_per==4]<-10
pointsIP$Cerco_per2[pointsIP$Cerco_per==9]<-NA

# Points according to material of "muro perimetral" 
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==1]<-1
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==2]<-1
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==3]<-0.5
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==4]<-0.5
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==5]<-NA
pointsIP$Mat_cerco2[pointsIP$Mat_cerco==9]<-NA

# Points according to material of walls
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==1]<-1.5
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==2]<-0.75
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==3]<-1
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==4]<-0.5
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==5]<-0.75
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==6]<-0.5
pointsIP$Mat_paredes2[pointsIP$Mat_paredes==9]<-0.5
pointsIP$Mat_paredes2[is.na(pointsIP$Mat_paredes)]<-0.5

# Points according to material of ceiling
pointsIP$Mat_techo2[pointsIP$Mat_techo==1]<-1.5
pointsIP$Mat_techo2[pointsIP$Mat_techo==2]<-1
pointsIP$Mat_techo2[pointsIP$Mat_techo==3]<-0.75
pointsIP$Mat_techo2[pointsIP$Mat_techo==4]<-0.5
pointsIP$Mat_techo2[pointsIP$Mat_techo==5]<-0.75
pointsIP$Mat_techo2[pointsIP$Mat_techo==6]<-0.5
pointsIP$Mat_techo2[pointsIP$Mat_techo==9]<-0.5
pointsIP$Mat_techo2[is.na(pointsIP$Mat_techo)]<-0.5

# Points according to material of floor
pointsIP$Mat_piso2[pointsIP$Mat_piso==1]<-1
pointsIP$Mat_piso2[pointsIP$Mat_piso==2]<-1.2
pointsIP$Mat_piso2[pointsIP$Mat_piso==3]<-0.75
pointsIP$Mat_piso2[pointsIP$Mat_piso==9]<-0.5
pointsIP$Mat_piso2[is.na(pointsIP$Mat_piso)]<-0.5

pointsIP[is.na(pointsIP)]<-0

pointsIP$Ind_Pobreza<-pointsIP$Mat_piso2*pointsIP$Mat_techo2*pointsIP$Mat_paredes2*(pointsIP$Mat_cerco2*pointsIP$Cerco_per2+
pointsIP$Drenaje2+pointsIP$Bano2+pointsIP$Letrina2+pointsIP$Cisterna2+pointsIP$fuente_ELEC2+pointsIP$fuenta_AP2+pointsIP$Tipo_inm2)
                                                                                   
maxIP<-max(pointsIP$Ind_Pobreza)
step<-maxIP/5

pointsIP$Cat_Pobreza[pointsIP$Ind_Pobreza>=0 & pointsIP$Ind_Pobreza<=step]<-"pobreza extrema"
pointsIP$Cat_Pobreza[pointsIP$Ind_Pobreza>step & pointsIP$Ind_Pobreza<=2*step]<-"pobreza moderada"
pointsIP$Cat_Pobreza[pointsIP$Ind_Pobreza>2*step & pointsIP$Ind_Pobreza<=3*step]<-"pobreza moderada"
pointsIP$Cat_Pobreza[pointsIP$Ind_Pobreza>3*step & pointsIP$Ind_Pobreza<=4*step]<-"pobreza leve"
pointsIP$Cat_Pobreza[pointsIP$Ind_Pobreza>4*step & pointsIP$Ind_Pobreza<=maxIP]<-"sin pobreza"

porcentajes<-vector()

porcentajes$Porc_Extrema<-length(pointsIP$Cat_Pobreza[pointsIP$Cat_Pobreza=="pobreza extrema"])/length(pointsIP$Cat_Pobreza)*100
porcentajes$Porc_Moderada<-length(pointsIP$Cat_Pobreza[pointsIP$Cat_Pobreza=="pobreza moderada"])/length(pointsIP$Cat_Pobreza)*100
porcentajes$Porc_Leve<-length(pointsIP$Cat_Pobreza[pointsIP$Cat_Pobreza=="pobreza leve"])/length(pointsIP$Cat_Pobreza)*100
porcentajes$Porc_SinPob<-length(pointsIP$Cat_Pobreza[pointsIP$Cat_Pobreza=="sin pobreza"])/length(pointsIP$Cat_Pobreza)*100

porcentajes<-as.data.frame(porcentajes)

write.csv(pointsIP,file="pointsIP.csv")
  
rm(list=ls()) 
data <- read.csv("dataset.csv")

# NIVEL DAÑO
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

# Points system to determine "poverty index"
dano<-data[,c(1:9,14:22,47:51,52:106)]

sumaleve<-c(34:38,43:47,52:56,71:73,78)
sumamedio<-c(24,30:31,60,68:69,76)
sumagrave<-c(32:33,39:42,48:51,57:59,66:67,70,74:75,77)

danoleve<-dano[,sumaleve]
danomedio<-dano[,sumamedio]
danograve<-dano[,sumagrave]

for (i in 1:dim(danoleve)[2])
{
  danoleve[,i][danoleve[,i]<=0.2]<-1
  danoleve[,i][danoleve[,i]>0.2 & danoleve[,i]<=0.4]<-2
  danoleve[,i][danoleve[,i]>0.4 & danoleve[,i]<=0.6]<-3
  danoleve[,i][danoleve[,i]>0.6 & danoleve[,i]<=0.8]<-4
  danoleve[,i][danoleve[,i]>0.8 & danoleve[,i]<=1.0]<-5
}

for (i in 1:dim(danomedio)[2])
{
  danomedio[,i][danomedio[,i]<=0.2]<-2
  danomedio[,i][danomedio[,i]>0.2 & danomedio[,i]<=0.4]<-4
  danomedio[,i][danomedio[,i]>0.4 & danomedio[,i]<=0.6]<-6
  danomedio[,i][danomedio[,i]>0.6 & danomedio[,i]<=0.8]<-8
  danomedio[,i][danomedio[,i]>0.8 & danomedio[,i]<=1.0]<-10
}

for (i in 1:dim(danograve)[2])
{
  danograve[,i][danograve[,i]<=0.2]<-3
  danograve[,i][danograve[,i]>0.2 & danograve[,i]<=0.4]<-6
  danograve[,i][danograve[,i]>0.4 & danograve[,i]<=0.6]<-9
  danograve[,i][danograve[,i]>0.6 & danograve[,i]<=0.8]<-12
  danograve[,i][danograve[,i]>0.8 & danograve[,i]<=1.0]<-15
}

fisurastotal<-rowSums(cbind(danograve[,c(1)],danomedio[,c(1:3,6)],danoleve[,c(1,5)]),na.rm=TRUE)
leakstotal<-rowSums(cbind(danograve[,c(2:5,17)],danoleve[,c(1,5)]),na.rm=TRUE)
glasstotal<-rowSums(cbind(danograve[,c(6:9)],danoleve[,c(6:10,16)]),na.rm=TRUE)
doortotal<-rowSums(cbind(danograve[,c(10:13)],danoleve[,c(11:15,17)]),na.rm=TRUE)
electtotal<-rowSums(cbind(danograve[,c(14:15)],danomedio[,c(4:5)],danoleve[,c(18)]),na.rm=TRUE)

fisurastotal<-as.data.frame(fisurastotal)
leakstotal<-as.data.frame(leakstotal)
glasstotal<-as.data.frame(glasstotal)
doortotal<-as.data.frame(doortotal)
electtotal<-as.data.frame(electtotal)

# pondano<-cbind(dano$todos_fiss_porc,dano$todos_leaks_porc,dano$todos_glass_porc,
#                  dano$todos_door_porc,dano$todos_elect_porc)
# 
# pondanomedio<-c(1,4:5)
# pondanograve<-c(2:3)
# 
# for (i in 1:length(pondanomedio))
# {
#   j<-pondanomedio[i]
#   pondano[,j][pondano[,j]<=0.2]<-0.4
#   pondano[,j][pondano[,j]>0.2 & pondano[,j]<=0.4]<-0.8
#   pondano[,j][pondano[,j]>0.4 & pondano[,j]<=0.6]<-1.2
#   pondano[,j][pondano[,j]>0.6 & pondano[,j]<=0.8]<-1.6
#   pondano[,j][pondano[,j]>0.8 & pondano[,j]<=1.0]<-2.0
# }
# 
# for (i in 1:length(pondanograve))
# {
#   j<-pondanograve[i]
#   pondano[,j][pondano[,j]<=0.2]<-0.4
#   pondano[,j][pondano[,j]>0.2 & pondano[,j]<=0.4]<-0.8
#   pondano[,j][pondano[,j]>0.4 & pondano[,j]<=0.6]<-1.2
#   pondano[,j][pondano[,j]>0.6 & pondano[,j]<=0.8]<-1.6
#   pondano[,j][pondano[,j]>0.8 & pondano[,j]<=1.0]<-2.0
# }


pondutil<-dano[,c(7:8)]
pondutil$Utilizacion<-pondutil$N_Alum_inscr/pondutil$N_Alum_capac
pondutil$Ponderador[pondutil$Utilizacion<=0.2]<-0.25
pondutil$Ponderador[pondutil$Utilizacion>0.2 & pondutil$Utilizacion<=0.4]<-0.50
pondutil$Ponderador[pondutil$Utilizacion>0.4 & pondutil$Utilizacion<=0.6]<-0.75
pondutil$Ponderador[pondutil$Utilizacion>0.6 & pondutil$Utilizacion<=0.8]<-1.0
pondutil$Ponderador[pondutil$Utilizacion>0.8 & pondutil$Utilizacion<=1.0]<-1.25
pondutil$Ponderador[pondutil$Utilizacion>1.0]<-1.5
pondutil$Ponderador[is.na(pondutil$Utilizacion)]<-1.0 #Si no hay informacion de N° alumnos, se asume utilizacion "normal"

  
  #Priorizacion por escuela: "Qué queremos solucionar primero?" = (1) Daño restrictivo func, (2)Situc hist, (3) Daño no rest, (4) Equipamiento
    #Opciónes: Pjte y/o cualitativo 4-5 niveles urgencia infra (sin problema, urgente, mayor, moderado, leve)
      #Daño restrictivo: aulas, bath, lavamanos
      #Situac historica: Tipo inm / AP / Elect / Sewage / Mat techo / Mat paredes / Mat Piso /Cerco
      #Daño no restrictivo: 
      #Carencia Equipamiento: Pizarra, silla, mesa (alum y prof), proyect, internet y pc.
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#Level of damage:
#(1) Daño restrictivo-variables: [todos fiss % + (fiss + elect)aula%  + (fiss + elect + water)bath%]
  #Si todos fiss % > 50% = grave
  #Si todos fiss % > 30% = mayor
  #Si todos fiss % > 15% = moderado  
  #Si todos fiss % < 15% = leve (may 0)
  #Si todos fiss % = 0% = sin daño
  #Si todos fiss % < 50%
    #Si Prom pond 70% - 30% (fiss + elect)aula% > 50% ó Si Avg. (fiss + elect + water)bath% > 50% = grave
    #Si Prom pond 70% - 30%(fiss + elect)% > 30% ó Si PP 50%-30%-20% (fiss + elect + water)% > 30% = mayor (del resto)
    #Si Prom pond 70% - 30%(fiss + elect)% > 15% ó Si PP 50%-30%-20% (fiss + elect + water)% > 15% = moderado (del resto)
    #Si Prom pond 70% - 30%(fiss + elect)% < 15% ó Si PP 50%-30%-20% (fiss + elect + water)% < 15% = leve (may 0)

#Level 1
pond1 <- 0.7
pond2 <- 1-pond1
pond3 <- 1/3
data$dano_rest[data$fiss_aula_porc*pond1+data$elect_aula*pond2 >.6 | data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 >.6] <- "grave"
data$dano_rest[data$fiss_aula_porc*pond1+data$elect_aula*pond2 <.6 &
                 data$fiss_aula_porc*pond1+data$elect_aula*pond2 >.3 | 
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 <.6 &
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 >.3] <- "mayor"
data$dano_rest[data$fiss_aula_porc*pond1+data$elect_aula*pond2 <.3 &
                 data$fiss_aula_porc*pond1+data$elect_aula*pond2 >.15 | 
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 <.6 &
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 >.3] <- "moderado"
data$dano_rest[data$fiss_aula_porc*pond1+data$elect_aula*pond2 <.15 &
                 data$fiss_aula_porc*pond1+data$elect_aula*pond2 >0 | 
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 <.6 &
                 data$bath_fiss_porc*pond3+data$bath_elect_porc*pond3+data$bath_no_water_porc*pond3 >.3] <- "leve"

head(data$dano_rest, n=30)
table(data$dano_rest)

#Level 2
data$dano_rest[data$todos_fiss_porc >.6] <- "grave"
data$dano_rest[data$todos_fiss_porc >.3 & data$todos_fiss_porc <.6] <- "mayor"
data$dano_rest[data$todos_fiss_porc >.15 & data$todos_fiss_porc <.3] <- "moderado"
data$dano_rest[data$todos_fiss_porc >.0 & data$todos_fiss_porc <.15] <- "leve"
data$dano_rest[data$todos_fiss_porc==0] <- "sin_dano"
#data$dano_rest[(data$todos_fiss_porc + data$todos_leaks_porc + data$todos_glass_porc + data$todos_door_porc + data$todos_elect_porc) ==0 ] <- "sin_dano"
head(data$dano_rest, n=30)
table <- table(data$dano_rest, useNA = "always")
pie(table, col=c("black","yellow","red","orange","green","white"))
help(barplot)
?pie
#------------------------------------------------------------------------------------------------------------------------------------------------
#(2) Indice Pobreza-variables: [tipo inm + fuente(AP+Elect+Letrina+Baño+Drenaje) + mat(Paredes,Techo,Piso)]
  #Si tipo inm = 4 (movil) | 5 (sin const)  = 1
  #Si tipo inm2 = 3 (provisorio) = 1
  #Si fuente AP = 3 (pozo) | 2 (Pipa) | 6 (no tiene)  = 1
  #Si fuente Elect = 5 (no tiene)  = 1
  #Si Letrina = 1 (si)  = 1
  #Si baño = 2 (no)  = 1
  #Si drenaje = 2 (no)  = 1
  #Si cerco per = 3 (no tiene) = 1
  #Si Mat paredes = 4 (bambú) | 5 (asbesto carton) | 6 (desecho) = 1
  #Si Mat techo = 5 (asbesto carton) | 6 (desecho) = 1
  #Si Mat piso = 3 (tierra) = 1

# Poverty index 11 columns
Totals3 <- data[,c(40:46,108:112)] 
Totals3$inmueble[Totals3$Tipo_inm==4|Totals3$Tipo_inm==5]<-1
Totals3$inmueble2[Totals3$Tipo_inm==3]<-1
Totals3$AP[Totals3$fuenta_AP==3|Totals3$fuenta_AP==2|Totals3$fuenta_AP==6] <- 1
Totals3$ELEC[Totals3$fuente_ELEC==5]<-1
Totals3$LETR[Totals3$Letrina==1]<-1
Totals3$BANO[Totals3$Bano==2]<-1
Totals3$DREN[Totals3$Drenaje==2]<-1
Totals3$CERCO[Totals3$Cerco_per==3]<-1
Totals3$PARED[Totals3$Mat_paredes==4|Totals3$Mat_paredes==5|Totals3$Mat_paredes==6] <- 1
Totals3$TECHO[Totals3$Mat_techo==5|Totals3$Mat_techo==6]<-1
Totals3$PISO[Totals3$Mat_piso==3]<-1

Totals4 <- Totals3[,c(13:23)]
Totals4$pov_index <- rowSums(Totals4,na.rm=T) #Sum todos los recintos por escuela
hist(Totals4$pov_index)

#goes from the lower to higher category
Totals4$pov_index2[Totals4$pov_index > 0 ] <- "leve"
Totals4$pov_index2[Totals4$pov_index > 3 ] <- "moderado"
Totals4$pov_index2[Totals4$pov_index > 6 ] <- "mayor"
Totals4$pov_index2[Totals4$inmueble == 1 & Totals4$AP == 1] <- "grave"
Totals4$pov_index2[Totals4$pov_index > 9 ] <- "grave"
Totals4$pov_index2 <- replace(Totals4$pov_index2,which(is.na(Totals4$pov_index2)),"sin daño")
Totals4$pov_index2[Totals3$Tipo_inm == 9 | (Totals3$fuenta_AP == 9 | is.na(Totals3$fuenta_AP)) & 
                  (Totals3$Cisterna == 9 | is.na(Totals3$Cisterna)) &
                  (Totals3$Letrina == 9 | is.na(Totals3$Letrina)) &
                  (Totals3$Bano == 9 | is.na(Totals3$Bano)) &
                  (Totals3$Drenaje == 9 | is.na(Totals3$Drenaje)) &
                  (Totals3$Cerco_per == 9 | is.na(Totals3$Cerco_per)) & 
                  (Totals3$Mat_paredes == 9 | is.na(Totals3$Mat_paredes)) &
                  (Totals3$Mat_techo == 9 | is.na(Totals3$Mat_techo)) &
                  (Totals3$Mat_piso == 9 | is.na(Totals3$Mat_piso))] <- "sin info"

table2 <- table(Totals4$pov_index2)
table2
#------------------------------------------------------------------------------------------------------------------------------------------------
#(3) Daño no restrictivo-variables: [todos(leaks + glass + door + elect)% + (leaks + glass + door + tazas + mingi +lavam + bebed)bath%]
Totals5 <- data[,c(49:52,99:101,104:107)] 
n <- 1.5
p1 <- n/11 #todos leaks, todos elect, bath leaks, tazas desp
p2 <- ((1-p1*4))/7
p2
sum(p1*4+p2*7) #test
Totals5$p1 <- p1 
Totals5$p2 <- p2
Totals6 <- Totals5
Totals6[,c(1,4,5,8)] <- Totals6[,c(1,4,5,8)]*Totals6$p1
Totals6[,c(2,3,6,7,9,10,11)] <- Totals6[,c(2,3,6,7,9,10,11)]*Totals6$p2 #Matrix 6 con ponderadores listos
Totals7 <- Totals6[,c(1:11)]
Totals7$dano_NO_rest <- rowSums(Totals7,na.rm=T) #Sum todos los recintos por escuela
boxplot(Totals7$dano_NO_rest)
mean(Totals7$dano_NO_rest)
sd(Totals7$dano_NO_rest)

#Level 1
Totals7$dano_NO_rest2[Totals7$dano_NO_rest >.6] <- "grave"
Totals7$dano_NO_rest2[Totals7$dano_NO_rest >.3 & Totals7$dano_NO_rest <.6] <- "mayor"
Totals7$dano_NO_rest2[Totals7$dano_NO_rest >.15 & Totals7$dano_NO_rest <.3] <- "moderado"
Totals7$dano_NO_rest2[Totals7$dano_NO_rest >.0 & Totals7$dano_NO_rest <.15] <- "leve"
Totals7$dano_NO_rest2[Totals7$dano_NO_rest==0] <- "sin_dano"

table3 <- table(Totals7$dano_NO_rest2)
table3
#------------------------------------------------------------------------------------------------------------------------------------------------
#(4) Equipamiento: 15-19 -- 119-128



