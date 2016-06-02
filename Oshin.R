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
#------------------------------------------------------------------------------------------------------------------------------------------------
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

  #Priorizacion por escuela: "Qué queremos solucionar primero?" = (1) Daño restrictivo func, (2)Situc hist, (3) Daño no rest, (4) Equipamiento
    #Opciónes: Pjte y/o cualitativo 4-5 niveles urgencia infra (sin problema, urgente, mayor, moderado, leve)
      #Daño restrictivo: aulas, bath, lavamanos
      #Situac historica: Tipo inm / AP / Elect / Sewage / Mat techo / Mat paredes / Mat Piso /Cerco
      #Daño no restrictivo:
      #Carencia Equipamiento: Pizarra, silla, mesa (alum y prof), proyect, internet y pc.
