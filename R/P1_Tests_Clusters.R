library(RMySQL)
library(openxlsx)
library(rgdal)   #para shapefiles
library(ggmap)   #plotea mapas en esto
library(ggplot2) #libreria grafica
library(ggdendro)#dendrogramas
library(plyr)    # manejo de datos
library(dplyr)   #para manejar como BD los dataframe
library(RColorBrewer)
library(gridExtra)
library(NbClust)
library(factoextra)
library(cluster)
library(ggrepel)
library(tidyr)

setwd("~/Dropbox/TesisEspecializacion/programas")


con <- dbConnect(MySQL(), user="root", password="", dbname="TesisEspecializacion", host="localhost")

sql=paste0("SELECT SMN_INTA_META_ARG.idOMM, 
           SMN_INTA_MON_DATA_ARG.`YEAR`, 
           SMN_INTA_MON_DATA_ARG.DEF, 
           SMN_INTA_MON_DATA_ARG.MAM, 
           SMN_INTA_MON_DATA_ARG.JJA, 
           SMN_INTA_MON_DATA_ARG.SON, 
           SMN_INTA_META_ARG.activo
           FROM SMN_INTA_META_ARG INNER JOIN SMN_INTA_MON_DATA_ARG ON SMN_INTA_META_ARG.idOMM = SMN_INTA_MON_DATA_ARG.idOMM 
           WHERE SMN_INTA_META_ARG.activo='*' AND SMN_INTA_MON_DATA_ARG.`YEAR` >= 1979 ")
rs <- dbSendQuery(con, sql)
DataEst <- fetch(rs,n=-1)

dbDisconnect(con)

#Cargo la matriz de grupos

load("variables/clusters.RData")

DataEst$cluster=0
for(cl in 1:length(DataEst[,1])){
  ix=which(CodEst.g$idOMM==DataEst$idOMM[cl])
  print(CodEst.g$cluster[ix])
  DataEst$cluster[cl]=CodEst.g$cluster[ix]
  
}

DataEst_v <- DataEst[,c("cluster","DEF")]

#ANOVA sin consiferar supuesto
res.aov=aov(DEF~factor(cluster), data=DataEst_v)
summary(res.aov)
# Df   Sum Sq Mean Sq F value Pr(>F)    
# factor(cluster)    5  3324587  664917   42.14 <2e-16 ***
#   Residuals       1437 22675735   15780                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

#Aplico una tranformacion porque no son distribuciones normales

library(MASS)
#ploteo el log-likelihood
boxcox(DEF~factor(cluster), data=DataEst_v,plotit=T)
#recupero llas evaluaciones
res.boxcox=boxcox(DEF~factor(cluster), data=DataEst_v)
#recupero la constante de transformacion box-cox
trf=res.boxcox$x[which(max(res.boxcox$y)== res.boxcox$y)]
#recupero la constante de transformacion box-cox
res.aov=aov(DEF^(trf)~factor(cluster), data=DataEst_v)
summary(res.aov)
shapiro.test(residuals(res.aov))
#Son nomales las distribuciones ahora!
# Shapiro-Wilk normality test
# 
# data:  residuals(res.aov)
# W = 0.99897, p-value = 0.5922


#Test de homocedasticidad
library(car)
leveneTest(DataEst_v$DEF^(trf) ,factor(DataEst_v$cluster))
#NO es homocedastica , el test de ANOVA parametrico no funciona!
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group    5  6.7119 3.422e-06 ***
# 1437                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#test no parametrico
kruskal.test(DataEst_v$DEF^(trf),factor(DataEst_v$cluster))
#Los grupos son distintos!!
# Kruskal-Wallis rank sum test
# 
# data:  DataEst_v$DEF^(trf) and factor(DataEst_v$cluster)
# Kruskal-Wallis chi-squared = 178.01, df = 5, p-value < 2.2e-16

#Analisizamos los grupos
library(pgirmess)
kruskalmc(DataEst_v$DEF^(trf),factor(DataEst_v$cluster))
#Resultado:
# Multiple comparison test after Kruskal-Wallis 
# p.value: 0.05 
# Comparisons
# obs.dif critical.dif difference
# 1-2   3.53141    131.38256      FALSE
# 1-3 137.89519    111.65370       TRUE
# 1-4 190.48462    109.24189       TRUE
# 1-5 116.66923    114.68010       TRUE
# 1-6 254.99615    131.38256       TRUE
# 2-3 134.36378    119.93532       TRUE
# 2-4 194.01603    117.69334       TRUE
# 2-5 113.13782    122.75771      FALSE
# 2-6 251.46474    138.48938       TRUE
# 3-4 328.37981     95.16772       TRUE
# 3-5  21.22596    101.36384      FALSE
# 3-6 117.10096    119.93532      FALSE
# 4-5 307.15385     98.70091       TRUE
# 4-6 445.48077    117.69334       TRUE
# 5-6 138.32692    122.75771       TRUE
