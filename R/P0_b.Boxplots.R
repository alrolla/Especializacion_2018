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

load("variables/clusters.RData")


DataEst$cluster=0

for(cl in 1:length(DataEst[,1])){
  ix=which(CodEst.g$idOMM==DataEst$idOMM[cl])
  print(CodEst.g$cluster[ix])
  DataEst$cluster[cl]=CodEst.g$cluster[ix]
  
}



main=ggplot(data=DataEst,aes(x=reorder(idOMM,cluster), y=DEF, fill=factor(cluster))) + 
  ggtitle("Distribución de datos x estación meteorológica") +
  xlab("Identificacion OMM")+
  ylab("Precipitación acumulada de verano")+
  geom_boxplot() + geom_jitter(size=0.21,width = 0.05)+
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=8, angle=90,vjust = 0.5),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=8, angle=0),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("graficos/BoxPlotxEstacion.pdf",plot = main,width = 8, height = 4)

#print(main)

main=ggplot(data=DataEst,aes(x=reorder(cluster,idOMM), y=DEF, fill=factor(cluster))) + 
  ggtitle("Distribución de datos x Región") +
  xlab("Región")+
  ylab("Precipitación acumulada de verano")+
  geom_boxplot() + geom_jitter(size=0.21,width = 0.05)+
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=8, angle=90),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=8, angle=0),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("graficos/BoxPlotxRegion.pdf",plot = main,width = 8, height = 4)

#ANOVA
DEF.aov=aov(idOMM ~ DEF,data=DataEst)
#RESUMEN
summary(DEF.aov)

#test de normalidad
shapiro.test(residuals(DEF.aov))
#tesst de mohocedasticidad
leveneTest( idOMM ~ DEF,data=DataEst)

dbDisconnect(con)

stop("FIN")
