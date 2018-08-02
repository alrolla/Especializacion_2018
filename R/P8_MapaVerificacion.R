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

setwd("~/Dropbox/TesisEspecializacion/programas")

prov.shape <- readOGR(dsn="shapes",layer="argentina")
#asigno la proyeccion al shapefile ( porque no la tiene ...)
proj4string(prov.shape) <- CRS("+proj=longlat +datum=WGS84")
prov.shape <- spTransform(prov.shape, CRS("+proj=longlat +datum=WGS84"))
prov.shape.df <- fortify(prov.shape)

sa.shape <- readOGR(dsn="shapes",layer="sa")
#asigno la proyeccion al shapefile ( porque no la tiene ...)
proj4string(sa.shape) <- CRS("+proj=longlat +datum=WGS84")
sa.shape <- spTransform(sa.shape, CRS("+proj=longlat +datum=WGS84"))
sa.shape.df <- fortify(sa.shape)

depto.shape <- readOGR(dsn="shapes",layer="departamentos")
#asigno la proyeccion al shapefile ( porque no la tiene ...)
proj4string(depto.shape) <- CRS("+proj=longlat +datum=WGS84")
depto.shape <- spTransform(depto.shape, CRS("+proj=longlat +datum=WGS84"))
depto.shape.df <- fortify(depto.shape)

con <- dbConnect(MySQL(), user="root", password="", dbname="TesisEspecializacion", host="localhost")
sw=0
for(nclus in 1:6){
    #Leo el predictando
    y.datos=read.xlsx(paste0("Pronostico/Verificacion_2016.xlsx"),sheet = paste0("CLUSTER_",nclus))
    c.esta=paste(shQuote(as.numeric(y.datos$idOMM)), collapse=", ")
    y.datos$Acierto[y.datos$Acierto=="SI"]<-0
    y.datos$Acierto[y.datos$Acierto=="NO"]<-1
    #Leer todos los codigos de estaciones
    sql<-paste0("SELECT idOMM,NomEstacion,Institucion,Longitud,Latitud FROM SMN_INTA_META_ARG where  idOMM in (",c.esta,") AND activo = '*' AND Institucion = 'SMN' order by idOMM")
    rs <- dbSendQuery(con, sql)
    CodEst <- fetch(rs,n=-1)
    CodEst$Acierto=y.datos$Acierto
    CodEst$cluster=nclus
    if(sw == 0){
      verif.df=CodEst
      sw=1
    }else{
      verif.df=rbind(verif.df,CodEst)
    }
}
dbDisconnect(con)

plotx=ggplot() +
  labs(title="Verificación Pronostico \n Año 2016",x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=12,color='black',face="plain"),
        axis.title.y = element_text(size=12,color='black',face="plain"),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        axis.text=element_text(size=8,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = sa.shape.df, aes(x = long, y = lat, group = group),color = 'brown', fill = NA, size = 1,alpha=1) +
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .2,alpha=1) +
  geom_polygon(data = depto.shape.df, aes(x = long, y = lat, group = group),color = '#0000ff66', fill = NA, size = .2,alpha=0.5) +
  coord_cartesian(xlim = c(-68, -55),ylim = c(-40,-28))+
  scale_color_manual(values=c( "#00770088","#ff0000bb"),
                     name="Acierto",
                     labels=c("SI", "NO"))+
  geom_point(data=verif.df, aes(Longitud, Latitud,shape=19,color=Acierto),size = 5,stroke=1.5)+

  geom_point(data=verif.df, aes(Longitud, Latitud,shape=cluster),size = 2,stroke=1.5)+scale_shape_identity()


ggsave("Pronostico/Verificacion2016.pdf",plot = plotx,width = 7.5, height = 7)


