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


con <- dbConnect(MySQL(), user="root", password="", dbname="TesisEspecializacion", host="localhost")

#Leer todos los datos de estaciones
sql<-"SELECT  idOMM,YEAR  ,DEF FROM SMN_INTA_MON_DATA_ARG where YEAR>=1979"
rs <- dbSendQuery(con, sql)
data <- fetch(rs,n=-1)

#Leer todos los codigos de estaciones
sql<-"SELECT idOMM,NomEstacion,Institucion,Longitud,Latitud
FROM SMN_INTA_META_ARG where activo = '*' AND Institucion = 'SMN' order by idOMM"
rs <- dbSendQuery(con, sql)
CodEst <- fetch(rs,n=-1)

sw=0
for ( cod in CodEst[,"idOMM"] ){
  sql=paste0("SELECT  idOMM,min(YEAR) as minYear, max(YEAR) as maxYear,avg(DEF) as DEF,avg(MAM) as MAM,avg(JJA) as JJA,avg(SON) as SON FROM SMN_INTA_MON_DATA_ARG where YEAR >= 1979 and idOMM= ",cod)
  rs <- dbSendQuery(con, sql)
  DataEst <- fetch(rs,n=-1)

  
  EST.x=DataEst[c("DEF","MAM","JJA","SON")]
  rownames(EST.x)=cod
  
  if(sw==0){
    EST=EST.x

    sw=1
  }else{
    EST=rbind(EST,EST.x)
  }
}

basemap=ggplot() +
  labs(title="Climatologias",x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=9,color='black',face="plain"),
        axis.title.y = element_text(size=9,color='black',face="plain"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        axis.text=element_text(size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = sa.shape.df, aes(x = long, y = lat, group = group),color = 'brown', fill = NA, size = 1,alpha=1) +
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .2,alpha=1) +
  #geom_point(data=CodEst.g, aes(x=Longitud, y=Latitud,colour = cluster),shape=21,size=2,fill="white",stroke = 1.5)+
  #geom_label_repel(data=CodEst,aes(label=CodEst$idOMM,x=Longitud, y=Latitud,colour = cluster),color="black",size = 2.5,label.size = 0.25,box.padding = 0.35, point.padding = 0.1)+
  coord_cartesian(xlim = c(-68, -55),ylim = c(-40,-28))
  # xlim(-68, -55) + ylim(-40,-28)

subplot=list()
for ( cod in 1:length(CodEst[,"idOMM"]) ){
  Estacion <-c("def", "mam", "jja", "son")
  Precip <-c(t(EST[cod,]))
  plot_data <-data.frame(Estacion, Precip)
  plot_data$Estacion=factor(plot_data$Estacion, levels = plot_data$Estacion[c(1,2,3,4)])
  subplot[[cod]]=ggplot(plot_data, aes(Estacion, Precip)) +geom_bar(stat = "identity",fill=c("red","blue","blue","blue"),colour="gray",size = 0.01)+
                       theme( 
                         legend.position = "none",
                         panel.background = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.spacing = unit(0,"null"),
                         plot.margin = rep(unit(0,"null"),4),
                         axis.ticks = element_blank(),
                         axis.text.x = element_blank(),
                         axis.line.x=element_line(),
                         axis.line.y=element_line(),
                         axis.text.y = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.ticks.length = unit(0,"null")
                       )
}

                       
plotx=basemap

  for ( cod in 1:length(CodEst[,"idOMM"]) ){
    plotx= plotx + annotation_custom(
    grob = ggplotGrob(subplot[[cod]]),
    xmin = CodEst[cod,"Longitud"], xmax = CodEst[cod,"Longitud"]+.5, ymin = CodEst[cod,"Latitud"], ymax = CodEst[cod,"Latitud"]+.2+0.5*(EST[cod,"DEF"]-min(EST[,"DEF"]))/(max(EST[,"DEF"])-min(EST[,"DEF"]))
    )
  }


Estacion <-c("V", "O", "I", "P")
Precip <-c(t(EST[1,]))
plot_data <-data.frame(Estacion, Precip)
plot_data$Estacion=factor(plot_data$Estacion, levels = plot_data$Estacion[c(1,2,3,4)])
referencia=ggplot(plot_data, aes(Estacion, Precip)) +geom_bar(stat = "identity",fill=c("red","blue","blue","blue"),colour="gray",size = 0.01)+
  theme( 
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0,"null"),
    plot.margin = rep(unit(0,"null"),4),
    axis.ticks = element_blank(),
    axis.line.x=element_line(),
    axis.line.y=element_line(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length = unit(0,"null")
  )

plotx= plotx + annotation_custom(
  grob = ggplotGrob(referencia),
  xmin = -57, xmax = -55, ymin = -40.5, ymax = -38.5
)



ggsave("graficos/CicloMedioAnual.pdf",plot = plotx,width = 5, height = 5.5)

dbDisconnect(con)
