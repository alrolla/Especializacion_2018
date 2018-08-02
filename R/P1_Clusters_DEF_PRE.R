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


#========== FUNCIONES ===================
  dendrograma <- function(hc, ng ){
    dendr  = dendro_data(hc, type="rectangle") # convert for ggplot
    clust    <- cutree(hc,k=ng)                    # find 2 clusters
    clust.df <- data.frame(label=names(clust), cluster=factor(clust))
    # dendr[["labels"]] has the labels, merge with clust.df based on label column
    dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
    # plot the dendrogram; note use of color=cluster in geom_text(...)
    g.dendrograma=ggplot() + 
      geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
      geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
                size=1.8) +
      coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
      theme(axis.line.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.text=element_text(size=5),
            axis.title.x = element_text(size=9,color='black',face="plain"),
            panel.background=element_rect(fill="lightyellow"),
            legend.position="none",
            panel.grid=element_blank())
    return(g.dendrograma)
  }


mapa <- function(hc, ng ,CodEst,sa.shape.df,prov.shape.df){
  cluster=factor(cutree(hc, k =ng, h = NULL))
  CodEst.g=cbind(CodEst,cluster)
  
  g.mapa = ggplot() +
    labs(x="Longitud", y = "Latitud")+
    theme(plot.title = element_text(hjust = 0.5),
          title =element_text(size=12, face='bold'),
          axis.title.x = element_text(size=9,color='black',face="plain"),
          axis.title.y = element_text(size=9,color='black',face="plain"),
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          axis.text=element_text(size=5),
          panel.border = element_rect(colour = "black", fill=NA, size=1))+
    geom_polygon(data = sa.shape.df, aes(x = long, y = lat, group = group),color = 'brown', fill = NA, size = 1,alpha=1) +
    geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .2,alpha=1) +
    geom_point(data=CodEst.g, aes(x=Longitud, y=Latitud,colour = cluster),size=2)+
    coord_map(xlim = c(-68, -55),ylim = c(-40,-28))
  return(g.mapa)
}

#========== FUNCIONES ===================


setwd("~/Dropbox/TesisEspecializacion/programas")

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


sw=1
for ( cod in CodEst[,"idOMM"] ){
  
  sql=paste0("SELECT  idOMM,YEAR,DEF FROM SMN_INTA_MON_DATA_ARG where YEAR >= 1979 and idOMM= ",cod)
  rs <- dbSendQuery(con, sql)
  DataEst <- fetch(rs,n=-1)
  DEF.x=t(DataEst[,"DEF"])
  rownames(DEF.x)=cod
  if(sw==1){
    DEF=DEF.x
    colnames(DEF)<-seq(1979,2017)
    sw=2
  }else{
    DEF=rbind(DEF,DEF.x)
  }
}

#=== EUCLIDEA ==========================================================

dist.e <- dist(DEF,method = "euclidean")

hc.e.s=hclust(dist.e,method="single")
hc.e.c=hclust(dist.e,method="complete")
hc.e.a=hclust(dist.e,method="average")
hc.e.w=hclust(dist.e,method="ward.D")

#=== CORRELACION ==========================================================
dist.c=as.dist(1-cor(t(DEF)))

hc.c.s=hclust(dist.c,method="single")
hc.c.c=hclust(dist.c,method="complete")
hc.c.a=hclust(dist.c,method="average")
hc.c.w=hclust(dist.c,method="ward.D")


ng=5
for (ng in 3:8){
              #=== EUCLIDEA ==========================================================
  
              dendro.e.s=dendrograma(hc.e.s, ng ) # dendrograma euclidea, single
              mapa.e.s=mapa(hc.e.s, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              
              print(paste0("C.Cofenetica hc.e.s: ",round(cor(cophenetic(hc.e.s), dist.e),3)))
              
              dendro.e.c=dendrograma(hc.e.c, ng ) # dendrograma euclidea, complete
              mapa.e.c=mapa(hc.e.c, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.e.c: ",round(cor(cophenetic(hc.e.c), dist.e),3)))
              
              
              dendro.e.a=dendrograma(hc.e.a, ng ) # dendrograma euclidea, average
              mapa.e.a=mapa(hc.e.a, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.e.a: ",round(cor(cophenetic(hc.e.a), dist.e),3)))
              
              dendro.e.w=dendrograma(hc.e.w, ng ) # dendrograma euclidea, ward (varianza)
              mapa.e.w=mapa(hc.e.w, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, wards
              print(paste0("C.Cofenetica hc.e.w: ",round(cor(cophenetic(hc.e.w), dist.e),3)))
              
              g=arrangeGrob(dendro.e.s, mapa.e.s,dendro.e.c,mapa.e.s,dendro.e.a,mapa.e.a,dendro.e.w,mapa.e.w,ncol=2)
              
              ggsave(file=paste0("graficos/Grupos_D_Euclidea_",ng,".pdf"), g,width = 8,height=12 )
              
              #=== CORRELACION ==========================================================
              
              dendro.c.s=dendrograma(hc.c.s, ng ) # dendrograma correlacion, single
              mapa.c.s=mapa(hc.c.s, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.c.s: ",round(cor(cophenetic(hc.c.s), dist.c),3)))
              
              dendro.c.c=dendrograma(hc.c.c, ng ) # dendrograma correlacion, complete
              mapa.c.c=mapa(hc.c.c, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.c.c: ",round(cor(cophenetic(hc.c.c), dist.c),3)))
              
              dendro.c.a=dendrograma(hc.c.a, ng ) # dendrograma correlacion, average
              mapa.c.a=mapa(hc.c.a, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.c.a: ",round(cor(cophenetic(hc.c.a), dist.c),3)))
              
              dendro.c.w=dendrograma(hc.c.w, ng ) # dendrograma correlacion, ward (varianza)
              mapa.c.w=mapa(hc.c.w, ng, CodEst,sa.shape.df,prov.shape.df) # dendrograma euclidea, ward
              print(paste0("C.Cofenetica hc.c.w: ",round(cor(cophenetic(hc.c.w), dist.c),3)))
              
              g=arrangeGrob(dendro.c.s, mapa.c.s,dendro.c.c,mapa.c.s,dendro.c.a,mapa.c.a,dendro.c.w,mapa.c.w,ncol=2)
              
              ggsave(file=paste0("graficos/Grupos_D_Pearson_",ng,".pdf"), g,width = 8,height=12 )
              

}

#K-MEANS ======================================================================

#Genero los clusters para distintos K usando la matriz de distancias euclidea ( porque anduvo mejor)
#Lo mejor fue seguir con 6 
nc=6
g.bestKM=fviz_nbclust(DEF, kmeans, method = "wss", diss = dist.e, k.max = 10) +
  labs(title = "KMEANS - Cantidad óptima de clusters") + 
  xlab("Número de clusters (K)") + 
  ylab("WSS(total within sum of square)") +
  theme_bw() +
  theme(plot.title = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))+
        expand_limits(x = 0, y = 0)+
        geom_vline(xintercept = nc, linetype = 2)

ggsave(plot=g.bestKM,"graficos/KMEANS_NumerodeClustersOptima_WSS.pdf", width = 6, height = 4)

#KMEANS ===============
KM <- kmeans(x = DEF, centers = nc, nstart=20)
#=================================

#PLOTEO de CLUSTERS para ver separabilidad
g.clusters=fviz_cluster(KM, data = DEF, ellipse.type = "convex", stand = TRUE, show.clust.cent = T, ellipse.alpha = 0.1, labelsize = 9,pointsize = 1)+ 
  labs(title = paste0("KMEANS - Ploteo de Clusters (K=",nc,")")) +
  theme_bw() +
  theme(plot.title = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=.5,vjust=.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))+
  expand_limits(x = 0, y = 0) 
ggsave(plot=g.clusters,paste0("graficos/KMEANS_Clusters_K",nc,".pdf"), width = 8, height = 7)

#PLOTEO de SILHOUETTE para ver separabilidad y cohesion de clusters
sil = silhouette(KM$cluster, dist.e)
g.sil=fviz_silhouette(sil.obj = sil,  print.summary = TRUE)+ 
  ylab("Ancho Silhouette") +
  theme_bw() +
  theme(plot.title = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=1,vjust=.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
        scale_y_continuous(expand = c(0, 0),breaks = seq(-0.5,1,0.1),limits = c(-0.5,1))
ggsave(plot=g.sil,paste0("graficos/KMEANS_Silhouette_K",nc,".pdf"), width = 8, height = 7)



cluster=factor(KM$cluster)
CodEst.g=cbind(CodEst,cluster)
g.kmeans = ggplot() +
  labs(title="K-MEANS (Numero de clusters=6)",x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=9,color='black',face="plain"),
        axis.title.y = element_text(size=9,color='black',face="plain"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        axis.text=element_text(size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_polygon(data = sa.shape.df, aes(x = long, y = lat, group = group),color = 'brown', fill = NA, size = 1,alpha=1) +
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .2,alpha=1) +
  geom_point(data=CodEst.g, aes(x=Longitud, y=Latitud,colour = cluster),shape=21,size=2,fill="white",stroke = 1.5)+
  geom_label_repel(data=CodEst,aes(label=CodEst$idOMM,x=Longitud, y=Latitud,colour = cluster),color="black",size = 2.5,label.size = 0.25,box.padding = 0.35, point.padding = 0.1)+
  coord_map(xlim = c(-68, -55),ylim = c(-40,-28))
ggsave(plot=g.kmeans,paste0("graficos/KMEANS_mapa_K",nc,".pdf"), width = 8, height = 7)

#IMPORTANTE GUARDAR LA CLUSTERIZACION!!
save(CodEst.g, file = "variables/clusters.RData")

#Cierro la coneccion a la base de datos!!
dbDisconnect(con)

# #CENTROIDES =======ESTO POR AHORA NO=====================================
# CodEst.g
# CodEst.g[order(CodEst.g$cluster),]
# lon.cent=aggregate(Longitud ~ cluster, CodEst.g, mean)
# lat.cent=aggregate(Latitud ~ cluster, CodEst.g, mean)
# 
# CodCentroide=cbind(lon.cent["cluster"],lon.cent["Longitud"],lat.cent["Latitud"])

  
cluster=factor(KM$cluster)
CodEst.g=cbind(CodEst,cluster)
g.kmeans = ggplot() +
  labs(title="K-MEANS (Numero de clusters=6)",x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=9,color='black',face="plain"),
        axis.title.y = element_text(size=9,color='black',face="plain"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=6),
        axis.text=element_text(size=5),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_polygon(data = sa.shape.df, aes(x = long, y = lat, group = group),color = 'brown', fill = NA, size = 1,alpha=1) +
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .2,alpha=1) +
  geom_point(data=CodEst.g, aes(x=Longitud, y=Latitud,colour = cluster),shape=21,size=2,fill="white",stroke = 1.5)+
  geom_point(data=CodCentroide, aes(x=Longitud, y=Latitud,colour = cluster),shape=21,size=2,fill="white",stroke = 3)+
  geom_label_repel(data=CodEst,aes(label=CodEst$idOMM,x=Longitud, y=Latitud,colour = cluster),color="black",size = 2.5,label.size = 0.25,box.padding = 0.35, point.padding = 0.1)+
  coord_map(xlim = c(-68, -55),ylim = c(-40,-28))


