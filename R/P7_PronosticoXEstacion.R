library(sp)
library(maptools)
library(openxlsx)
library(raster)
library(ncdf4)
library(fields)
library(fpp)

#Modificar aca para cambiar el directorio de trabajo
setwd("~/Dropbox/TesisEspecializacion/programas")

mesn = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Set","Oct","Nov","Dec","A_year","DEF","MAM","JJA","SON")
meses = c("FebMar","MarApr","AprMay","MayJun","JunJul","JulAug","AugSet","SetOct","OctNov","NovDec","DecJan","JanFeb")
imeses = 1
variables = c("sst","tcw","hgt500","hgt1000","hgt200","u850","v850")
varnom = c("skt","pr_wtr","hgt","hgt","hgt","uwnd","vwnd")
umbrales=list(c(-0.35,0.35),c(-0.35,0.35),c(-0.35,0.35),c(-0.35,0.35),c(-0.35,0.35),c(-0.35,0.35),c(-0.35,0.35))

#Lectura de series medias de los CLUSTERS
datos.obs=read.xlsx("clusters/series.medias.xlsx",sheet="Clusters")
cluster.name=colnames(datos.obs[3:ncol(datos.obs)])

#parametros de referencia

miny <- 1979
maxy <- 2015

mes.referencia=11 #mes de referencia
cant.de.anios=maxy-miny+1

miny.pred <- 2016
maxy.pred <- miny.pred

#dimension de los reanalisis
dimx=144
dimy=73

y.pred=c()
pred=c()
obs=c()
ti=c()
ts=c()
#Leo las series medias de los clusters
cluster.obs=read.xlsx("clusters/series.medias.xlsx",sheet="Clusters")
cluster.name=colnames(cluster.obs[3:ncol(cluster.obs)])

wb <- createWorkbook()
for(nclus in 1:length(cluster.name)){
  #for(nclus in 2:2){
  #dir.create(path = paste0("./Predictores/Cluster",nclus), showWarnings = FALSE)
  #dir.create(path = paste0("./Predictores.Regiones/Cluster",nclus), showWarnings = FALSE)
  
  #Leo las variables del excel de LASSO
  predictores.modelo=read.xlsx(xlsxFile = paste0(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO.xlsx")))
  pred_names=colnames(predictores.modelo)
  #Leo las regiones de los predictores
  sw=0
  for(np in 1:length(pred_names)){
    
    pre_nom=pred_names[np]
    #leo la variable ptos.inn de la region del predictor en cuestion
    load(file=paste0("Predictores.Regiones/Cluster",nclus,"/Predictor_",pre_nom,".Rdata"))
    #leo la matriz del reanalisis para calcular el predictor
    #abro el netcdf y leo las coordenadas y la variable
    nom_var=as.character(strsplit(pre_nom,"_")[[1]][1])
    fh =  nc_open(paste0("../nnr/",nom_var,".nc"))
    lon = ncvar_get(fh,"lon")-180
    lat = rev(ncvar_get(fh,"lat"))
    time = ncvar_get(fh,"time")
    
    #recupero el nombre de la variable del netcdf
    var = ncvar_get(fh,varnom[which(variables == nom_var)])
    #reacomodo la matriz var 
    varp = var[c((round(dimx/2,0)/2+1):dimx, 1:(round(dimy/2,0))),ncol(var):1,]
    #image.plot(varp[,,1])  
    
    c.lon=c(1:dimx)
    c.lat= c(1:dimy) 
    
    print(paste("GENERANDO LOS PREDICTORES NUEVOS EN CADA REGION",nclus," ",pre_nom))  
    
    puntos=expand.grid(x = c.lon, y = c.lat)    #generar los puntos para extraer las regiones interiores de los contornos  
    varp1=varp #copiar la matriz de reanalisis para redimensionarla
    dim(varp1)=c(dimx*dimy,length(time)) #redimensionar la matriz de 3d a 2d
    tiempos=seq((miny.pred-1979)*12+mes.referencia,(maxy.pred-1979)*12+mes.referencia,12) #genera los tiempos a extraer
    
    long=as.numeric(lon[puntos[ptos.inn,]$x])#Longitudes de los puntos a promediar areal
    lati=as.numeric(lat[puntos[ptos.inn,]$y]) #Latitudes de los puntos a promediar areal
    predictor=c()
    for(t in 1:length(tiempos) ){
      predictor[t]=mean(varp1[ptos.inn,tiempos[t]],na.rm=TRUE)
    }  
    
    if(sw == 0){
      x.col=data.frame(predictor)
      colnames(x.col)=pre_nom
      predictor.nuevo=x.col
      sw=1
    }else{
      x.col=data.frame(predictor)
      colnames(x.col)=pre_nom
      predictor.nuevo=cbind(predictor.nuevo,x.col)
    }      
    
  }
  predictor.nuevo=cbind(year=c(miny.pred:maxy.pred),predictor.nuevo)
  write.xlsx(predictor.nuevo,file=paste0(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO_PRED.xlsx")))
  
  #Leo los datos para la prediccion
  
  #Leo modelos de cada cluster
  modelos=read.xlsx(xlsxFile = paste0("Modelos/Cluster",nclus,"/Modelos_C",nclus,".xlsx"))
  #Selecciono el mejor modelo, ordenando por mejor de R2 de menor a mayor y tomando el primero
  bFormula=modelos[order(modelos$AdjR2,decreasing = T),][1,1]
  #Leo los predictores
  predictores=read.xlsx(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO.xlsx"))
  #Leo el predictando
  y.datos=read.xlsx(paste0("Pronostico/prueba.xlsx"),sheet = paste0("CLUSTER_",nclus))
  y.pred=c()
  pred=c()
  obs=c()
  ti=c()
  ts=c()
  for (iest in 2:length(y.datos[1,])){
    y.dato = ts(as.numeric(as.vector(y.datos[,iest])),start=1, end=cant.de.anios) 
    tercil=quantile(y.dato, c(0.40, 0.60))
    delta=(tercil[2]-tercil[1])*0.0
    tercil[1]=tercil[1]-delta
    tercil[2]=tercil[2]+delta
    #Ajusto la formula para obtener los coeficientes de nuevo.
    fit = lm(as.formula(bFormula),data = predictores)
    y.pred[iest-1]=predict.lm(fit, newdata=predictor.nuevo) # calculo la prediccion en el punto faltante con TSLM
    if(y.pred[iest-1] > tercil[2]){ pred[iest-1]="sobre"}   
    if(tercil[1]<=y.pred[iest-1] & y.pred[iest-1] <= tercil[2]){ pred[iest-1]="normal"}   
    if(y.pred[iest-1] < tercil[1]){ pred[iest-1]="sub"}   
    
    if(y.datos[miny.pred-1979+1,iest] > tercil[2]){ obs[iest-1]="sobre"}   
    if(tercil[1]<=y.datos[miny.pred-1979+1,iest] & y.datos[miny.pred-1979+1,iest] <= tercil[2]){ obs[iest-1]="normal"}   
    if(y.datos[miny.pred-1979+1,iest] < tercil[1]){ obs[iest-1]="sub"}   
    
    ti[iest-1]=tercil[1]
    ts[iest-1]=tercil[2]
  }

  res=data.frame()
  res=cbind(est=1:(length(y.datos[1,])-1),obs=as.numeric(y.datos[(miny.pred-1979+1),2:length(y.datos[1,])]),y.pred,ti,ts,obs,pred)
  addWorksheet(wb, sheetName = paste0("CLUSTER_",nclus))
  writeData(wb, sheet = paste0("CLUSTER_",nclus), x = res, startCol = 1, startRow = 1,rowNames = F)
  print(table(pred,obs))
}

saveWorkbook(wb, file = paste0("./Pronostico/clasif_esta2017.xlsx"),overwrite = T)




