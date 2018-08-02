require(openxlsx)
library(fpp)
library(ggplot2)
library(reshape2)
#library(ade4)
library(MASS)

#Modificar aca para cambiar el directorio de trabajo
setwd("~/Dropbox/TesisEspecializacion/programas")

#parametros de referencia
miny <- 1979
maxy <- 2015
mes.referencia=11 #mes de referencia
cant.de.anios=maxy-miny+1

#Leo las series medias de los clusters
cluster.obs=read.xlsx("clusters/series.medias.xlsx",sheet="Clusters")
cluster.name=colnames(cluster.obs[3:ncol(cluster.obs)])

s_datos=rep(T,cant.de.anios)
#y.original=ts(as.numeric(as.vector(cluster.obs[,2+nclus])),start=1, end=cant.de.anios) 
y_sombrero <- function(bFormula,cant.de.anios,y.original,predictores ){
  m1=c()
  m2=c()
  obs=c()
  cvfit=c()
  
  bFormula=modelos[order(modelos$CV,decreasing = F),][1,1]
  #y.original=ts(as.numeric(as.vector(cluster.obs[,2+nclus])),start=1, end=cant.de.anios) 
  for (i in 1:length(s_datos)){
    s_datos_x=rep(T,cant.de.anios) # genero un vector de T con la longitud de la serie
    s_datos_x[i]=F  # asigno False al dato que quiero eleminar para predecir
    eval_p=predictores[which(s_datos_x == F),] # recupero los predictores del punto faltante
    predic_p=predictores[which(s_datos_x == T),] # recupero el resto de los predictores
    
    y.dato.x=y.original 
    eval_y_hat=y.dato.x[which(s_datos_x == F)] #recupero el predictando del punto faltante  
    y.dato=y.dato.x[which(s_datos_x == T)] # recupero el resto de los predictandos
    
    #Ajusto la formula para obtener los coeficientes de nuevo.
    #convierto los predictores en clase ts
    for(p in 1:length(predictor.name)){
      predic_p[,p] = ts(predic_p[,p],start =1, end=cant.de.anios-1)
    }  
    y.dato = ts(y.dato,start=1, end=cant.de.anios-1) # convierto los datos a time serie
    fit = tslm(as.formula(bFormula),data = predic_p) # ajusto el modelo de regresion con el modelo lineal de TSLM
    fit.2 <-  lm(as.formula(bFormula), data = predic_p) # ajusto el modelo de regresion con el modelo lineal de LM
    m1[i]=predict.lm(fit, newdata=eval_p) # calculo la prediccion en el punto faltante con TSLM
    m2[i]=predict.lm(fit.2, newdata=eval_p)# calculo la prediccion en el punto faltante con LM
    obs[i]=eval_y_hat # guardo el predictando delpunto faltante
  }
  return(m2)    
}

for(nclus in 1:length(cluster.name)){

  #nclus=1
  #Abro el workbook para grabar los datos
  wb <- createWorkbook()
 
  
  #Creoq el directorio de trabajo para cada cluster
  dir.create(path = paste0("./regresion_Class_Verif/Cluster",nclus), showWarnings = FALSE)
  #Leo modelos de cada cluster
  modelos=read.xlsx(xlsxFile = paste0("Modelos/Cluster",nclus,"/Modelos_C",nclus,".xlsx"))
  #Leo los predictores
  predictores=read.xlsx(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO.xlsx"))
  predictor.name=colnames(predictores[1:ncol(predictores)])
  y.dato = ts(as.numeric(as.vector(cluster.obs[,2+nclus])),start=1, end=cant.de.anios) 

  #convierto los predictores en clase ts
  for(p in 1:length(predictor.name)){
    predictores[,p] = ts(predictores[,p],start =1, end=cant.de.anios)
  }
  
  #Selecciono el mejor modelo, ordenando por mejor de CV de menor a mayor y tomando el primero
  bFormula=modelos[order(modelos$AdjR2,decreasing = T),][1,1]
  
  #Ajusto la formula para obtener los coeficientes de nuevo.
  fit = tslm(as.formula(bFormula),data = predictores)
  
  #Evaluo el modelo en los puntos de entrenamiento
  y.pred <- predict(fit, predictores)

  y.hat=y_sombrero(bFormula,cant.de.anios,y.dato,predictores )
  #Armo los datos para plotear las curvas de observado y estimado
  p.data=melt(data.frame(year=cluster.obs$YEAR[1:cant.de.anios],Observ=as.numeric(y.dato),Modelo=as.numeric(y.pred),Y_HAT=as.numeric(y.hat)),id="year")
  
  
  #Calculo los terciles para la serie de observaciones
  tercil=quantile(y.dato, c(.33, .66))
  
  
  
  
  #Ploteo las de series de observaciones y estimaciones
  ggplot(data=p.data,
         aes(x=year, y=value, colour=variable)) +
         geom_line() +
         geom_point() +
         geom_hline(yintercept = tercil[1])+
         geom_hline(yintercept = tercil[2])+
         scale_x_continuous(expand = c(0.01, 0),breaks=seq(min(p.data$year), max(p.data$year), 1))+
         scale_y_continuous(expand = c(0.01, 0))+
         geom_label(x=1979.5,y=tercil[1],label=round(tercil[1],1),colour="black",size=1.5,fontface = "plain")+
         geom_label(x=1979.5,y=tercil[2],label=round(tercil[2],1),colour="black",size=1.5,fontface = "plain")+
         labs(x = "Año")+
         labs(y = "Precip DEF")+
         labs(title = paste0("Modelo Region: ",nclus))+
         labs(color = "Pre DEF")+
         theme(axis.text.x = element_text(angle = 90, vjust=.5,size=7),
               axis.text.y = element_text(size=7))
  ggsave(paste0("./regresion_Class_Verif/Cluster",nclus,"/serieC",nclus,".pdf"),units = c("in"),width=7,height=3)
  
  y.obs.clase=rep(NA,length(y.dato))

  y.obs.clase[which(y.dato>tercil[2])]="sobre"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="normal"
  y.obs.clase[which(y.dato<tercil[1])]="sub"
  #y.obs.clase

  y.pred.clase=rep(NA,length(y.pred))
  y.pred.clase[which(y.pred>tercil[2])]="sobre"
  y.pred.clase[which(tercil[1]<=y.pred & y.pred<=tercil[2])]="normal"
  y.pred.clase[which(y.pred<tercil[1])]="sub"

  y.pred.clase.2=rep(NA,length(y.hat))
  y.pred.clase.2[which(y.hat>tercil[2])]="sobre"
  y.pred.clase.2[which(tercil[1]<=y.hat & y.hat<=tercil[2])]="normal"
  y.pred.clase.2[which(y.hat<tercil[1])]="sub"

  clas=data.frame(observado= y.obs.clase,pronosticado=y.pred.clase)
  clas2=data.frame(observado= y.obs.clase,pronosticado=y.pred.clase.2)

  addWorksheet(wb, sheetName = "Clasificacion")
  df.clasificacion=data.frame(year=cluster.obs$YEAR[1:cant.de.anios] ,
                              OBS=round(as.numeric(y.dato),1),
                              MOD=round(y.pred,1),
                              OBS.clas=clas$observado,
                              MOD.clas=clas$pronosticado,
                              MOD2=round(y.hat,1),
                              OBS2.clas=clas2$observado,
                              MOD2.clas=clas2$pronosticado          
                              )
  writeData(wb, sheet = "Clasificacion", x = "MODELO", startCol = 1, startRow = 1,rowNames = T)
  writeData(wb, sheet = "Clasificacion", x = t(data.frame(round(fit$coefficients,2))), startCol = 1, startRow = 2,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.clasificacion, startCol = 1, startRow = 4,rowNames = F)

  
  #tabla de contingencia 3 clases TOTAL del MEJOR MODELO
  t.c.numerica=t(table(clas)) #qcomodo la tabla de contingencias para ordenarla
  t.c.numerica=t.c.numerica[c(3,1,2),c(3,1,2)]
  t.c.numerica
  addWorksheet(wb, sheetName = "Skill")
  writeData(wb, sheet = "Skill", x = as.data.frame.matrix(t.c.numerica), startCol = 2, startRow = 3,rowNames = T)
  t.c.porcentual=round(t.c.numerica/sum(t.c.numerica)*100,1)
  
  t.c.porcentual
  writeData(wb, sheet = "Skill", x = as.data.frame.matrix(t.c.porcentual), startCol = 2, startRow = 9,rowNames = T)
  
  
  #tabla de confusion SUBNORMAL
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="NO"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="NO"
  y.obs.clase[which(y.dato<tercil[1])]="YES"
  #y.obs.clase
  
  y.pred.clase=rep(NA,length(y.pred))
  y.pred.clase[which(y.pred>tercil[2])]="NO"
  y.pred.clase[which(tercil[1]<=y.pred & y.pred<=tercil[2])]="NO"
  y.pred.clase[which(y.pred<tercil[1])]="YES"
  #y.pred.clase
  
  clas.sub=data.frame(SUB.OBS= y.obs.clase,SUB.PRONO=y.pred.clase)

  sub.c.numerica=t(table(clas.sub))
  sub.c.numerica=sub.c.numerica[2:1,2:1]
  #  SUB
  sub.c.numerica
  
  sub.df=data.frame(Hit.Rate=round(sum(sub.c.numerica[1,1],sub.c.numerica[2,2])/sum(sub.c.numerica),2),
                     Threat.Score=round( sub.c.numerica[1,1]/sum(sub.c.numerica[1,1],sub.c.numerica[2,1],sub.c.numerica[1,2]),2),
                     POD=round(sub.c.numerica[1,1]/sum(sub.c.numerica[1,1],sub.c.numerica[2,1]),2),
                     FAR=round( sub.c.numerica[1,2]/sum(sub.c.numerica[1,2],sub.c.numerica[2,2]),2)
  ) 
  
  writeData(wb, sheet = "Skill", x = "SUBNORMAL", startCol = 7, startRow = 2,rowNames = T)
  writeData(wb, sheet = "Skill", x = as.data.frame.matrix(sub.c.numerica), startCol = 7, startRow = 3,rowNames = T)
  writeData(wb, sheet = "Skill", x = sub.df, startCol = 11, startRow = 3,rowNames = F)
  
  #tabla de confusion NORMAL
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="NO"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="YES"
  y.obs.clase[which(y.dato<tercil[1])]="NO"
  #y.obs.clase
  
  y.pred.clase=rep(NA,length(y.pred))
  y.pred.clase[which(y.pred>tercil[2])]="NO"
  y.pred.clase[which(tercil[1]<=y.pred & y.pred<=tercil[2])]="YES"
  y.pred.clase[which(y.pred<tercil[1])]="NO"
  #y.pred.clase
  
  clas.normal=data.frame(NORMAL.OBS= y.obs.clase,NORMAL.PRONO=y.pred.clase)

  normal.c.numerica=t(table(clas.normal))
  normal.c.numerica=normal.c.numerica[2:1,2:1]
  
  #  NORMAL
  normal.c.numerica
  
  normal.df=data.frame(Hit.Rate=round(sum(normal.c.numerica[1,1],normal.c.numerica[2,2])/sum(normal.c.numerica),2),
                       Threat.Score=round( normal.c.numerica[1,1]/sum(normal.c.numerica[1,1],normal.c.numerica[2,1],normal.c.numerica[1,2]),2),
                       POD=round(normal.c.numerica[1,1]/sum(normal.c.numerica[1,1],normal.c.numerica[2,1]),2),
                       FAR=round( normal.c.numerica[1,2]/sum(normal.c.numerica[1,2],normal.c.numerica[2,2]),2)
  )
  writeData(wb, sheet = "Skill", x = "NORMAL", startCol = 7, startRow = 7,rowNames = T)
  writeData(wb, sheet = "Skill", x = as.data.frame.matrix(normal.c.numerica), startCol = 7, startRow = 8,rowNames = T)
  writeData(wb, sheet = "Skill", x = normal.df, startCol = 11, startRow = 8,rowNames = F)
  
  #tabla de confusion SOBRE
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="YES"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="NO"
  y.obs.clase[which(y.dato<tercil[1])]="NO"
  #y.obs.clase
  
  y.pred.clase=rep(NA,length(y.pred))
  y.pred.clase[which(y.pred>tercil[2])]="YES"
  y.pred.clase[which(tercil[1]<=y.pred & y.pred<=tercil[2])]="NO"
  y.pred.clase[which(y.pred<tercil[1])]="NO"
  #y.pred.clase
  
  clas.sobre=data.frame(SOBRE.OBS= y.obs.clase, SOBRE.PRONO=y.pred.clase)
  
  sobre.c.numerica=t(table(clas.sobre))
  sobre.c.numerica=sobre.c.numerica[2:1,2:1]
  sobre.c.numerica
  
  sobre.df=data.frame(Hit.Rate=round(sum(sobre.c.numerica[1,1],sobre.c.numerica[2,2])/sum(sobre.c.numerica),2),
                      Threat.Score=round( sobre.c.numerica[1,1]/sum(sobre.c.numerica[1,1],sobre.c.numerica[2,1],sobre.c.numerica[1,2]),2),
                      POD=round( sobre.c.numerica[1,1]/sum(sobre.c.numerica[1,1],sobre.c.numerica[2,1]),2),
                      FAR=round( sobre.c.numerica[1,2]/sum(sobre.c.numerica[1,2],sobre.c.numerica[2,2]),2)
  )
  writeData(wb, sheet = "Skill", x = "SOBRENORMAL", startCol = 7, startRow = 12,rowNames = T)
  writeData(wb, sheet = "Skill", x = as.data.frame.matrix(sobre.c.numerica), startCol = 7, startRow = 13,rowNames = T)
  writeData(wb, sheet = "Skill", x = sobre.df, startCol = 11, startRow = 13,rowNames = F)  

  
  #Caculo de limites  
  lim.min=round(min(y.pred)/100,0)*100
  lim.max=(round(max(y.pred)/100,0))*100
  #calculo de breaks
  break1=seq(lim.min,lim.max,length.out = 8)
  break1[1]=-Inf
  break1[length(break1)]=Inf
  #calculo de cortes
  cut.o=cut(as.numeric(y.dato),breaks = break1)
  cut.m=cut(as.numeric(y.pred),breaks = break1)
  #series de frecuencias
  serie.o=data.frame(x=seq(1,length(break1)),y=cumsum(c(0,table(cut.o)/length(y.dato))))
  serie.m=data.frame(x=seq(1,length(break1)),y=cumsum(c(0,table(cut.m)/length(y.pred))))
  
  #grafico de cdf's de modelo y observaciones
  ggplot(data=serie.o,aes(x=x, y=y)) +
    geom_line(data=serie.o,aes(x=x,y=y,colour="Observacion")) +
    geom_point(color="black")+
    geom_line(data = serie.m,aes(x=x,y=y,colour="Modelo"))+
    geom_point(data = serie.m,aes(x=x,y=y),color="black")+
    scale_y_continuous(expand = c(0.01, 0))+
    scale_x_continuous(expand = c(0.01, 0),breaks=seq(1:length(break1)),labels=round(break1,0))+
    theme(legend.title=element_blank())+
    labs(x = "Precip DEF")+
    labs(y = "Probabilidad acumulada")
  ggsave(paste0("./regresion_Class_Verif/Cluster",nclus,"/CDFs_C",nclus,".pdf"),units = c("in"),width=7,height=6)
  
  #test de bondad de ajuste chi-cuadrado.
  
  df.cut.o=data.frame(table(cut.o))
  df.cut.o=data.frame(clase=df.cut.o$cut.o,frecuencia=df.cut.o$Freq,P.Acum=cumsum(table(cut.o)/length(y.dato)))
  df.cut.m=data.frame(table(cut.m))
  df.cut.m=data.frame(clase=df.cut.m$cut.m,frecuencia=df.cut.m$Freq,P.Acum=cumsum(table(cut.m)/length(y.pred)))
  
  t.chi2=chisq.test(data.frame(o=df.cut.o$frecuencia,m=df.cut.m$frecuencia))
  if(t.chi2$p.value > 0.05 ){
    res.t.chi2="NO RECHAZO H0 : El Ajuste es bueno :-) "
  }else{
    res.t.chi2="RECHAZO H0 : El Ajuste no es bueno :-( "
  }
  
  df.t.chi2=data.frame(X.squared=t.chi2$statistic,DF=t.chi2$parameter,p.value=t.chi2$p.value,test=res.t.chi2)

    
  writeData(wb, sheet = "Clasificacion", x = "OBSERVACION", startCol = 10, startRow = 5,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.cut.o, startCol = 10, startRow = 6,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = "MODELO", startCol = 14, startRow = 5,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.cut.m, startCol = 14, startRow = 6,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.t.chi2, startCol = 18, startRow = 6,rowNames = F)
  

  
  
  
  
  #tabla de contingencia 3 clases TOTAL de los y_HAT
  t.c.numerica2=t(table(clas2)) #qcomodo la tabla de contingencias para ordenarla
  t.c.numerica2=t.c.numerica2[c(3,1,2),c(3,1,2)]
  t.c.numerica2
  addWorksheet(wb, sheetName = "Skill2")
  writeData(wb, sheet = "Skill2", x = as.data.frame.matrix(t.c.numerica2), startCol = 2, startRow = 3,rowNames = T)
  t.c.porcentual2=round(t.c.numerica2/sum(t.c.numerica2)*100,1)
  
  t.c.porcentual2
  writeData(wb, sheet = "Skill2", x = as.data.frame.matrix(t.c.porcentual2), startCol = 2, startRow = 9,rowNames = T)
  
  
  #tabla de confusion SUBNORMAL
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="NO"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="NO"
  y.obs.clase[which(y.dato<tercil[1])]="YES"
  #y.obs.clase
  
  y.pred.clase2=rep(NA,length(y.hat))
  y.pred.clase2[which(y.hat>tercil[2])]="NO"
  y.pred.clase2[which(tercil[1]<=y.hat & y.hat<=tercil[2])]="NO"
  y.pred.clase2[which(y.hat<tercil[1])]="YES"
  #y.pred.clase
  
  clas.sub2=data.frame(SUB.OBS= y.obs.clase,SUB.PRONO=y.pred.clase2)
  
  sub.c.numerica2=t(table(clas.sub2))
  sub.c.numerica2=sub.c.numerica2[2:1,2:1]
  #  SUB
  sub.c.numerica2
  
  sub.df2=data.frame(Hit.Rate=round(sum(sub.c.numerica2[1,1],sub.c.numerica2[2,2])/sum(sub.c.numerica2),2),
                    Threat.Score=round( sub.c.numerica2[1,1]/sum(sub.c.numerica2[1,1],sub.c.numerica2[2,1],sub.c.numerica2[1,2]),2),
                    POD=round(sub.c.numerica2[1,1]/sum(sub.c.numerica2[1,1],sub.c.numerica2[2,1]),2),
                    FAR=round( sub.c.numerica2[1,2]/sum(sub.c.numerica2[1,2],sub.c.numerica2[2,2]),2)
  ) 
  
  writeData(wb, sheet = "Skill2", x = "SUBNORMAL", startCol = 7, startRow = 2,rowNames = T)
  writeData(wb, sheet = "Skill2", x = as.data.frame.matrix(sub.c.numerica2), startCol = 7, startRow = 3,rowNames = T)
  writeData(wb, sheet = "Skill2", x = sub.df2, startCol = 11, startRow = 3,rowNames = F)
  
  #tabla de confusion NORMAL
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="NO"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="YES"
  y.obs.clase[which(y.dato<tercil[1])]="NO"
  #y.obs.clase
  
  y.pred.clase2=rep(NA,length(y.hat))
  y.pred.clase2[which(y.hat>tercil[2])]="NO"
  y.pred.clase2[which(tercil[1]<=y.hat & y.hat<=tercil[2])]="YES"
  y.pred.clase2[which(y.hat<tercil[1])]="NO"
  #y.pred.clase
  
  clas.normal2=data.frame(NORMAL.OBS= y.obs.clase,NORMAL.PRONO=y.pred.clase2)
  
  normal.c.numerica2=t(table(clas.normal2))
  normal.c.numerica2=normal.c.numerica2[2:1,2:1]
  
  #  NORMAL
  normal.c.numerica2
  
  normal.df2=data.frame(Hit.Rate=round(sum(normal.c.numerica2[1,1],normal.c.numerica2[2,2])/sum(normal.c.numerica2),2),
                       Threat.Score=round( normal.c.numerica2[1,1]/sum(normal.c.numerica2[1,1],normal.c.numerica2[2,1],normal.c.numerica2[1,2]),2),
                       POD=round(normal.c.numerica2[1,1]/sum(normal.c.numerica2[1,1],normal.c.numerica2[2,1]),2),
                       FAR=round( normal.c.numerica2[1,2]/sum(normal.c.numerica2[1,2],normal.c.numerica2[2,2]),2)
  )
  writeData(wb, sheet = "Skill2", x = "NORMAL", startCol = 7, startRow = 7,rowNames = T)
  writeData(wb, sheet = "Skill2", x = as.data.frame.matrix(normal.c.numerica2), startCol = 7, startRow = 8,rowNames = T)
  writeData(wb, sheet = "Skill2", x = normal.df2, startCol = 11, startRow = 8,rowNames = F)
  
  #tabla de confusion SOBRE
  y.obs.clase=rep(NA,length(y.dato))
  
  y.obs.clase[which(y.dato>tercil[2])]="YES"
  y.obs.clase[which(tercil[1]<=y.dato & y.dato<=tercil[2])]="NO"
  y.obs.clase[which(y.dato<tercil[1])]="NO"
  #y.obs.clase
  
  y.pred.clase2=rep(NA,length(y.hat))
  y.pred.clase2[which(y.hat>tercil[2])]="YES"
  y.pred.clase2[which(tercil[1]<=y.hat & y.hat<=tercil[2])]="NO"
  y.pred.clase2[which(y.hat<tercil[1])]="NO"
  #y.pred.clase
  
  clas.sobre2=data.frame(SOBRE.OBS= y.obs.clase, SOBRE.PRONO=y.pred.clase2)
  
  sobre.c.numerica2=t(table(clas.sobre2))
  sobre.c.numerica2=sobre.c.numerica2[2:1,2:1]
  sobre.c.numerica2
  
  sobre.df2=data.frame(Hit.Rate=round(sum(sobre.c.numerica2[1,1],sobre.c.numerica2[2,2])/sum(sobre.c.numerica2),2),
                      Threat.Score=round( sobre.c.numerica2[1,1]/sum(sobre.c.numerica2[1,1],sobre.c.numerica2[2,1],sobre.c.numerica2[1,2]),2),
                      POD=round( sobre.c.numerica2[1,1]/sum(sobre.c.numerica2[1,1],sobre.c.numerica2[2,1]),2),
                      FAR=round( sobre.c.numerica2[1,2]/sum(sobre.c.numerica2[1,2],sobre.c.numerica2[2,2]),2)
  )
  writeData(wb, sheet = "Skill2", x = "SOBRENORMAL", startCol = 7, startRow = 12,rowNames = T)
  writeData(wb, sheet = "Skill2", x = as.data.frame.matrix(sobre.c.numerica2), startCol = 7, startRow = 13,rowNames = T)
  writeData(wb, sheet = "Skill2", x = sobre.df2, startCol = 11, startRow = 13,rowNames = F)  
  
  
  #Caculo de limites  
  lim.min=round(min(y.hat)/100,0)*100
  lim.max=(round(max(y.hat)/100,0))*100
  #calculo de breaks
  break1=seq(lim.min,lim.max,length.out = 8)
  break1[1]=-Inf
  break1[length(break1)]=Inf
  #calculo de cortes
  cut.o=cut(as.numeric(y.dato),breaks = break1)
  cut.m=cut(as.numeric(y.hat),breaks = break1)
  #series de frecuencias
  serie.o=data.frame(x=seq(1,length(break1)),y=cumsum(c(0,table(cut.o)/length(y.dato))))
  serie.m=data.frame(x=seq(1,length(break1)),y=cumsum(c(0,table(cut.m)/length(y.hat))))
  
  #grafico de cdf's de modelo y observaciones
  ggplot(data=serie.o,aes(x=x, y=y)) +
    geom_line(data=serie.o,aes(x=x,y=y,colour="Observacion")) +
    geom_point(color="black")+
    geom_line(data = serie.m,aes(x=x,y=y,colour="Mod_YHAT"))+
    geom_point(data = serie.m,aes(x=x,y=y),color="black")+
    scale_y_continuous(expand = c(0.01, 0))+
    scale_x_continuous(expand = c(0.01, 0),breaks=seq(1:length(break1)),labels=round(break1,0))+
    theme(legend.title=element_blank())+
    labs(x = "Precip DEF")+
    labs(y = "Probabilidad acumulada")
  ggsave(paste0("./regresion_Class_Verif/Cluster",nclus,"/CDFs_C",nclus,"_YHAT.pdf"),units = c("in"),width=7,height=6)
  
  #test de bondad de ajuste chi-cuadrado.
  
  df.cut.o=data.frame(table(cut.o))
  df.cut.o=data.frame(clase=df.cut.o$cut.o,frecuencia=df.cut.o$Freq,P.Acum=cumsum(table(cut.o)/length(y.dato)))
  df.cut.m=data.frame(table(cut.m))
  df.cut.m=data.frame(clase=df.cut.m$cut.m,frecuencia=df.cut.m$Freq,P.Acum=cumsum(table(cut.m)/length(y.pred)))
  
  t.chi2=chisq.test(data.frame(o=df.cut.o$frecuencia,m=df.cut.m$frecuencia))
 
  if(!is.na(t.chi2$p.value)){
    if(t.chi2$p.value > 0.05 ){
      res.t.chi2="NO RECHAZO H0 : El Ajuste es bueno :-) "
    }else{
      res.t.chi2="RECHAZO H0 : El Ajuste no es bueno :-( "
    }
    df.t.chi2=data.frame(X.squared=t.chi2$statistic,DF=t.chi2$parameter,p.value=t.chi2$p.value,test=res.t.chi2)  
  }else{
    res.t.chi2="NO PUDE HACER EL TEST" 
    df.t.chi2=data.frame(X.squared=t.chi2$statistic,DF=t.chi2$parameter,p.value="NA",test=res.t.chi2)  
  }
  
    
  df.t.chi2=data.frame(X.squared=t.chi2$statistic,DF=t.chi2$parameter,p.value=t.chi2$p.value,test=res.t.chi2)
  
  writeData(wb, sheet = "Clasificacion", x = "OBSERVACION", startCol = 10, startRow = 20,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.cut.o, startCol = 10, startRow = 21,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = "MODELO2", startCol = 14, startRow = 20,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.cut.m, startCol = 14, startRow = 21,rowNames = F)
  writeData(wb, sheet = "Clasificacion", x = df.t.chi2, startCol = 18, startRow = 21,rowNames = F)

  saveWorkbook(wb, file = paste0("./regresion_Class_Verif/Cluster",nclus,"/VerificacionC",nclus,".xlsx"),overwrite = T)

  
}  




