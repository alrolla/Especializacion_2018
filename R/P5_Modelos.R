require(openxlsx)
library(fpp)

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

for(nclus in 1:length(cluster.name)){
  #Leo los predictores
  predictores=read.xlsx(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO.xlsx"))
  predictor.name=colnames(predictores[1:ncol(predictores)])
  y.dato = ts(as.numeric(as.vector(cluster.obs[,2+nclus])),start=1, end=37) 
  #convierto los predictores en clase ts
  for(p in 1:length(predictor.name)){
    predictores[,p] = ts(predictores[,p],start =1, end=cant.de.anios)
  }
  dir.create(path = paste0("./Modelos/Cluster",nclus), showWarnings = FALSE)
  
  n <- length(predictor.name)
  #Genero las combinaciones de predictores
  id <- unlist(
    lapply(1:n,
           function(i)combn(1:n,i,simplify=F)
    )
    ,recursive=F)
  #Genero las formulas
  Formulas <- sapply(id,function(i)
    paste("y.dato ~ ",paste0(predictor.name[i],collapse="+"))
  )
  
  umbral=0.1
  resultado.modelo=NULL
  coef=list()
  i=1
  for(fml in 1:length(Formulas)){
    
    #Ajusto al modelo propuesto
    fit = tslm(as.formula(Formulas[fml]),data=predictores)
    errors = CV(fit)
    resultado.modelo <- rbind(resultado.modelo,t(errors))
    coef[[i]]=fit$coefficients
    i=i+1
    #Chequeo el umbral
    if (errors[["AdjR2"]] > umbral){
      fit.final=fit
      print(fit$coefficients)
      umbral=errors[["AdjR2"]]
      print(paste("fml:",fml,"error: ",errors[["AdjR2"]],"umbral",umbral))
    }
    
  }
  resultado.modelo2=data.frame(Formulas,resultado.modelo,as.character(coef))
  colnames(resultado.modelo2)[1] ="Formula"
  colnames(resultado.modelo2)[7] ="Coeficientes"
  
  print(paste("Mejor modelo:" ))
  fit.final$coefficients      
  
  write.xlsx(resultado.modelo2,file=paste0(paste0("Modelos/Cluster",nclus,"/Modelos_C",nclus,".xlsx")))

}  
