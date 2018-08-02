library(glmnet)
library(plotmo)
library(openxlsx)

#Modificar aca para cambiar el directorio de trabajo
setwd("~/Dropbox/TesisEspecializacion/programas")

#parametros de referencia
miny <- 1979
maxy <- 2015
mes.referencia=11 #mes de referencia
cant.de.anios=maxy-miny+1
set.seed(12573) 

#funcion auxiliar 
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs,cex=0.3, ...)
}


#Leo las series medias de los clusters
datos.obs=read.xlsx("clusters/series.medias.xlsx",sheet="Clusters")
cluster.name=colnames(datos.obs[3:ncol(datos.obs)])

for(nclus in 1:length(cluster.name)){
  #Leo los predictores
  predictores=read.xlsx(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,".xlsx"))

  datos=datos.obs[cluster.name[nclus]][1:cant.de.anios,]  
  y=scale(datos)
  x=scale(as.matrix(predictores))

  mod <- glmnet(x=as.matrix(x),y= y,alpha=1)

  #esto hace la cross validacion  y despues muestra los predictores seleccionados como mejores
  #inicio
  set.seed(12573) 
  cvfit<-cv.glmnet(x,y,nfolds=length(x[,1]),grouped=F)
  yhat <- predict(cvfit, s=cvfit$lambda.1se, newx=x)
  
  #ploteo el error de la CV
  pdf(file=paste0("Lasso/Cluster_",nclus,".pdf"),width = 12,height=6)
    plot(cvfit,main=paste0("Cluster",nclus)) #plotea el error en base al lambda
  dev.off()
  

  glmcoef<-as.data.frame(as.matrix(coef(mod,cvfit$lambda.min)))
  
  coef.increase<-dimnames(glmcoef[glmcoef[,1]>0,0])[[1]] 
  coef.decrease<-dimnames(glmcoef[glmcoef[,1]<0,0])[[1]]
  
  allnames<-names(coef(mod)[,ncol(coef(mod))][order(coef(mod)[,ncol(coef(mod))],decreasing=TRUE)])
  allnames<-setdiff(allnames,allnames[grep("Intercept",allnames)]) # saco el "Intercept"
  
  #asigno los colores rojo, gris, verde
  cols<-rep("gray",length(allnames))
  cols[allnames %in% coef.increase]<-"red"      # positivos
  cols[allnames %in% coef.decrease]<-"blue"        # negativos
  

#grafico la extinsion de los predictores
  pdf(file=paste0("Lasso/Cluster_",nclus,"Coeficientes.pdf"),width = 12,height=6)
    i <- which(cvfit$lambda == cvfit$lambda.min)
    npred=cvfit$glmnet.fit$df[i]
    #plot_glmnet(mod, label=npred,xvar='lambda',main=paste0("Cluster_",nclus))  
    plot_glmnet(mod,label=TRUE,s=cvfit$lambda.min,col=cols,main=paste0("Cluster_",nclus))
  dev.off()

#Guardo los predictores seleccionados para los modelos
  ix = which(abs(glmcoef) > 0)[-1]
  vars=rownames(glmcoef)[ix]
  predictores.modelo=predictores[vars]
  write.xlsx(predictores.modelo,file=paste0(paste0("Predictores/Cluster",nclus,"/Predictores_C",nclus,"_LASSO.xlsx")))
}


#fin

# y=scale(datos)
# x=scale(as.matrix(predictores))
# set.seed(12573) 
# cvfit<-cv.glmnet(y = as.matrix(datos),x=as.matrix(predictores),nfolds=length(datos),grouped=F)
# yhat <- predict(cvfit, s="lambda.1se", newx=data.matrix(datos))

# 
# plot(y,type="l",col="red")
# lines(yhat,col="green")
# 
