library(RMySQL)
library(openxlsx)

setwd("~/Dropbox/TesisEspecializacion/programas")

load("variables/clusters.RData")

#Recupero la variable CodEst clusterizada

#Voy a armar las series medias de las regiones definidas.
nc=6
con <- dbConnect(MySQL(), user="root", password="", dbname="TesisEspecializacion", host="localhost")

sw=0
for(cluster in 1:nc){
    esta=CodEst.g[which(CodEst.g[,"cluster"] == cluster),"idOMM"]
    c.esta=paste(shQuote(esta), collapse=", ")
    
    sql=paste("select YEAR,AVG(DEF) as mDEF from SMN_INTA_MON_DATA_ARG where idOMM in (",c.esta,") group by YEAR having YEAR >= 1979")
    
    #Leer la serie media de las estaciones del cluster
    rs <- dbSendQuery(con, sql)
    serie <- fetch(rs,n=-1)
    if(sw == 0){
      colnames(serie)=c("YEAR",paste0("CLUSTER",cluster))
      serie.clusters=serie
      serie.clusters$CLUSTER1=round(serie.clusters$CLUSTER1,1)
      sw=1
    }else{
      serie$mDEF=round(serie$mDEF,1)
      serie.clusters=cbind(serie.clusters,serie$mDEF)   
      names(serie.clusters)[names(serie.clusters) == "serie$mDEF"] <- paste0("CLUSTER",cluster)
    }
}

#####################################################################################
## Create Workbook object and add worksheets
wb <- createWorkbook()
## Add worksheets
addWorksheet(wb, "Clusters")
writeData(wb, sheet="Clusters", x=serie.clusters, startCol = 1, startRow =1, rowNames = TRUE)
#####################################################################################
## Save workbook
saveWorkbook(wb, "clusters/series.medias.xlsx", overwrite = TRUE)


