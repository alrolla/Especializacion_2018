library(RMySQL)
library(openxlsx)

setwd("~/Dropbox/TesisEspecializacion/programas")

load("variables/clusters.RData")

#Recupero la variable CodEst clusterizada

#Voy a armar las series medias de las regiones definidas.
nc=6
con <- dbConnect(MySQL(), user="root", password="", dbname="TesisEspecializacion", host="localhost")


wb <- createWorkbook()
sw=0
for(cluster in 1:nc){
  esta=CodEst.g[which(CodEst.g[,"cluster"] == cluster),"idOMM"]
  c.esta=paste(shQuote(esta), collapse=", ")
  
  for(iest in 1:length(esta)){
          sql=paste("select YEAR,DEF  from SMN_INTA_MON_DATA_ARG where idOMM = ",esta[iest]," and YEAR >= 1979")
          #Leer la serie media de las estaciones del cluster
          rs <- dbSendQuery(con, sql)
          serie <- fetch(rs,n=-1)
          
          print(esta[iest])
    
          if(sw == 0){
            colnames(serie)=c("YEAR",esta[iest])
            serie.clusters=serie
            #names(serie.clusters)=paste0(esta[1])
            #serie.clusters$CLUSTER1=round(serie.clusters$CLUSTER1,1)
            sw=1
          }else{
            serie$DEF=round(serie$DEF,1)
            serie.clusters=cbind(serie.clusters,serie$DEF)   
            names(serie.clusters)[names(serie.clusters) == "serie$DEF"] <- paste0(esta[iest])
          }
  }  
  sw=0
  addWorksheet(wb, sheetName = paste0("CLUSTER_",cluster))
  writeData(wb, sheet = paste0("CLUSTER_",cluster), x = serie.clusters, startCol = 1, startRow = 1,rowNames = F)
  
 
  
}
saveWorkbook(wb, file = paste0("./Pronostico/prueba.xlsx"),overwrite = T)
dbDisconnect(con)
#####################################################################################
## Create Workbook object and add worksheets
#wb <- createWorkbook()
## Add worksheets
#addWorksheet(wb, "Clusters")
#writeData(wb, sheet="Clusters", x=serie.clusters, startCol = 1, startRow =1, rowNames = TRUE)
#####################################################################################
## Save workbook
#saveWorkbook(wb, "clusters/series.medias.xlsx", overwrite = TRUE)


