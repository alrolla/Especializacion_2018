# Trabajo de Especialización 2018 (ALR) [descargar](https://github.com/alrolla/Especializacion_2018/blob/master/Doc/T_Esp_ALFREDO_ROLLA_2018.pdf)
## Resumen
<p align="justify" >
Se estudió la variabilidad interanual de la lluvia de verano en la región Pampeana para intentar predecir la precipitación estacional utilizando modelos de regresión lineal múltiple. Este trabajo utilizó las precipitaciones observadas de verano (diciembre, enero y febrero) como predictando en la región Pampeana de Argentina, y como variables predictoras series de variables atmosféricas y oceánicas globales del mes anterior al verano (noviembre). Se emplearon técnicas de agrupamiento para definir regiones similares de precipitación estacional de verano. Las principales variables predictoras globales utilizadas fueron la altura geopotencial (presión atmosférica), cantidad total de agua en la columna atmosférica, viento en capas bajas de la atmósfera (850hPa) y la temperatura de la superficie del mar. El análisis estadístico de las mismas permitió definir las series de variables predictoras a ser utilizadas en cada región para la construcción de modelos de regresión lineal múltiple regionales. Finalmente se aplicaron los modelos regionales a una situación real de pronóstico.
</p>

## PROGRAMAS (Lenguaje R)
   - ### Análisis exploratorio de los datos.
      - **P0_a.DistrEstaciones.R** . DISTRIBUCION DE ESTACIONES METEOROLOGICAS.  
      - **P0_b.Boxplots.R**        . BOXPLOTS POR ESTACION Y POR AGRUPAMIENTO.    
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)
   - ### Clustering (agrupamientos).       
      - **P1_Clusters_DEF_PRE.R**  . AGRUPAMIENTO DE ESTACIONES (JERARQUICO y  NO- JERARQUICO).  
      - **P1_Tests_Clusters.R**   . ANOVA DE AGRUPAMIENTOS.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Agrupamientos)
   - ### Generacion de series de datos.
      - **P2_SeriesEstacion.R**  . GENERA SERIES POR ESTACION METEO.  
      - **P2_SeriesMedias.R**    . GENERA SERIES MEDIAS POR AGRUPAMIENTO.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Series_Medias_Agrup_Estaciones)
   - ### Análisis de forzantes globales.         
      - **P3_Predictores.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Predictores)
   - ### LASSO (pre-selección de predictores).     
      - **P4_Lasso.R**    . SELECCION DE PREDICTORES SIGNIFICATIVOS.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Lasso)
   - ### Construcción de los modelos.              
      - **P5_Modelos.R**    . STEP_FORWAD GENERACION DE MODELOS CON LOS PREDICTORES SELECCIONADOS.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Modelos)
   - ### Verificación de los modelos.             
      - **P6_Regresion_Clasificacion.R**    . VERIFICACION Y LA CLASIFICACION EN SUB,SOBRE y NORMAL.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Regresion_Clasificacion)
   - ### Pronóstico estacional de verano.      
      - **P7_PronosticoXEstacion.R**    . PRONOSTICO PARA LA REGION CON LOS MEJORES MODELOS.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Pronostico)
   - ### Verificación del pronóstico para el año 2016     
      - **P8_MapaVerificacion.R**    . MAPA RESULTANTE DEL PRONOSTICO.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Pronostico)



