# Trabajo de Especializacion 2018 
## Resumen
<p align="justify" >
Se estudió la variabilidad interanual de la lluvia de verano en la región Pampeana para intentar predecir la precipitación estacional utilizando modelos de regresión lineal múltiple. Este trabajo utilizó las precipitaciones observadas de verano (diciembre, enero y febrero) como predictando en la región Pampeana de Argentina, y como variables predictoras series atmosféricas y oceánicas globales del mes anterior al verano (noviembre). Se emplearon técnicas de agrupamiento para definir regiones similares de precipitación estacional de verano. Las principales variables predictoras globales utilizadas fueron la altura geopotencial (presión atmosférica), cantidad total de agua en la columna atmosférica, viento en altura (850hPa) y la temperatura de la superficie del mar. El análisis estadístico de las mismas nos permitió definir las series de variables predictoras ha ser utilizadas en cada región para la construcción de modelos de regresión lineal múltiple regionales. Finalmente se aplicaron los modelos regionales a una situación real de pronóstico.
</p>

## PROGRAMAS (Lenguaje R)
   - ### Análisis exploratorio de los datos.
      - **P0_a.DistrEstaciones.R** . DISTRIBUCION DE ESTACIONES METEOROLOGICAS.  
      - **P0_b.Boxplots.R**        . BOXPLOTS POR ESTACION Y POR AGRUPAMIENTO.    
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)
   - ### Clustering (agrupamientos).       
      - **P1_Clusters_DEF_PRE.R**  . AGRUPAMIENTO DE ESTACIONES (JERARQUICO y  NO- JERARQUICO).  
      - **P1_Tests_Clusters.R**   . ANOVA DE AGRUPAMIENTOS.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)
   - ### Generacion de series de datos.
      - **P2_SeriesEstacion.R**  . GENERA SERIES POR ESTACION METEO.  
      - **P2_SeriesMedias.R**    . GENERA SERIES MEDIAS POR AGRUPAMIENTO.     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)

      - **P3_Predictores.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)

      - **P4_Lasso.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)
         
      - **P5_Modelos.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)

      - **P6_Regresion_Clasificacion.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)

      - **P7_PronosticoXEstacion.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)

      - **P8_MapaVerificacion.R**    . GENERA PREDICTORES ( MAPAS DE CORRELACION y SERIES DE PREDICTORES).     
         - [RESULTADOS](https://github.com/alrolla/Especializacion_2018/tree/master/Analisis_Exploratorio)



