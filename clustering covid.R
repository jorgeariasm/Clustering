---
title: "Clustering dataset Covid"
author: "Jorge Arias"
date: "02/09/2020"
output:
  html_document:
    theme: cerulean
    toc: yes
    toc_depth: 1
---

0. - Parámetros

```{r}
options(scipen=999)#Desactiva la notación científica
```

1. - Preparación del entorno
1.1 - Cargamos las librerías que vamos a utilizar

```{r}
#lista de paquetes que vamos a usar
paquetes <- c('data.table',#para leer y escribir datos de forma rapida
              'dplyr',#para manipulación de datos
              'tidyr',#para manipulación de datos
              'ggplot2',#para gráficos 
              'TeachingDemos', #para caras de chernoff
              'aplpack' #caras chernoff
              
)
#Crea un vector lógico con si están instalados o no
instalados <- paquetes %in% installed.packages()
#Si hay al menos uno no instalado los instala
if(sum(instalados == FALSE) > 0) {
  install.packages(paquetes[!instalados])
}
lapply(paquetes,require,character.only = TRUE)
```

1.2 - Cargamos los datos
Usamos fread de data.table para una lectura mucho mas rapida
```{r}
df <- fread('agregados.csv')
```

2 - Análisis exploratorio
2.1 - Análisis exploratorio general y tipo de datos
```{r, message=TRUE, warning=TRUE}
as.data.frame(sort(names(df)))
str(df)
glimpse(df)
```

Observaciones: Encontramos un dataset con 8 variables y 1737 registros, con formatos a cambiar como la fecha. Los últimos 6 registros son anotaciones que alteran los valores del dataset. (serán eliminados)

2.2 - Calidad de datos: Estadísticos básicos
Hacemos un summary, con lapply que sale en formato de lista y se lee mejor
```{r}
lapply(df,summary)
```

2.3 - Calidad de datos: Análisis de nulos
```{r}
data.frame(colSums(is.na(df)))
```

Observaciones: Encontramos un alto numero de núlos, los convertiremos a ceros.

2.4 - Acciones resultado del analisis de calidad de datos y exploratorio

Vamos a hacer lo siguiente:
- eliminar las variables sin información (últimos 6 registros)
- Convertir nulos a ceros
- transformar a Date las variables de fecha


```{r}
df <- df[1:1729, ]

df[is.na(df)] <- 0

df$FECHA <- as.Date(df$FECHA, format="%d/%m/%Y")
```

Hacemos nuevamente un Lapply para conocer de forma f;acil el rango de fechas del dataset

```{r}
lapply(df,summary)
```

Ahora podemos comprobar que los datos recogidos son desde el 20-02-2020 al 20-05-2020

3. - Transformación de datos:

3.1. -  Creación variable caso confirmados: Se usará la variable CASOS y se actualizará con la suma de PCR y TestAc+

```{r}
df$CASOS <- (df$`PCR+` + df$`TestAc+`)
```

3.2. - Agrupación de datos por CCAAs para obtener casos totales

```{r}
#los nombres de PCR+ y TestAC+ dan problema si no sustituyo el símbolo +, así que renombramos antes:

names(df)[4]<-"PCRs"
names(df)[5]<-"TestACs"

df_CCAA<-group_by(df, CCAA)
df_CCAA<-summarize(df_CCAA, count=n(),
               CONFIRMADOS=sum(CASOS, na.rm=T),
               PCRs=sum(PCRs, na.rm=T),
               TestAC=sum(TestACs, na.rm=T),
               Hospitalizados=sum(Hospitalizados, na.rm=T),
               UCIs=sum(UCI, na.rm=T),
               Fallecidos=sum(Fallecidos,na.rm = T))

```

Comprobamos resultados, ordenando salida de menor a mayor casos confirmados

```{r}
df_CCAA %>%
  arrange(CONFIRMADOS)
```

3.3. - Salvamos dataset limpio:

```{r}
saveRDS(df_CCAA,'df_CCAA.rds')
```

4. - Comprobamos las caras de Chernoff

```{r}
CARAS<-TeachingDemos::faces(df_CCAA[,3:8])
```

Ahora las observaremos con mas detalles y color

```{r}
library(aplpack)

CCAAfaces <- faces(df_CCAA[,3:8], labels = row.names(df_CCAA), face.type =5)
```

```{r}
row.names(df_CCAA) <- df_CCAA$CCAA
```

5. - Clustering 

5.1. - Clustering con MST

```{r}
porCCAAs <- hclust(dist(df_CCAA,method="euclidian"),method="single")
```

```{r}
plot(porCCAAs)
plot(hclust(dist(df_CCAA,method="euclidian"),method="single"))
```

5.2. - Clustering con KMEANS

```{r}
# Trabajamos con la base de datos df_CCAA
# Primero vamos a escalar los datos para equilibrar los calculos

datos <- as.data.frame(scale(df_CCAA[, 3:8]))

# asignamos una semilla para garantizar la aleatoriedad de la primera asignacion

set.seed(123)

# Creamos los clusteres mediante kmeans

datos.kmeans <- kmeans(datos, centers = 6)
# k=centers

# observamos los resultados y la calidad de los mismos
datos.kmeans
```

```{r}
str(datos.kmeans)
```

5.2.1. - Metricas

```{r}
#Inercia ENTRE grupos:  mayor es mejor
datos.kmeans$betweenss
```

```{r}
#Inercia INTRA grupos:  menor es mejor
datos.kmeans$withinss
```

```{r}
#Inercia total INTRA grupos:  menor es mejor
datos.kmeans$tot.withinss
```

```{r}
#Representamos los resultados

comunidades<-df_CCAA$CCAA
grupo<-datos.kmeans$cluster
datosc<-data.frame(comunidades, grupo)
```

```{r}
ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=comunidades),color=grupo, size=5)
```

```{r}
ggplot(datosc)+
 geom_point(mapping=aes(x=comunidades, y=grupo),color=grupo)
```