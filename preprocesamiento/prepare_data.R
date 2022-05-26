library(tidyverse)
library(readxl)
library(janitor)
library(spatstat)

#GUARDA: REINICIAR
# Todo el preprocesamiento de datos que querramos hacer offline. 

# source('preprocesamiento/levantar_EPH.R')
# source('preprocesamiento/funciones_procesamientos.R')
# source('preprocesamiento/genero_tablas.R')

#problemas con tildes, usar en vez: 

eval(parse('preprocesamiento/levantar_EPH.R', encoding="UTF-8"))
eval(parse('preprocesamiento/funciones_procesamientos.R', encoding="UTF-8"))
eval(parse('preprocesamiento/genero_tablas.R', encoding="UTF-8"))


#saveRDS(tabla_resultados,"tabla_resultados.RDS")

#problemas con tildes guardando el .Rdata, alguna idea para solucionarlo?

#save.image('data.Rdata')
