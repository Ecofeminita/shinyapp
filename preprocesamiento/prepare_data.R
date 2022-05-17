library(tidyverse)
library(readxl)
library(janitor)
library(spatstat)

#GUARDA: REINICIAR
# Todo el preprocesamiento de datos que querramos hacer offline. 

source('preprocesamiento/levantar_EPH.R')
source('preprocesamiento/funciones_procesamientos.R')
source('preprocesamiento/genero_tablas.R')

save.image('data.Rdata')
