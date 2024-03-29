library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(eph)

ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes_manual.xlsx", 
                              sheet = "indec_2016_100")


names(ipc_series_ctes)[1] <- "periodo"


ipc_series_ctes <- ipc_series_ctes %>% 
  mutate(ano4 = year(periodo),
         mes = month(periodo)) %>% 
  mutate(trimestre = case_when(mes %in% c(1:3) ~ 1,
                               mes %in% c(4:6) ~ 2,
                               mes %in% c(7:9) ~ 3,
                               mes %in% c(10:12) ~ 4)) %>% 
  select(-mes,-periodo) %>% 
  gather(.,key = "region", value = "ipc", -ano4, -trimestre) %>% 
  group_by(ano4,trimestre,region) %>%
  summarise(ipc_trim = mean(ipc)) %>% 
  mutate(region = case_when(region == "Región GBA" ~ 1,
                            region == "Región Cuyo"  ~ 42,
                            region ==  "Región Noreste"  ~ 41,
                            region ==  "Región Noroeste" ~ 40,
                            region ==  "Región Pampeana" ~ 43,
                            region == "Región Patagonia" ~ 44))

write.xlsx(ipc_series_ctes, "preprocesamiento/ipc_aux.xlsx")

####cambio de base

####actualizar a la base deseada:

ano4_base <- 2023

trimestre_base <- 3

texto <- "3er trimestre 2023"

##construyo 2016 2 y 3 a mano (se publicaba únicamente GBA)

aux_2016_2_3 <- data.frame(ano4 = 2016, trimestre = c(2,3), region = 1, ipc_trim = c(90.0123207
,95.4728013))

ipc_series_ctes <- bind_rows(aux_2016_2_3,ipc_series_ctes)
  

ipc_cambia_base <- ipc_series_ctes %>% 
  group_by(region) %>% 
  mutate(ipc_100 = case_when(ano4 == ano4_base & trimestre == trimestre_base ~ 100,
                             TRUE ~ ipc_trim*100/ipc_trim[ano4 == ano4_base & trimestre == trimestre_base])) %>% 
  mutate(inflador = (1/ipc_100)*100)

#expando 2016 2 y 3

aux_2016_2_3 <- ipc_cambia_base %>% 
  filter(ano4 == 2016, trimestre %in% c(2,3))

aux_2016_2_3_f <- aux_2016_2_3

for (r in c(40,41,42,43,44)) {
  
  aux <- aux_2016_2_3
  
  aux$region <- r
  
  aux_2016_2_3_f <- bind_rows(aux_2016_2_3_f,aux)
  
}

aux_2016_2_3_f <- aux_2016_2_3_f %>% filter(region!=1)

#lo agrego 

ipc_cambia_base <- bind_rows(aux_2016_2_3_f, ipc_cambia_base)

ipc_cambia_base <- ipc_cambia_base %>% 
  mutate(nombre_trim_base = texto)

ipc_cambia_base <- ipc_cambia_base %>% 
  rename("ANO4" = "ano4",
         "TRIMESTRE" = "trimestre",
         "REGION" = "region") %>% 
  select(-ipc_trim) %>% 
  ungroup()

write.xlsx(ipc_cambia_base, "preprocesamiento/fuentes/ipc_series_ctes.xlsx")
