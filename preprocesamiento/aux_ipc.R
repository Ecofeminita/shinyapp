library(readxl)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(eph)

ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes.xlsx", 
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
