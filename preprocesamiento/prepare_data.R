library(tidyverse)
library(readxl)
library(janitor)
library(spatstat)

#GUARDA: REINICIAR
# Todo el preprocesamiento de datos que querramos hacer offline. 


#problemas con tildes, usar en vez: 

eval(parse('preprocesamiento/levantar_EPH.R', encoding="UTF-8"))
eval(parse('preprocesamiento/funciones_procesamientos.R', encoding="UTF-8"))
eval(parse('preprocesamiento/genero_tablas.R', encoding="UTF-8"))
#eval(parse('preprocesamiento/genero_series_constantes.R', encoding="UTF-8"))

###nombres ramas (tildes y etc)

levels(tabla_resultados[["ramas_sexo_df"]]$"Rama de la ocupación")[levels(tabla_resultados[["ramas_sexo_df"]]$"Rama de la ocupación") == "Ensenanza"] <- "Enseñanza"

###reacomodos de tasas sexo para mostrar brechas y tasa de no registro

tabla_resultados$tasas_no_registro_df <- tabla_resultados$tasas_no_registro_df %>% 
  rename("Tasa de No Registro" = "Proporción de no Registrados") %>% 
  gather(.,key = indicador, value = valor, -ANO4, -TRIMESTRE, -Sexo)

tabla_resultados[["tasas_por_sexo_df"]] <- bind_rows(tabla_resultados[["tasas_por_sexo_df"]], tabla_resultados$tasas_no_registro_df )

tabla_resultados[["tabla_brechas_tasas"]] <- tabla_resultados[["tasas_por_sexo_df"]] %>% 
  spread(.,key = "Sexo", value = "valor") %>% 
  mutate("Brecha (%)" = round(((Varones-Mujeres)/Varones)*100, 1))

tabla_resultados[["tasas_por_sexo_df"]] <- tabla_resultados[["tasas_por_sexo_df"]] %>% 
  left_join(.,tabla_resultados[["tabla_brechas_tasas"]], by =c("ANO4", "TRIMESTRE","indicador")) %>% 
  select(-Mujeres,-Varones)

saveRDS(tabla_resultados,"www/tabla_resultados.RDS")




