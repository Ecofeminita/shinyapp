library(tidyverse)
library(httr)
library(stringr)




# Función que crea las tablas
creador_tablas <- function(base, 
                           base_hogar,
                           tareas_domesticas=TRUE
                           ){
  
  tasas_por_sexo_df              <- tasas_por_sexo(base)
  tasas_por_sexo_edad_df         <- tasas_por_sexo_edad(base)
  tasas_no_registro_df           <- tasas_no_registro(base)
  sexo_segun_jerarquias_df       <- sexo_segun_jerarquias(base)
  jerarquias_segun_sexo_df       <- jerarquias_segun_sexo(base)
  ramas_sexo_df                  <- rama_ocupacion(base)
  brecha_ITI_df                  <- brecha_ITI(base)
  brecha_IOP_df                  <- brecha_IOP(base)
  brecha_IOP_no_reg_df           <- brecha_IOP_no_reg(base)
  sexo_segun_calif_df            <- sexo_segun_calif(base)
  calif_segun_sexo_df            <- calif_segun_sexo(base)
  brecha_IOP_calif_df            <- brecha_IOP_calif(base)
  sexo_segun_nivel_educ_df       <- sexo_segun_nivel_educ(base)
  nivel_educ_segun_sexo_df       <- nivel_educ_segun_sexo(base)
  brecha_IOP_nivel_educ_df       <- brecha_IOP_nivel_educ(base)
  horas_semanales_df             <- horas_semanales(base)
  brecha_IOP_hr_df               <- brecha_IOP_hr(base)
  brecha_IOP_hr_calif_df         <- brecha_IOP_hr_calif(base)
  brecha_IOP_hr_nivel_educ_df    <- brecha_IOP_hr_nivel_educ(base)
  
  OP_hr_calif_df         <- OP_hr_calif(base)
  OP_hr_nivel_educ_df    <- OP_hr_nivel_educ(base)
  
  
  
  if (tareas_domesticas) {
    tareas_domesticas_sexo_df  <- tareas_domesticas_sexo(base, base_hogar)
  }else{
    tareas_domesticas_sexo_df  <- tibble()
  }
  servicio_domestico_sexo_df     <- servicio_domestico_sexo(base)
  servicio_domestico_ocupadas_df <- servicio_domestico_ocupadas(base)
  derechos_servicio_domestico_df <- derechos_servicio_domestico(base)
  deciles_ITI_sexo_df            <- deciles_ITI_sexo(base)
  deciles_IPCF_sexo_df           <- deciles_IPCF_sexo(base)
  
  lista <- list("tasas_por_sexo_df" = tasas_por_sexo_df,
                 "tasas_por_sexo_edad_df" = tasas_por_sexo_edad_df,
                 "tasas_no_registro_df" = tasas_no_registro_df,
                 "sexo_segun_jerarquias_df" = sexo_segun_jerarquias_df,
                 "jerarquias_segun_sexo_df" = jerarquias_segun_sexo_df,
                "ramas_sexo_df" = ramas_sexo_df,
                 "brecha_ITI_df" = brecha_ITI_df,
                "brecha_IOP_df" = brecha_IOP_df,
                "brecha_IOP_no_reg_df" = brecha_IOP_no_reg_df,
                "sexo_segun_calif_df" = sexo_segun_calif_df,
                "calif_segun_sexo_df" = calif_segun_sexo_df,
                "brecha_IOP_calif_df" = brecha_IOP_calif_df,
                "sexo_segun_nivel_educ_df" = sexo_segun_nivel_educ_df,
                "nivel_educ_segun_sexo_df" = nivel_educ_segun_sexo_df,
                "brecha_IOP_nivel_educ_df" = brecha_IOP_nivel_educ_df,
                "horas_semanales_df" = horas_semanales_df,
                "brecha_IOP_hr_df" = brecha_IOP_hr_df,
                "brecha_IOP_hr_calif_df" = brecha_IOP_hr_calif_df,
                "brecha_IOP_hr_nivel_educ_df" = brecha_IOP_hr_nivel_educ_df,
                "OP_hr_calif_df" = OP_hr_calif_df,
                "OP_hr_nivel_educ_df" = OP_hr_nivel_educ_df,
                
                 "tareas_domesticas_sexo_df" = tareas_domesticas_sexo_df,
                "servicio_domestico_sexo_df" = servicio_domestico_sexo_df,
                "servicio_domestico_ocupadas_df" = servicio_domestico_ocupadas_df,
                "derechos_servicio_domestico_df" = derechos_servicio_domestico_df,
                "deciles_ITI_sexo_df" = deciles_ITI_sexo_df,
                "deciles_IPCF_sexo_df" = deciles_IPCF_sexo_df
                
  )
  
  return(lista)
  
}


tabla_resultados <- creador_tablas(bases,base_hogar)

#Al final borro la base cruda y las funciones que ya usé

rm(list=names(Filter(is.function, mget(ls(all=T)))))
rm(bases,base_hogar)

#un par de ajustes mas

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
