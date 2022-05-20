library(tidyverse)
library(httr)
library(stringr)


#Armo las tablas (dejo todo comentado menos una para probar)

# Función que crea las tablas
creador_tablas <- function(base#, 
                           #base_hogar,
                           #tareas_domesticas=TRUE
                           ){
  
  tasas_por_sexo_df              <- tasas_por_sexo(base)
  tasas_por_sexo_edad_df         <- tasas_por_sexo_edad(base)
  tasas_no_registro_df           <- tasas_no_registro(base)
  sexo_segun_jerarquias_df       <- sexo_segun_jerarquias(base)
  jerarquias_segun_sexo_df       <- jerarquias_segun_sexo(base)
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
  # if (tareas_domesticas) {
  #   tareas_domesticas_sexo_df  <- tareas_domesticas_sexo(base, base_hogar)
  # }else{
  #   tareas_domesticas_sexo_df  <- tibble()
  # }
  servicio_domestico_sexo_df     <- servicio_domestico_sexo(base)
  servicio_domestico_ocupadas_df <- servicio_domestico_ocupadas(base)
  deciles_ITI_sexo_df            <- deciles_ITI_sexo(base)
  deciles_IPCF_sexo_df           <- deciles_IPCF_sexo(base)
  
  lista <- list("tasas_por_sexo_df" = tasas_por_sexo_df,
                 "tasas_por_sexo_edad_df" = tasas_por_sexo_edad_df,
                 "tasas_no_registro_df" = tasas_no_registro_df,
                 "sexo_segun_jerarquias_df" = sexo_segun_jerarquias_df,
                 "jerarquias_segun_sexo_df" = jerarquias_segun_sexo_df,
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
                # "tareas_domesticas_sexo_df" = tareas_domesticas_sexo_df,
                "servicio_domestico_sexo_df" = servicio_domestico_sexo_df,
                "servicio_domestico_ocupadas_df" = servicio_domestico_ocupadas_df,
                "deciles_ITI_sexo_df" = deciles_ITI_sexo_df,
                "deciles_IPCF_sexo_df" = deciles_IPCF_sexo_df
                
  )
  
  return(lista)
  
}


tabla_resultados <- creador_tablas(bases)

#Al final borro la base cruda y las funciones que ya usé

rm(list=names(Filter(is.function, mget(ls(all=T)))))
rm(bases)


