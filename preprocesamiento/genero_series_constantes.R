
library(readxl)


ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes.xlsx")


dfs_pesos <- c("brecha_ITI_df",
               "brecha_IOP_df",
               "brecha_IOP_no_reg_df",
               "brecha_IOP_hr_df",
               "brecha_IOP_calif_df",
               "brecha_IOP_hr_calif_df",
               "brecha_IOP_nivel_educ_df",
               "brecha_IOP_hr_nivel_educ_df")


for (df in dfs_pesos) {
  
  tabla_inflada <- tabla_resultados[[df]] %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE")) %>% 
    mutate(cte_media.mujeres = media.mujeres*inflador,
           cte_media.varones = media.varones*inflador) %>% 
    select(-c("IPC_base_100", "inflador"))
  
  tabla_resultados[[df]] <- tabla_inflada
}


tabla_resultados[["ramas_sexo_df"]] <- tabla_resultados[["ramas_sexo_df"]]%>% 
  left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE")) %>% 
  mutate(`Ingreso mensual promedio (constante)` = `Ingreso mensual promedio`*inflador,
         `Ingreso horario (constante)` = `Ingreso horario`*inflador) %>% 
  select(-c("IPC_base_100", "inflador"))
