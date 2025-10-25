library(tidyverse)
library(httr)
library(stringr)
library(haven)
library(readxl)
library(foreign)
library(spatstat)

ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes.xlsx")

# Función de tasas por sexo (14 años y más)
tasas_por_sexo <- function(base){
  
  tabla <- base %>% 
    filter(CH06 >= 14) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Poblacion         = sum(PONDERA),
              Ocupados          = sum(PONDERA[ESTADO == 1]),
              Desocupados       = sum(PONDERA[ESTADO == 2]),
              PEA               = Ocupados + Desocupados,
              Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
              Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
              Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
              Subocupados       = Suboc_demandante + Suboc_no_demand,
              'Tasa de Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa de Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa de Desocupación'               = round(Desocupados/PEA*100, 1),
              'Tasa de Ocupados Demandantes'       = round(Ocupados_demand/PEA*100, 1),
              'Tasa de Subocupación'               = round(Subocupados/PEA*100, 1),
              'Tasa de Subocupación demandante'    = round(Suboc_demandante/PEA*100, 1),
              'Tasa de Subocupación no demandante' = round(Suboc_no_demand/PEA*100, 1)) %>% 
    gather(indicador, valor, 4:ncol(.))
  
  return(tabla)
  
}

# Función de tasas por sexo y grupos de edad (14 años y más)
tasas_por_sexo_edad <- function(base){
  
  tabla <- base %>% 
    filter(CH06 >= 14) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, GRUPO_EDAD) %>% 
    summarise(Poblacion         = sum(PONDERA),
              Ocupados          = sum(PONDERA[ESTADO == 1]),
              Desocupados       = sum(PONDERA[ESTADO == 2]),
              PEA               = Ocupados + Desocupados,
              Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
              Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
              Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
              Subocupados       = Suboc_demandante + Suboc_no_demand,
              'Tasa de Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa de Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa de Desocupación'               = round(Desocupados/PEA*100, 1),
              'Tasa de Subocupación'               = round(Subocupados/PEA*100, 1)) %>% 
    gather(indicador, valor, 5:ncol(.)) %>% 
    filter(!is.na(GRUPO_EDAD))
  
  return(tabla)
  
}

# Función de tasa de no registro (ocupades asalariades)
tasas_no_registro <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO==1,       # Ocupades
           CAT_OCUP==3) %>% # Asalariades
    filter(!is.na(PP07H)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise("Proporción de no Registrados" = round(sum(PONDERA[PP07H==2])/sum(PONDERA)*100, 1))
  
  return(tabla)
  
}


# Función de jerarquías en cada sexo (ocupades con jerarquía válida)
# sexo_segun_jerarquias <- function(base){
#   
#   tabla <- base %>% 
#     filter(JERARQUIA != "0", # Jerarquia valida
#            ESTADO == 1) %>%  # Ocupades
#     group_by(Sexo) %>% 
#     mutate(Frecuencia = sum(PONDERA)) %>% 
#     group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
#     summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
#   
#   return(tabla)
#   
# }

sexo_segun_jerarquias <- function(bases){
  
  tabla <- bases %>% 
    filter(JERARQUIA != "0", # Jerarquia valida
           ESTADO == 1) %>%  # Ocupades
    group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
    summarise(Frecuencia = sum(PONDERA)) %>% 
    ungroup() %>% 
    group_by(ANO4, TRIMESTRE,Sexo) %>% 
    mutate(tasa = round(Frecuencia/sum(Frecuencia)*100, 1)) %>% 
    select(-Frecuencia)
  
  return(tabla)
  
}

# Función de sexo en cada jerarquía (ocupades con jerarquía válida)
jerarquias_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(JERARQUIA != "0", # Jerarquia valida
           ESTADO == 1) %>%  # Ocupades
    group_by(JERARQUIA) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}


rama_ocupacion <- function(base){
  
  tabla <- organize_caes(base) %>% 
    mutate(Rama = caes_eph_label) %>% 
    filter(ESTADO == 1,
           PP3E_TOT > 0, # Horas trabajadas positivas
           PP3E_TOT != 999,
           P21 > 0,
           PONDIIO > 0) %>%  # Ingresos positivos
    mutate(PP3E_TOT = as.numeric(gsub(",", #cambio "." por "comas"," en los decimales para poder operar
                                      ".",
                                      PP3E_TOT,
                                      fixed = TRUE))) %>% 
    filter(!is.na(Rama)) %>% 
    group_by(ANO4, TRIMESTRE,Rama) %>% 
    summarise(tasa_feminizacion = (sum(PONDERA[Sexo == "Mujeres"])/sum(PONDERA))*100,
              ingreso_promedio = round(weighted.mean(P21, PONDIIO/sum(PONDIIO)), 2),
              ingreso_hor = round(weighted.mean(P21/(PP3E_TOT * 30 / 7), PONDIIO), 2),
              trabajadoras_totales = sum(PONDERA[Sexo == "Mujeres"])) %>% 
    ungroup() %>% 
    group_by(ANO4, TRIMESTRE) %>% 
    mutate("Proporción del empleo femenino total" = (trabajadoras_totales/sum(trabajadoras_totales))*100)
  
  tabla_reg <- organize_caes(base) %>% 
    mutate(Rama = caes_eph_label) %>% 
    filter(ESTADO == 1,
           PP3E_TOT > 0, # Horas trabajadas positivas
           PP3E_TOT != 999,
           P21 > 0,
           PONDIIO > 0) %>%  # Ingresos positivos
    mutate(PP3E_TOT = as.numeric(gsub(",", #cambio "." por "comas"," en los decimales para poder operar
                                      ".",
                                      PP3E_TOT,
                                      fixed = TRUE))) %>% 
    filter(!is.na(Rama)) %>% 
    group_by(ANO4, TRIMESTRE,Rama,REGION) %>% 
    summarise(ingreso_promedio = round(weighted.mean(P21, PONDIIO/sum(PONDIIO)), 2),
              ingreso_hor = round(weighted.mean(P21/(PP3E_TOT * 30 / 7), PONDIIO), 2)) %>% 
    ungroup() %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_ingreso_promedio = ingreso_promedio*inflador,
           cte_ingreso_hor = ingreso_hor*inflador) %>% 
    group_by(ANO4, TRIMESTRE,Rama) %>% 
    summarise(cte_ingreso_promedio = mean(cte_ingreso_promedio),
              cte_ingreso_hor = mean(cte_ingreso_hor))
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE","Rama")) %>% 
    rename("Rama de la ocupación" = "Rama",
           "Tasa de feminización" = "tasa_feminizacion",
           "Ingreso mensual promedio" = "ingreso_promedio",
           "Ingreso horario" = "ingreso_hor",
           "Ingreso mensual promedio (constante)" ="cte_ingreso_promedio",
           "Ingreso horario (constante)" = "cte_ingreso_hor")
  
  
  
  return(tabla)
  
}


# Función de brecha del ingreso total individual (perceptores)
brecha_ITI <- function(base){
  
  tabla <- base %>% 
    filter(P47T > 0) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.ITI = round(weighted.mean(P47T, PONDII/sum(PONDII)), 2)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.ITI_corrientes)
  
  tabla_reg <- base %>% 
    filter(P47T > 0) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, REGION) %>% 
    mutate(i_inflado = P47T*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>%
    summarise(Media.ITI = round(weighted.mean(i_inflado, PONDII), 2)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,brecha.ITI)

  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE"))
  
  return(tabla)
  
}



# Función de brecha del ingreso de la ocupación principal (ocupades)
brecha_IOP <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP) %>% 
    mutate(brecha.IOP_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP_corrientes)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1) %>%
    group_by(ANO4, TRIMESTRE, Sexo, REGION) %>% 
    summarise(Media.ITI = round(weighted.mean(P21, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,brecha.IOP)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE"))
  
  
  return(tabla)  
  
}

# Función de brecha del ingreso de la ocupación principal (asalariades no registrades)
brecha_IOP_no_reg <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1 &      # Ocupades
             CAT_OCUP == 3 &  # Asalariades
             PP07H == 2) %>%  # No registrades
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP.nr = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.nr) %>% 
    mutate(brecha.IOP.nr_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nr_corrientes)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1 &      # Ocupades
             CAT_OCUP == 3 &  # Asalariades
             PP07H == 2) %>%  # No registrades
    group_by(ANO4, TRIMESTRE, Sexo, REGION) %>% 
    summarise(Media.ITI = round(weighted.mean(P21, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.nr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,brecha.IOP.nr)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE"))
  
  return(tabla)  
  
}

# Función de calificación en cada sexo (ocupades con calificación válida)
sexo_segun_calif <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0") %>% 
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de sexo en cada calificación (ocupades con calificación válida)
calif_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0") %>% 
    group_by(CALIFICACION) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de brecha del ingreso de la ocupación principal por calificación (ocupades con calificación válida)
brecha_IOP_calif <- function(base){
  
  tabla <- base %>% 
    filter(CALIFICACION!="0", # Calificacion valida
           ESTADO == 1) %>%   # Ocupades
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(Media.IOP.calif  = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.calif) %>% 
    mutate(brecha.IOP.calif_corriente = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.calif_corriente)
  
  
  tabla_reg <- base %>% 
    filter(CALIFICACION!="0", # Calificacion valida
           ESTADO == 1) %>%   # Ocupades
    group_by(ANO4, TRIMESTRE, Sexo, REGION, CALIFICACION) %>% 
    summarise(Media.ITI = round(weighted.mean(P21, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,CALIFICACION,brecha.IOP.calif)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE","CALIFICACION"))
  
  return(tabla)  
  
}


# Función de nivel educativo en cada sexo (ocupades con nivel educativo válido)
sexo_segun_nivel_educ <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función de sexo en cada nivel educativo (ocupades con nivel educativo válido)
nivel_educ_segun_sexo <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(NIVEL_EDUCATIVO) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
  return(tabla)
  
}

# Función del brecha del ingreso de la ocupación principal por nivel educativo (ocupades con nivel educativo válido)
brecha_IOP_nivel_educ <- function(base){
  
  tabla <- base %>%
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>%
    summarise(Media.IOP.nivel.educ  = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.nivel.educ) %>% 
    mutate(brecha.IOP.nivel.educ_corr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nivel.educ_corr)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1,                 # Ocupades
           !is.na(NIVEL_EDUCATIVO)) %>% # Nivel educativo valido
    group_by(ANO4, TRIMESTRE, Sexo, REGION, NIVEL_EDUCATIVO) %>% 
    summarise(Media.ITI = round(weighted.mean(P21, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>%
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,NIVEL_EDUCATIVO,brecha.IOP.nivel.educ)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE","NIVEL_EDUCATIVO"))
  
  return(tabla)  
  
}

# Función de horas (ocupades con horas positivas)
horas_semanales <- function(base){
  
  tabla <- base %>%
    filter(ESTADO == 1,
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>%                      # Filtro los NAs en las horas de ocupacion principal
    mutate(PP3F_TOT = as.double(PP3F_TOT),           
           PP3F_TOT = case_when(PP3F_TOT == 999 ~ 0, # Cambio los NAs en las horas de ocup sec para que sumen 0
                                TRUE ~ PP3F_TOT),
           hs.total.ocup = PP3E_TOT + PP3F_TOT) %>%  # Sumo las horas de la ocup princ y de ocup sec
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.hs.ocup.princ = round(weighted.mean(PP3E_TOT, PONDERA,na.rm=T), 2),
              Media.hs.total.ocup = round(weighted.mean(hs.total.ocup, PONDERA,na.rm=T), 2))
  
  return(tabla)
  
}


# Función de brecha del INGHORA de la ocupación principal (ocupades con horas positivas)
brecha_IOP_hr <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP.hr = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.hr) %>% 
    mutate(brecha.IOP.hr_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr_corrientes)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1,
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, REGION) %>% 
    summarise(Media.ITI = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.hr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE,brecha.IOP.hr, cte_media.mujeres = Mujeres, cte_media.varones = Varones)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE"))
  
  return(tabla)  
  
}

# Función de brecha del INGHORA de la ocupación principal por calificación (ocupades con calificación válida 
# y horas positivas)
brecha_IOP_hr_calif <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0",
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>%
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(Media.IOP.hr.calif  = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP.hr.calif) %>% 
    mutate(brecha.IOP.hr.calif_corr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.calif_corr)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0",
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, REGION, CALIFICACION) %>% 
    summarise(Media.ITI = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.hr.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,CALIFICACION,brecha.IOP.hr.calif)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE","CALIFICACION"))
  
  return(tabla)
  
}

# Función de brecha del INGHORA de la ocupación principal por nivel educativo (ocupades con nivel educativo válido
# y horas positivas)
brecha_IOP_hr_nivel_educ <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           !is.na(NIVEL_EDUCATIVO),
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(Media.IOP.hr.nivel.educ = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>% 
    spread(., Sexo, Media.IOP.hr.nivel.educ) %>% 
    mutate(brecha.IOP.hr.nivel.educ_corr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.nivel.educ_corr)
  
  tabla_reg <- base %>% 
    filter(ESTADO == 1,
           !is.na(NIVEL_EDUCATIVO),
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(IOP_hr = round(P21/(PP3E_TOT * 30 / 7), 2)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, REGION, NIVEL_EDUCATIVO) %>% 
    summarise(Media.ITI = round(weighted.mean(IOP_hr, PONDIIO), 2)) %>% 
    left_join(.,ipc_series_ctes, by = c("ANO4", "TRIMESTRE", "REGION")) %>% 
    mutate(cte_Media.ITI = Media.ITI*inflador) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP.hr.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, cte_media.mujeres = Mujeres, cte_media.varones = Varones,NIVEL_EDUCATIVO,brecha.IOP.hr.nivel.educ)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("ANO4","TRIMESTRE","NIVEL_EDUCATIVO"))
  
  return(tabla)
  
}
#Función de horas trabajadas por calificación y nivel educativo


OP_hr_calif <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           CALIFICACION!="0",
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(PP3F_TOT = as.double(PP3F_TOT),           
           PP3F_TOT = case_when(PP3F_TOT == 999 ~ 0, # Cambio los NAs en las horas de ocup sec para que sumen 0
                                TRUE ~ PP3F_TOT),
           hs.total.ocup = PP3E_TOT + PP3F_TOT) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, CALIFICACION) %>% 
    summarise(Media.hs.ocup.princ = round(weighted.mean(PP3E_TOT, PONDERA,na.rm=T), 2),
              Media.hs.total.ocup = round(weighted.mean(hs.total.ocup, PONDERA,na.rm=T), 2))
  
  return(tabla)
  
}

OP_hr_nivel_educ <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           !is.na(NIVEL_EDUCATIVO),
           PP3E_TOT > 0,
           PP3E_TOT != 999) %>% 
    mutate(PP3F_TOT = as.double(PP3F_TOT),           
           PP3F_TOT = case_when(PP3F_TOT == 999 ~ 0, # Cambio los NAs en las horas de ocup sec para que sumen 0
                                TRUE ~ PP3F_TOT),
           hs.total.ocup = PP3E_TOT + PP3F_TOT) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, NIVEL_EDUCATIVO) %>% 
    summarise(Media.hs.ocup.princ = round(weighted.mean(PP3E_TOT, PONDERA,na.rm=T), 2),
              Media.hs.total.ocup = round(weighted.mean(hs.total.ocup, PONDERA,na.rm=T), 2))
  
  return(tabla)
  
}





# Función de proporción de cada sexo entre las personas que realizan las tareas domésticas del hogar
tareas_domesticas_sexo <- function(base, base_hogar){
  
  tabla <- base %>% 
    left_join(., base_hogar %>% select(CODUSU, NRO_HOGAR, VII1_1, VII1_2), by = c("CODUSU", "NRO_HOGAR")) %>% 
    mutate(proporcion = case_when(VII1_1 == COMPONENTE | VII1_2 == COMPONENTE ~ 1,
                                  TRUE ~ 0)) %>% 
    select(ANO4, TRIMESTRE, Sexo, proporcion, PONDERA) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(proporcion = sum(proporcion*PONDERA)) %>% 
    mutate(proporcion = round(proporcion/sum(proporcion)*100, 0))
  
  return(tabla)
  
}

# Función de proporción de cada sexo entre quienes se dedican al servicio doméstico
servicio_domestico_sexo <- function(base){
  
  tabla <- base %>%
    filter(PP04B1 == 1) %>% 
    mutate(Total = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Proporcion = round(sum(PONDERA)/unique(Total)*100, 1))
  
  return(tabla)
  
}

# Función de proporción de mujeres que se dedican al servicio doméstico entre el total de ocupadas
servicio_domestico_ocupadas <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1,
           Sexo == "Mujeres") %>% 
    mutate(servicio.domestico = case_when(PP04B1 == 1 ~ "Sí",
                                          PP04B1 != 1 ~ "No")) %>% 
    group_by(ANO4, TRIMESTRE, servicio.domestico) %>% 
    summarise(frecuencia = sum(PONDERA)) %>% 
    mutate(proporcion = round(frecuencia/sum(frecuencia)*100, 0))
  
  return(tabla)
  
}


derechos_servicio_domestico <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1, #Trabajadoras domésticas del servicio doméstico
           Sexo == "Mujeres",
           PP04B1 == 1) %>% 
    dplyr::group_by(ANO4, TRIMESTRE) %>% 
    summarise("No tiene descuento jubilatorio" = (sum(PONDERA[PP07H==2])/sum(PONDERA[PP07H%in%c(1,2)]))*100,
              "No tiene vacaciones pagas" = (sum(PONDERA[PP07G1 == 2])/sum(PONDERA[PP07G1%in%c(1,2)]))*100,
              "No tiene aguinaldo" = (sum(PONDERA[PP07G2 == 2])/sum(PONDERA[PP07G2%in%c(1,2)]))*100,
              "No tiene días pagos por enfermedad" = (sum(PONDERA[PP07G3 == 2])/sum(PONDERA[PP07G3%in%c(1,2)]))*100,
              "No tiene obra social" = (sum(PONDERA[PP07G4 == 2])/sum(PONDERA[PP07G4%in%c(1,2)]))*100
              ) %>% 
    dplyr::ungroup() %>% 
    gather(., key = "indicador", value = "valor", -ANO4, -TRIMESTRE)
  
  return(tabla)
  
}

# Función de la composición según sexo de los deciles de ingresos totales individuales (perceptores)
deciles_ITI_sexo <- function(base){

  tabla <- base %>%
    select(ANO4, TRIMESTRE, DECINDR, P47T, PONDII, Sexo) %>%
    mutate(DECINDR = as.numeric(DECINDR)) %>% 
    filter(DECINDR %in% c(1:10)) %>%
    group_by(ANO4, TRIMESTRE,DECINDR, Sexo) %>%
    summarise(Pob = sum(PONDII)) %>%
    group_by(ANO4, TRIMESTRE, DECINDR) %>%
    mutate(Prop = round(Pob/sum(Pob)*100, 1))%>% 
    mutate(DECINDR = factor(DECINDR, levels = c("1","2","3","4","5","6","7","8","9","10")))

  return(tabla)

}




#distribución de las personas de cada sexo en cada decil
# deciles_ITI_sexo <- function(base){
#   
#   tabla <- base %>% 
#     select(ANO4, TRIMESTRE, DECINDR, P47T, PONDII, Sexo) %>% 
#     filter(DECINDR %in% c(1:10)) %>% 
#     group_by(ANO4, TRIMESTRE, Sexo, DECINDR) %>% 
#     summarise(Pob = sum(PONDII)) %>% 
#     group_by(ANO4, TRIMESTRE, Sexo) %>%
#     mutate(Prop = round((Pob/sum(Pob))*100, 1)) %>% 
#     mutate(DECINDR = factor(DECINDR, levels = c("10","9","8","7","6","5","4","3","2","1")))
#   
#   return(tabla)
#   
# }


# Función de la composición según sexo de los deciles del ingreso per cápita familiar (total de la población)
deciles_IPCF_sexo <- function(base){
  
  tabla <- base %>%
    select(ANO4, TRIMESTRE, DECCFR, IPCF, PONDIH, Sexo) %>%
    mutate(DECCFR = as.numeric(DECCFR)) %>% 
    filter(DECCFR %in% c(1:10)) %>%
    group_by(ANO4, TRIMESTRE,DECCFR, Sexo) %>%
    summarise(Pob = sum(PONDIH)) %>%
    group_by(ANO4, TRIMESTRE, DECCFR) %>%
    mutate(Prop = round(Pob/sum(Pob)*100, 1))%>% 
    mutate(DECCFR = factor(DECCFR, levels = c("1","2","3","4","5","6","7","8","9","10")))
  
  return(tabla)
  
}


#distribución de las personas de cada sexo en cada decil
# deciles_IPCF_sexo <- function(base){
#   
#   tabla <- base %>% 
#     select(ANO4, TRIMESTRE, DECCFR, IPCF, PONDIH, Sexo) %>% 
#     filter(DECCFR %in% c(1:10)) %>% 
#     group_by(ANO4, TRIMESTRE, Sexo, DECCFR) %>% 
#     summarise(Pob = sum(PONDIH)) %>% 
#     group_by(ANO4, TRIMESTRE, Sexo) %>%
#     mutate(Prop = round((Pob/sum(Pob))*100, 1))%>% 
#     mutate(DECCFR = factor(DECCFR, levels = c("10","9","8","7","6","5","4","3","2","1")))
#   
#   return(tabla)
#   
# }

