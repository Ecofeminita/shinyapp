library(tidyverse)
library(httr)
library(stringr)
library(haven)
library(readxl)
library(foreign)
library(spatstat)

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
    gather(indicador, valor, 5:ncol(.))
  
  return(tabla)
  
}

# Función de tasa de no registro (ocupades asalariades)
tasas_no_registro <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO==1,       # Ocupades
           CAT_OCUP==3) %>% # Asalariades
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise("Proporción de no Registrados" = round(sum(PONDERA[PP07H==2])/sum(PONDERA)*100, 1))
  
  return(tabla)
  
}

# Función de jerarquías en cada sexo (ocupades con jerarquía válida)
sexo_segun_jerarquias <- function(base){
  
  tabla <- base %>% 
    filter(JERARQUIA != "0", # Jerarquia valida
           ESTADO == 1) %>%  # Ocupades
    group_by(Sexo) %>% 
    mutate(Frecuencia = sum(PONDERA)) %>% 
    group_by(ANO4, TRIMESTRE, Sexo, JERARQUIA) %>% 
    summarise(tasa = round(sum(PONDERA)/unique(Frecuencia)*100, 1))
  
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

# Función de brecha del ingreso total individual (perceptores)
brecha_ITI <- function(base){
  
  tabla <- base %>% 
    filter(P47T > 0) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.ITI = round(weighted.mean(P47T, PONDII), 2)) %>% ##############
  spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.ITI)
  
  return(tabla)
  
}

# Función de brecha del ingreso de la ocupación principal (ocupades)
brecha_IOP <- function(base){
  
  tabla <- base %>% 
    filter(ESTADO == 1) %>% 
    group_by(ANO4, TRIMESTRE, Sexo) %>% 
    summarise(Media.IOP = round(weighted.mean(P21, PONDIIO), 2)) %>%
    spread(., Sexo, Media.IOP) %>% 
    mutate(brecha.IOP = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP)
  
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
    mutate(brecha.IOP.nr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nr)
  
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
    mutate(brecha.IOP.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.calif)
  
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
    mutate(brecha.IOP.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.nivel.educ)
  
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
    summarise(Media.hs.ocup.princ = round(weighted.mean(PP3E_TOT, PONDERA), 2),
              Media.hs.total.ocup = round(weighted.mean(hs.total.ocup, PONDERA), 2))
  
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
    mutate(brecha.IOP.hr = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr)
  
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
    mutate(brecha.IOP.hr.calif = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, CALIFICACION, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.calif)
  
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
    mutate(brecha.IOP.hr.nivel.educ = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(ANO4, TRIMESTRE, NIVEL_EDUCATIVO, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP.hr.nivel.educ)
  
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

# Función de la composición según sexo de los deciles de ingresos totales individuales (perceptores)
deciles_ITI_sexo <- function(base){
  
  tabla <- base %>% 
    select(ANO4, TRIMESTRE, DECINDR, P47T, PONDII, Sexo) %>% 
    filter(DECINDR %in% c(1:10)) %>% 
    group_by(DECINDR) %>% 
    mutate(Pob = sum(PONDII)) %>% 
    group_by(ANO4, TRIMESTRE, DECINDR, Sexo) %>%
    summarise(Prop = round(sum(PONDII)/unique(Pob)*100, 1))
  
  return(tabla)
  
}

# Función de la composición según sexo de los deciles del ingreso per cápita familiar (total de la población)
deciles_IPCF_sexo <- function(base){
  
  tabla <- base %>% 
    select(ANO4, TRIMESTRE, DECCFR, IPCF, PONDIH, Sexo) %>% 
    filter(DECCFR %in% c(1:10)) %>% 
    group_by(DECCFR) %>% 
    mutate(Pob = sum(PONDIH)) %>% 
    group_by(ANO4, TRIMESTRE, DECCFR, Sexo) %>%
    summarise(Prop = round(sum(PONDIH)/unique(Pob)*100, 1))
  
  return(tabla)
  
}


