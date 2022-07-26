library(eph)
library(tidyverse)
library(readxl)
library(janitor)
library(readr)

#Poner acá nuestra limpieza general de eph (cambios de tipo de variable, nombres, etc)

nombres_filtro_personas <- read_excel("preprocesamiento/nombres_filtro.xlsx", 
                             col_names = FALSE) %>% 
  pull("...1")



#Levanto bases

bases <- get_microdata(year = 2016:2019, 
                       trimester = 1:4,
                       type =  'individual',
                       vars = nombres_filtro_personas,
                       destfile = 'preprocesamiento/fuentes/bases_eph.rds')

basesb <- get_microdata(year = 2020:2021, 
                       trimester = 1:4,
                       type =  'individual',
                       vars = nombres_filtro_personas,
                       destfile = 'preprocesamiento/fuentes/bases_eph_b.rds'
                       )



bases <- bases %>% unnest(cols = c(microdata)) %>% select(-wave) 
basesb <- basesb %>% unnest(cols = c(microdata)) %>% select(-wave) 

bases <- bases %>% select(nombres_filtro_personas)
basesb <- basesb %>% select(nombres_filtro_personas)



bases <- bind_rows(bases,basesb)
rm(basesb)
# Función de limpieza de la base individual
limpieza_individuos <- function(base){
  
  individuos <- base %>% 
    mutate(Sexo = as.character(CH04),
           Sexo = case_when(Sexo=="1" ~ "Varones",
                            Sexo=="2" ~ "Mujeres"),
           PP04D_COD = as.character(PP04D_COD),
           PP04D_COD = case_when(nchar(PP04D_COD) == 5 ~ PP04D_COD,
                                 nchar(PP04D_COD) == 4 ~ paste0("0", PP04D_COD),
                                 nchar(PP04D_COD) == 3 ~ paste0("00", PP04D_COD),
                                 nchar(PP04D_COD) == 2 ~ paste0("000", PP04D_COD),
                                 nchar(PP04D_COD) == 1 ~ paste0("0000", PP04D_COD)),
           CALIFICACION = substr(PP04D_COD, 5, 5),
           CALIFICACION = case_when(CALIFICACION=="1" ~ "Profesionales",
                                    CALIFICACION=="2" ~ "Técnicos",
                                    CALIFICACION=="3" ~ "Operativos",
                                    CALIFICACION=="4" ~ "No Calificados",
                                    TRUE ~ "0"),
           CALIFICACION = factor(CALIFICACION, c("Profesionales", "Técnicos", "Operativos", "No Calificados")),
           JERARQUIA = substr(PP04D_COD, 3, 3),
           JERARQUIA = case_when(JERARQUIA=="0" ~ "Dirección",
                                 JERARQUIA=="1" ~ "Cuentapropia",
                                 JERARQUIA=="2" ~ "Jefes",
                                 JERARQUIA=="3" ~ "Trabajadores Asalariados",
                                 TRUE ~ "0"),
           JERARQUIA = factor(JERARQUIA, c("Jefes", "Dirección", "Trabajadores Asalariados", "Cuentapropia")),
           NIVEL_EDUCATIVO = case_when(NIVEL_ED==1 ~ "Sin Instrucción",
                                       NIVEL_ED==2 ~ "Primaria",
                                       NIVEL_ED==3 ~ "Primaria",
                                       NIVEL_ED==4 ~ "Secundaria",
                                       NIVEL_ED==5 ~ "Secundaria",
                                       NIVEL_ED==6 ~ "Superior",
                                       NIVEL_ED==7 ~ "Sin Instrucción",
                                       NIVEL_ED==9 ~ "NS/NR"),
           NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO, levels = c("Sin Instrucción", "Primaria", "Secundaria", "Superior")),
           GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "de 14 a 29 años",
                                  CH06 >= 30 & CH06 <= 64 ~ "de 30 a 64 años"))
  
  return(individuos)  
}


bases <- limpieza_individuos(bases)


###Para más adelante

# Función de recambio de nombres (de version 03-06 a 16)

# limpieza_nombres <- function(base){
#   nombres <- levantar_bases(path = "Fuentes/variables_viejas_a_nuevas.txt")
#   
#   for (i in c(1:nrow(nombres))) {
#     row <- nombres[i,]
#     if (row$old %in% names(base)) {
#       nro_nombre <- which(names(base)==row$old)
#       names(base)[nro_nombre] <- as.character(row$new) 
#     }
#   }
#   return(base)
# }


nombres_filtro_hogares <- read_excel("preprocesamiento/nombres_filtro.xlsx", 
                             sheet = "hogares", col_names = FALSE)%>% 
  pull("...1")


bases_hogar <- get_microdata(year = 2016:2019, 
                       trimester = 1:4,
                       type =  'hogar',
                       vars = nombres_filtro_hogares,
                       destfile = 'preprocesamiento/fuentes/bases_eph_hogar.rds')

basesb_hogar <- get_microdata(year = 2020:2021, 
                        trimester = 1:4,
                        type =  'hogar',
                        vars = nombres_filtro_hogares,
                        destfile = 'preprocesamiento/fuentes/bases_eph_hogar_b.rds')


bases_hogar <- bases_hogar %>% unnest(cols = c(microdata)) %>% select(-wave) 
basesb_hogar <- basesb_hogar %>% unnest(cols = c(microdata)) %>% select(-wave)

bases_hogar <- bases_hogar %>% select(nombres_filtro_hogares)
basesb_hogar <- basesb_hogar %>% select(nombres_filtro_hogares)

base_hogar <- bind_rows(bases_hogar,basesb_hogar)

rm(basesb_hogar)

rm(bases_hogar)

rm(nombres_filtro_hogares,nombres_filtro_personas)
