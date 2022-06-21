

#Hola! Acá levantamos dfs que vayamos a usar en varios módulos o funciones que necesitemos en varios módulos.
#De esta forma, los levanta una sola vez cuando empieza a correr la app


#Objects defined in global.R are similar to those defined in app.R outside of the server function definition, with one important difference: they are loaded into the global environment of the R session; all R code in a Shiny app is run in the global environment or a child of it.


############### General ##################

library(tidyverse)
library(readxl)
library(openxlsx)

tabla_resultados <- readRDS("www/tabla_resultados.RDS")

trimestres <- tabla_resultados[["tasas_por_sexo_df"]] %>% ungroup() %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  select(periodo) %>% unique() %>% pull(periodo)

tabla_metadata <- read_excel("www/metadata.xlsx") %>% select(indicador, metadata)

###Inflación#####




ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes.xlsx")

nombre_trimestre_base <- unique(ipc_series_ctes$nombre_trim_base[!is.na(ipc_series_ctes$nombre_trim_base)])


###################Secciones#####################################


####Tipo de inserción##############

t3_acomodo <- tabla_resultados[["tasas_no_registro_df"]] %>% 
  mutate(JERARQUIA = "Trabajadores Asalariados")

tabla_tipo_insercion <- tabla_resultados[["sexo_segun_jerarquias_df"]] %>% 
  left_join(.,t3_acomodo, by = c("ANO4", "TRIMESTRE","Sexo", "JERARQUIA"))

tabla_tipo_insercion_asal <- tabla_tipo_insercion %>% 
  filter(JERARQUIA == "Trabajadores Asalariados") %>% 
  mutate("Trabajadores Asalariados No Registrados" = round(tasa*(`Proporción de no Registrados`)/100,1),
         "Trabajadores Asalariados Registrados" = round(tasa*(100-`Proporción de no Registrados`)/100,1)) %>% 
  select(-`Proporción de no Registrados`,-"JERARQUIA",-tasa) %>% 
  gather(., key ="JERARQUIA", value = "tasa",-Sexo,-ANO4,-TRIMESTRE)

tabla_tipo_insercion <- bind_rows((tabla_tipo_insercion %>% select(-`Proporción de no Registrados`)),tabla_tipo_insercion_asal) %>%
  filter(JERARQUIA != "Trabajadores Asalariados")

jerarqs <- tabla_tipo_insercion %>% ungroup() %>%  select(JERARQUIA) %>% unique()

jerarqs <- jerarqs$JERARQUIA

####Tasas sexo##############

tasas <- tabla_resultados[["tasas_por_sexo_df"]]$indicador %>% unique()

tasas <- tasas[grepl("Tasa",tasas)]

####Tasas sexo edad##############

tasas_edad <- tabla_resultados[["tasas_por_sexo_edad_df"]]$indicador %>% unique()

tasas_edad <- tasas_edad[grepl("Tasa",tasas_edad)]

grupos_edad <- (tabla_resultados[["tasas_por_sexo_edad_df"]] %>% drop_na())$GRUPO_EDAD %>% unique()

####Ramas##############

ramas <- tabla_resultados[["ramas_sexo_df"]] %>% ungroup() %>%  select(`Rama de la ocupación`) %>% unique() %>% drop_na()

ramas <- ramas$`Rama de la ocupación`

####Deciles##############

nombres_deciles <- data.frame("tabla" =c("deciles_IPCF_sexo_df",
                                         "deciles_ITI_sexo_df"),
                              
                              "cod" =c("DECCFR",
                                       "DECINDR"),
                              
                              "nombre"= c("Ingreso per cápita Familiar",
                                          "Ingreso Total Individual"
                              ))

ingresos <- nombres_deciles %>% pull(nombre)

####Brechas##############

#respetar orden!!!!!
nombres_brechas <- data.frame("tabla" =c("brecha_ITI_df",
                                         "brecha_IOP_df",
                                         "brecha_IOP_hr_df",
                                         "brecha_IOP_no_reg_df"),
                              
                              "cod" =c("brecha.ITI",
                                       "brecha.IOP",
                                       "brecha.IOP.hr",
                                       "brecha.IOP.nr"),
                              
                              "nombre"= c("Ingreso Total Individual",
                                          "Ingreso mensual de la Ocupación Principal",
                                          "Ingreso horario de la Ocupación Principal",
                                          "Ingreso de la Ocupación Principal - Asalariadas/os sin desc. jubil"))

####Brechas desagregadas##############


#respetar orden!!!!!
nombres_brechas_desag <- data.frame("tabla" =c("brecha_IOP_calif_df",
                                               "brecha_IOP_hr_calif_df",
                                               "brecha_IOP_nivel_educ_df",
                                               "brecha_IOP_hr_nivel_educ_df"),
                                    
                                    "cod" =c("brecha.IOP.calif",
                                             "brecha.IOP.hr.calif",
                                             "brecha.IOP.nivel.educ",
                                             "brecha.IOP.hr.nivel.educ"),
                                    
                                    "nombre"= c("Ingreso mensual de la Ocupación Principal",
                                                "Ingreso horario de la Ocupación Principal",
                                                "Ingreso mensual de la Ocupación Principal",
                                                "Ingreso horario de la Ocupación Principal"),
                                    
                                    "variable_desag" = c("CALIFICACION", "CALIFICACION", "NIVEL_EDUCATIVO","NIVEL_EDUCATIVO"),
                                    "variable_desag_nombre" = c("Calificación", "Calificación", "Nivel educativo","Nivel educativo"))


v1 <- as.character(unique(tabla_resultados[["brecha_IOP_calif_df"]]$CALIFICACION))
v2 <- as.character(unique(tabla_resultados[["brecha_IOP_nivel_educ_df"]]$NIVEL_EDUCATIVO))


opciones_actualizacion<- data.frame("Calificación" = v1,"Nivel educativo" = v2, "id" = c(1,2,3,4)
) %>% 
  pivot_longer(!id,names_to = "variable", values_to = "valores")

opciones_actualizacion$variable[opciones_actualizacion$variable =="Nivel.educativo"] <- "Nivel educativo"


####Trabajo remunerado##############

vector_horas <- c("Ocupación principal","Totales -todas las ocupaciones-")


#respetar orden!!!!!
nombres_horas_remuneradas <- data.frame("tabla" =c("OP_hr_calif_df",
                                                   "OP_hr_nivel_educ_df"),
                                        
                                        "variable_desag" = c("CALIFICACION", "NIVEL_EDUCATIVO"),
                                        "variable_desag_nombre" = c("Calificación", "Nivel educativo"),
                                        
                                        "cod" =c("Media.hs.ocup.princ",
                                                 "Media.hs.total.ocup"),
                                        
                                        "nombre"= c(vector_horas[1],
                                                    vector_horas[2]))


####Derechos laborales de las trabajadoras de servicio doméstico##############

derechos <- unique(tabla_resultados$derechos_servicio_domestico_df$indicador) 

