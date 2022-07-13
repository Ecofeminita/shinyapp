############### General ##################

library(tidyverse)
library(readxl)
library(openxlsx)

tabla_resultados <- readRDS("www/tabla_resultados.RDS")


# trimestres <- tabla_resultados[["tasas_por_sexo_df"]] %>% ungroup() %>% 
#   mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
#                           levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
#   select(periodo) %>% unique() %>% pull(periodo)

trimestres <- tabla_resultados[["tasas_por_sexo_df"]] %>% ungroup() %>% 
  mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                          levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
  select(periodo) %>% unique() %>% pull(periodo)

tabla_metadata <- read_excel("www/metadata.xlsx") %>% select(indicador, metadata)

###Inflación#####


ipc_series_ctes <- read_excel("preprocesamiento/fuentes/ipc_series_ctes.xlsx")

nombre_trimestre_base <- unique(ipc_series_ctes$nombre_trim_base[!is.na(ipc_series_ctes$nombre_trim_base)])


###################Secciones#####################################



####Deciles##############

nombres_deciles <- data.frame("tabla" =c("deciles_IPCF_sexo_df",
                                         "deciles_ITI_sexo_df"),
                              
                              "cod" =c("DECCFR",
                                       "DECINDR"),
                              
                              "nombre"= c("Ingreso per cápita Familiar",
                                          "Ingreso Total Individual"
                              ))

ingresos <- nombres_deciles %>% pull(nombre)



###gráfico#####

colores = c("#FE1764", "#00BDD6")

plot <- function(base,
                 tipo_ingreso,
                 periodo_i,
                 periodo_f){
  
  datagraf1 <- base %>%         
    mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                            levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
    rename("Decil" = tipo_ingreso) %>% 
    filter(Sexo == "Mujeres")
  
  
  datagraf2 <- datagraf1%>% 
    filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
  
  grafico <- ggplot(datagraf2, aes(x=periodo, y=Decil, fill=Prop
                                   
                                   
  )) + 
    geom_tile()+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
          legend.position = "right",
          panel.background = element_rect(fill = "gray99", color = "gray90"),
          
          strip.text.y = element_text(angle = 0),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=18),
          text = element_text(size = 15)) +
    scale_fill_gradient(low=colores[2], high=colores[1],
                        labels= function(x) paste0(x,"%")) +
    
    labs(x = "Período",
         y = "Decil",
         title = "Distribución de la población según decil de Ingreso Total Individual, por sexo.",
         subtitle = "Desde 2°T 2016 hasta 4°T 2019. Población perceptora de ingresos.",
         fill=str_wrap("Población femenina", 10))
  
  
  grafico
  
  
}


plot(tabla_resultados[["deciles_ITI_sexo_df"]],"DECINDR","2°T 2016","4°T 2019")


####Ramas##############

ramas <- tabla_resultados[["ramas_sexo_df"]] %>% ungroup() %>%  select(`Rama de la ocupación`) %>% unique() %>% drop_na()

ramas <- ramas$`Rama de la ocupación`

colores <- c("#8cddd3","#8594c6","#fbd17e","#e9c1d0","#e5616e")

plot_i <- function(base,
                   vary,
                   eje_x,
                   valores_filter,
                   periodo_i,
                   periodo_f){
  
  datagraf1 <- base %>%         
    mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                            levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
    rename("ingreso" = vary)
  
  datagraf2 <- datagraf1%>% 
    filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
    filter(`Rama de la ocupación` %in% valores_filter)
  
  
  grafico <- ggplot(datagraf2, aes(x=periodo, y=ingreso, color=`Rama de la ocupación`, size =`Proporción del empleo femenino total`
                                   
                                   
  )) + 
    geom_point(alpha = .6)+
    scale_size(range = c(3, 20))+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.7),
          legend.position = "top",
          #panel.background = element_rect(fill = "gray99", color = "gray90"),
          #plot.background = element_rect(fill="gray99", color = NA),
          strip.text.y = element_text(angle = 0),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(size=22),
          plot.subtitle = element_text(size=18),
          text = element_text(size = 15)) +
    
    labs(x = eje_x,
         y = vary,
         title = "Ingresos por rama de actividad",
         subtitle = "Desde 2°T 2016 hasta 4°T 2019.",
         color = "")+
    scale_color_manual(values = colores)+
    scale_y_continuous(labels = function(x) (paste0("$",x)))+
    guides(size = "none",
           #colour = guide_legend(nrow = 3)
           color = guide_legend(override.aes = list(size=5))
           )
  
  grafico

}


plot_i(tabla_resultados[["ramas_sexo_df"]],
       
       eje_x = "Período",
       vary = "Ingreso mensual promedio",
       valores_filter = c("Servicio domestico", "Construccion", "Industria manufacturera", "Enseñanza", "Comercio"),
       "2°T 2016","4°T 2019")

