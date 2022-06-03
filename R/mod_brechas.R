library(tidyverse)
library(plotly)

options(scipen = 9999)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")

#tabla_resultados$brecha_ITI_df
#tabla_resultados$brecha_IOP_df
#tabla_resultados$brecha_IOP_no_reg_df
#tabla_resultados$brecha_IOP_hr_df

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

brechas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    colores <-  c("#FE1764", "#00BDD6", "black")
    
   
    
    armar_tabla <- function(dataframe,
                            brecha,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- dataframe %>% 
                                  
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        rename("brecha" = brecha)
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Mujeres (Ingreso medio)"="media.mujeres","Varones (Ingreso medio)"="media.varones", "Brecha" = "brecha")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
    plot <- function(base,var,nombre,periodo_i, periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        mutate(x = (media.mujeres+media.varones)/2) %>% 
        rename("brecha" = var)
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      fig <- plot_ly(tabla, color = I("gray80"))
      fig <- fig %>% add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
      fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0(brecha,"%"), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) 
      fig <- fig %>% add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Mujeres</b>','</br>$',round(media.mujeres,0)))
      fig <- fig %>% add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Varones</b>','</br>$',round(media.varones,0)))
      
      fig <- fig %>% layout(
        title = "",
        xaxis = list(title = nombre),
        yaxis = list(title = "Período"),
        margin = list(l = 65),
        showlegend = T,
        font = list(family = "Times New Roman")
      )
      
      fig
      
    }
    
    
    generar_titulo <- function(nombre,periodo_i, periodo_f){
      titulo <- paste0("<b>","<font size='+2'>","Brechas de ", nombre ,". Desde ", periodo_i, " hasta ", periodo_f,"</b>","</font>")
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      plot(base = tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
           
           
           var = nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
           nombre =input$ingreso_id,
           input$id_periodo[1],
           input$id_periodo[2]) 
    })
    
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                  brecha =nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2])})
    
  })
}




trimestres <- tabla_resultados[[(nombres_brechas$tabla[1])]] %>% ungroup() %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  select(periodo) %>% unique()

trimestres <- trimestres$periodo

brechas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Brechas de ingresos - general',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso',
                           choices = nombres_brechas$nombre,
                           selected = nombres_brechas$nombre[1],
                           multiple = FALSE),
               selectInput(ns('precios_id'),label = 'Valuación:',
                           choices = c("Precios corrientes", "Precios constantes"),
                           selected = "Precios corrientes",
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T4"))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_br_gen",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600),
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))
                        ),
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_br_gen",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 column(9, 
                                        box(tableOutput(ns('tabla')))),
                                 column(3,          
                                        box(title = "Metadata", width = NULL, textOutput(ns('metadata2')))
                                        
                                        
                                 ))
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}


