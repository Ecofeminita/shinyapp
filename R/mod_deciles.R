
library(plotly)
library(shinyWidgets)
library(shinydashboard)


#armar_tabla(tabla_tipo_insercion, jerarqs[1], trimestres[1], trimestres[4])
#plot(tabla_tipo_insercion, "Período", jerarqs[1], trimestres[1], trimestres[4])

#tabla_resultados$deciles_IPCF_sexo_df
#tabla_resultados$deciles_ITI_sexo_df

nombres_deciles <- data.frame("tabla" =c("deciles_IPCF_sexo_df",
                                         "deciles_ITI_sexo_df"),
                              
                              "cod" =c("DECCFR",
                                       "DECINDR"),
                              
                              "nombre"= c("Ingreso per cápita Familiar",
                                          "Ingreso Total Individual"
                              ))

deciles_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    #colores = c("#FE1764", "#00BDD6")
    
    #armar_tabla(tabla_resultados[["deciles_ITI_sexo_df"]],"DECINDR","16T2","16T3")
    
    armar_tabla <- function(dataframe,
                            tipo_ingreso,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- dataframe %>%                        
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        rename("Decil" = tipo_ingreso)
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Sexo", "Decil", "Porcentaje de la población" = "Prop")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
    
    plot <- function(base,
                     eje_x,
                     tipo_ingreso,
                     periodo_i,
                     periodo_f){
      
      datagraf1 <- base %>%         
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        rename("Decil" = tipo_ingreso)
        
      
      datagraf2 <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      grafico <- ggplot(datagraf2, aes(x=periodo, y=Prop, fill=Decil
                                       ,text=paste0('</br>',Sexo,'</br><b>Decíl: ',Decil,'</b></br>Población: ',Prop,'%', '</br>Período: ',periodo)
                                       
      )) + 
        geom_col(position = "stack")+
        facet_wrap(~Sexo)+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_fill_viridis_d(option = "B", alpha = .7, end = .7)+
        #scale_fill_manual(values = colores) +
        labs(x = eje_x,
             y = "",
             fill = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")+
        scale_y_continuous(labels = function(x) (paste0(x,"%")))
      
      #grafico
      ggplotly(grafico, tooltip = c("text")) %>%
        layout(showlegend = F,
               font = list(family = "Times New Roman"))
    }
    
    
    #plot(tabla_resultados[["deciles_ITI_sexo_df"]],"x","DECINDR","16T2","16T3")
    
    generar_titulo <- function(tipo_ingreso,periodo_i, periodo_f){
      titulo <- paste0("<b>","<font size='+2'>","Distribución de la población según decil de ",tipo_ingreso,". Desde ", periodo_i, " hasta ", periodo_f,"</b>","</font>")
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      plot(tabla_resultados[[(nombres_deciles$tabla[nombres_deciles$nombre == input$ingreso_id])]],
           
           eje_x = "Período",
           tipo_ingreso = nombres_deciles$cod[nombres_deciles$nombre == input$ingreso_id],
           input$id_periodo[1],
           input$id_periodo[2]) 
    })
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_deciles$tabla[nombres_deciles$nombre == input$ingreso_id])]],
                  tipo_ingreso =  nombres_deciles$cod[nombres_deciles$nombre == input$ingreso_id],
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id,input$id_periodo[1],input$id_periodo[2])})
    
  })
}


ingresos <- nombres_deciles %>% pull(nombre)


trimestres <- tabla_resultados[["deciles_ITI_sexo_df"]] %>% ungroup() %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  pull(periodo) 

deciles_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Deciles de ingreso',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso',
                           choices = ingresos,
                           selected = ingresos[2],
                           multiple = F),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","17T4"))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_deciles",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600),
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))
                        ),
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_deciles",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 column(7, 
                                        box(tableOutput(ns('tabla')))),
                                 column(5,          
                                        box(title = "Metadata", width = NULL, textOutput(ns('metadata2')))
                                        
                                        
                                 ))
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}