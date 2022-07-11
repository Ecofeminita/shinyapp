library(plotly)
library(shinyWidgets)
library(shinydashboard)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")
#tabla_resultados$servicio_domestico_ocupadas_df

serv_dom_ocupadas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    colores = c("#FE1764", "#00BDD6")
    
    
    
    armar_tabla <- function(dataframe,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%                          
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        filter(servicio.domestico == "Sí")
      
      
     
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        select(-periodo, -servicio.domestico, -frecuencia, "Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Porcentaje de ocupadas mujeres que se dedican al servicio doméstico"="proporcion")
      
      datagraf
    }
    
    generar_titulo <- function(periodo_i, periodo_f){
  
      titulo <- paste0("<b>","<font size='+2'>","</br>Porcentaje de ocupadas mujeres que se dedican al servicio doméstico desde ", periodo_i, " hasta ", periodo_f, ".</font> </br><font size='+1'>Mujeres ocupadas.","</b>","</font>")
    }
    
    graficos_series <- function(dataframe, 
                                
                                porcentaje = TRUE,
                                periodo_i,
                                periodo_f
    ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           # Daraframe para 2016-19
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         # Periodo como factor y con formato 
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE))))  %>% 
        filter(servicio.domestico == "Sí")
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      
      grafico <- ggplot(datagraf, aes(periodo, proporcion,group=1
                                      ,text=paste0('</br>Porcentaje de ocupadas mujeres que se dedican al servicio doméstico: ',proporcion,'%', '</br>Período: ',periodo)
      )) +
        geom_line(size = 1, alpha = 0.75, color = colores[1]) +
        geom_point(size = 1, color = colores[1]) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
       # scale_color_manual(values = colores) +
        labs(x = "Período",
             y = "Porcentaje de ocupadas mujeres que se dedican al servicio doméstico",
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
      if(porcentaje){
        grafico <- grafico + 
          scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(10,25))    
      }
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
    }
    
    
    
    
    output$plot <- renderPlotly({graficos_series(dataframe= "servicio_domestico_ocupadas_df",
                                                 porcentaje = TRUE,
                                                 periodo_i = input$id_periodo[1],
                                                 periodo_f = input$id_periodo[2]
    )})
    
    output$tabla <- renderTable({
      armar_tabla(dataframe= "servicio_domestico_ocupadas_df",
                  
                  input$id_periodo[1],input$id_periodo[2]
      )
    })
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == "Ocupadas servicio doméstico"]})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('ocup_serv_dom','.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(dataframe= "servicio_domestico_ocupadas_df",
                               
                               input$id_periodo[1],input$id_periodo[2]
        ), 
        file)    }
    )
    
  })
}



serv_dom_ocupadas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Ocupadas en el servicio doméstico',
           titlePanel('Ocupadas en el servicio doméstico'),
           sidebarLayout(
             sidebarPanel(
               # selectInput(ns('indicador'),label = 'Elegir indicador',
               #             choices = tasas,
               #             selected = tasas[1],
               #             multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T4")),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_serv_dom",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 500)
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_serv_dom",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 column(9, 
                                        box(tableOutput(ns('tabla')))),
                                 column(3,          
                                        box(width = NULL,
                                            downloadButton(ns('downloadTable'),'Descargar tabla'))
                                        
                                        
                                 ))
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}
