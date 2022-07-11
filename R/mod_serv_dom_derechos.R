library(plotly)
library(shinyWidgets)
library(shinydashboard)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")
#tabla_resultados$derechos_servicio_domestico_df

serv_dom_derechos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    #colores = c("#FE1764", "#00BDD6")
    
    
    
    armar_tabla <- function(dataframe,
                            
                            valores_filter = c("No tiene descuento jubilatorio"),
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%                          
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        filter(indicador %in% valores_filter) %>% 
        relocate(valor, .after = last_col())
      
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Porcentaje de trabajadoras del servicio doméstico que.."= "indicador", valor)
      
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("<b>","<font size='+2'></br>Trabajadoras del servicio doméstico que: ",nombre_variable ," desde ", periodo_i, " hasta ", periodo_f, ". </font> </br><font size='+1'> Mujeres que se dedican al servicio doméstico.","</b>","</font>")
    }
    
    graficos_series <- function(dataframe, 
                                valores_filter = c("No tiene descuento jubilatorio"),
                               # eje_y = "",
                                porcentaje = TRUE,
                                periodo_i,
                                periodo_f
    ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           # Daraframe para 2016-19
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         # Periodo como factor y con formato 
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) 
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        filter(indicador %in% valores_filter) 
      
      
      grafico <- ggplot(datagraf, aes(periodo, valor, color = indicador, group = indicador
                                      ,text=paste0('</br><b>',indicador,'</b></br>Tasa: ',round(valor,2),'%', '</br>Período: ',periodo)
      )) +
        geom_line(size = 1, alpha = 0.75) +
        geom_point(size = 1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
       # scale_color_manual(values = colores) +
        labs(x = "Período",
             y = paste0("Porcentaje de trabajadoras del servicio doméstico que..."),
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
      #scale_x_yearqtr(format = "%yQ%q", n = 19)               # Para trabajar con formato fecha
      
      if(porcentaje){
        grafico <- grafico + 
          scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))    # Para que se peque el valor y el signo de %
      }
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
    }
    
    
    
    
    output$plot <- renderPlotly({graficos_series(dataframe= "derechos_servicio_domestico_df",
                                                
                                                 valores_filter = input$indicador,
                                                
                                                 periodo_i = input$id_periodo[1],
                                                 periodo_f = input$id_periodo[2]
    )})
    
    output$tabla <- renderTable({
      armar_tabla(dataframe= "derechos_servicio_domestico_df",
                  
                  valores_filter = input$indicador,
                  input$id_periodo[1],input$id_periodo[2]
      )
      
    })
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador %in% input$indicador]})
    
    output$titulo1 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('derechos_vulnerados_serv_dom','.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(dataframe= "derechos_servicio_domestico_df",
                               
                               valores_filter = input$indicador,
                               input$id_periodo[1],input$id_periodo[2]
        ), 
        file)    }
    )
    
  })
}




serv_dom_derechos_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Derechos laborales',
           titlePanel('Derechos laborales de las trabajadoras de servicio doméstico'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('indicador'),label = 'Elegir derecho laboral vulnerado:',
                           choices = derechos,
                           selected = derechos[1],
                           multiple = T),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T4")),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(htmlOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_der",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 550)
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_der",
                        
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
