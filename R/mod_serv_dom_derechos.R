library(plotly)
library(shinyWidgets)
library(shinydashboard)


serv_dom_derechos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(dataframe,
                            
                            valores_filter = c("No tiene descuento jubilatorio"),
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%                          
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        filter(indicador %in% valores_filter) %>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))%>% 
        relocate(valor, .after = last_col())
      
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))))%>% 
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Porcentaje de trabajadoras del servicio doméstico que.."= "indicador", "Valor"= "valor")
      
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("<b>","<font size='+2'></br>Trabajadoras de Casas Particulares que: ",nombre_variable ,".</font>","<font size='+1'></br> Desde ", periodo_i, " hasta ", periodo_f, ". </font> </br> Trabajadoras de Casas Particulares.","</b>")
    }
    
    graficos_series <- function(dataframe, 
                                valores_filter = c("No tiene descuento jubilatorio"),
                               
                                porcentaje = TRUE,
                                periodo_i,
                                periodo_f
    ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           # Daraframe para 2016-19
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         # Periodo como factor y con formato 
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) 
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) %>% 
        filter(indicador %in% valores_filter) 
      
      
      grafico <- ggplot(datagraf, aes(periodo, valor, color = indicador, group = indicador
                                      ,text=paste0('</br><b>',indicador,'</b></br>Tasa: ',round(valor,2),'%', '</br>Período: ',periodo)
      )) +
        geom_line(linewidth = 1, alpha = 0.75) +
        geom_point(size = 1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores5) +
        labs(x = "Período",
             y = paste0("Porcentaje de trabajadoras de Casas Particulares que..."),
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
     
      
      if(porcentaje){
        grafico <- grafico + 
          scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))   
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
      
    }
    
    #,
    #width="600px"
    )
    
    output$metadata <- renderText({paste0("</br>",tabla_metadata$metadata[tabla_metadata$indicador %in% input$indicador], "</br>")})
    
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
        file)  
        
        shinyalert(
          title = "",
          text = texto_cita,
          size = "xs", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "ok!",
          confirmButtonCol = colores2[1],
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
        
        }
    )
    
  })
}




serv_dom_derechos_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Derechos laborales',
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Ingresos y tasa de feminización", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black"),
                    onclick="fakeClick('Ingresos y tasa de feminización')",
                    class="btn btn-primary btn-s"),
             tags$a("Ocupadas en el servicio doméstico", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black"),
                    onclick="fakeClick('Ocupadas en el servicio doméstico')",
                    class="btn btn-primary btn-s")
             
           ),
           
           titlePanel('Derechos laborales de las trabajadoras de Casas Particulares'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('indicador'),label = 'Elegir derecho laboral vulnerado:',
                           choices = derechos,
                           selected = derechos[1],
                           multiple = T),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
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
                        plotlyOutput(ns('plot'), height = 550)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        
                        
                        tags$div( style="display: inline-flex;",  id = ns("fuentes"),
                                  tags$p(texto_fuentes), 
                                  HTML('&nbsp;'),
                                  tags$a("Metodología", id = ns("f_metod"),
                                         
                                         
                                         onclick="fakeClick('Metodología')"#,
                                         
                                  ),
                        )
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_der",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                        fluidRow(
                          column(12,
                                 
                                 column(9, 
                                        
                                        box(tableOutput(ns('tabla')), width = 12)),
                                 column(3,          
                                        box(width = NULL,
                                            downloadButton(ns('downloadTable'),'Descargar tabla'))
                                        
                                        
                                 ))
                        ),
                        br(),
                        
                        
                        tags$div( style="display: inline-flex;",  id = ns("fuentes2"),
                                  tags$p(texto_fuentes), 
                                  HTML('&nbsp;'),
                                  tags$a("Metodología", id = ns("f_metod"),
                                         
                                         
                                         onclick="fakeClick('Metodología')"#,
                                         
                                  ),
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}
