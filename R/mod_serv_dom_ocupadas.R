library(plotly)
library(shinyWidgets)
library(shinydashboard)


serv_dom_ocupadas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    armar_tabla <- function(dataframe,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%                          
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        filter(servicio.domestico == "Sí")%>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))
      
      
     
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))))%>% 
        select(-periodo, -servicio.domestico, -frecuencia, "Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Porcentaje de ocupadas mujeres que se dedican al servicio doméstico"="proporcion")
      
      datagraf
    }
    
    generar_titulo <- function(periodo_i, periodo_f){
  
      titulo <- paste0("<b>","<font size='+2'>","</br>Porcentaje de ocupadas mujeres que se dedican al servicio doméstico.", "</b>", "</font>","<font size='+1'>", "</br> Desde ", periodo_i, " hasta ", periodo_f, ".","</font>","</br>Mujeres ocupadas.")
    }
    
    graficos_series <- function(dataframe, 
                                
                                porcentaje = TRUE,
                                periodo_i,
                                periodo_f
    ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),        
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4))))  %>% 
        filter(servicio.domestico == "Sí")
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) 
      
      
      
      grafico <- ggplot(datagraf, aes(periodo, proporcion,group=1
                                      ,text=paste0('</br>Porcentaje de ocupadas mujeres que se dedican al servicio doméstico: ',proporcion,'%', '</br>Período: ',periodo)
      )) +
        geom_line(linewidth = 1, alpha = 0.75, color = colores2[1]) +
        geom_point(size = 1, color = colores2[1]) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
       # scale_color_manual(values = colores2) +
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
    }#,
    #width="600px"
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == "Ocupadas servicio doméstico"]})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('ocup_serv_dom','.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(dataframe= "servicio_domestico_ocupadas_df",
                               
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



serv_dom_ocupadas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Ocupadas en el servicio doméstico',
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Ingresos y tasa de feminización", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black"),
                    onclick="fakeClick('Ingresos y tasa de feminización')",
                    class="btn btn-primary btn-s"),
             tags$a("Derechos laborales", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black"),
                    onclick="fakeClick('Derechos laborales')",
                    class="btn btn-primary btn-s")
             
           ),
           
           titlePanel('Ocupadas en el servicio doméstico'),
           sidebarLayout(
             sidebarPanel(
              
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(htmlOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_serv_dom",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 500)%>% withSpinner(type = 5, color ="#e5616e"),
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
                        value = "t_serv_dom",
                        
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
                                         
                                         
                                         onclick="fakeClick('Metodología')"
                                         
                                  ),
                        )
                        
               )
               
               
             )
             
             
             )
             
             
           )
  )
}
