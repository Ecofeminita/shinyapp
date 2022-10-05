
library(plotly)
library(shinyWidgets)
library(shinydashboard)


tasas_edad_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    armar_tabla <- function(dataframe,
                            variable = "indicador", 
                            valores_filter = c("Tasa de Actividad"),
                            grupos,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%  
        filter(!is.na(GRUPO_EDAD)) %>% 
        filter(GRUPO_EDAD %in% grupos) %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        filter(eval(parse(text=variable)) %in% valores_filter) %>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))%>% 
        relocate(valor, .after = last_col())
      
      names(datagraf1)[length(datagraf1)] <- paste0(valores_filter[1])
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        select(-periodo,-indicador,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Grupo edad" = "GRUPO_EDAD", "Sexo")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("</br> <b>","<font size='+2'>",nombre_variable ,".</b></font>","<font size='+1'>","</br>  Desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
    
    graficos_series <- function(dataframe, 
                                filtro = FALSE, 
                                variable = "indicador", 
                                valores_filter = c("Tasa de Actividad"),
                                eje_x = "Período",
                                eje_y = "",
                                titulo = "Titulo", 
                                subtitulo = "Subtitulo",
                                porcentaje = TRUE,
                                periodo_i,
                                periodo_f,
                                grupos
    ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           
        filter(GRUPO_EDAD %in% grupos) %>% 
        mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",              
                                 TRUE ~ "2016-2019"),
               grp = paste0(GRUPO_EDAD,Sexo, dummy),                                           
               periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         #
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) 
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      if (filtro) {                                    
        datagraf <- datagraf %>% 
          filter(eval(parse(text=variable)) %in% valores_filter)
      }                                                  
      
      grafico <- ggplot(datagraf, aes(periodo, valor, color = grp, group = grp, shape=GRUPO_EDAD
                                      ,text=paste0('</br><b>',Sexo,'</br>',GRUPO_EDAD,'</b></br>Tasa: ',valor,'%', '</br>Período: ',periodo)
      )) +
        geom_line(size = 1, alpha = 0.75) +
        geom_point(size = 1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "none",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
            
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores4) +
        labs(title = titulo,
             subtitle = subtitulo,
             x = eje_x,
             y = eje_y,
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")#+
        #facet_wrap(~GRUPO_EDAD, nrow = 2)
      
      if(porcentaje){
        grafico <- grafico + 
          scale_y_continuous(labels = function(x) paste0(x, "%"))   
      }
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
    }
    
   
    
    output$plot <- renderPlotly({graficos_series(dataframe= "tasas_por_sexo_edad_df",
                                                 filtro = TRUE, 
                                                 variable = "indicador", 
                                                 valores_filter = input$indicador,
                                                 eje_x = "Período",
                                                 eje_y = "",
                                                 titulo = "",
                                                 subtitulo = "Población de 14 años y más. Por sexo y período. Total 31 aglomerados urbanos.",
                                                 periodo_i = input$id_periodo[1],
                                                 periodo_f = input$id_periodo[2],
                                                 grupos = input$g_edad
    )})
    
    output$tabla <- renderTable({
      armar_tabla(dataframe= "tasas_por_sexo_edad_df",
                  variable = "indicador", 
                  valores_filter = input$indicador,
                  grupos = input$g_edad,
                  input$id_periodo[1],input$id_periodo[2]
      )
    }#,
    #width="600px"
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == input$indicador]})
    output$metadata_edad <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Desagregación edad")]})
    
    
    output$metadata_pea <- renderText({

      if(grepl("económicamente activa",
               tabla_metadata$metadata[tabla_metadata$indicador == input$indicador],
               ignore.case = T)
      ){
        tabla_metadata$metadata[tabla_metadata$indicador == paste0("Población Económicamente Activa")]
      }else{
        paste0("")
      }
      
     })
    
    output$titulo1 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(input$indicador,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx( armar_tabla(dataframe= "tasas_por_sexo_edad_df",
                                variable = "indicador", 
                                valores_filter = input$indicador,
                                grupos = input$g_edad,
                                input$id_periodo[1],input$id_periodo[2]
        ), 
                   file)    }
    )
    
  })
}



tasas_edad_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Por grupos de edad',
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Tasas básicas",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Tasas básicas')",
                    class="btn btn-primary btn-s"),
             
             tags$a("Tipo de inserción laboral",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Tipo de inserción laboral')",
                    class="btn btn-primary btn-s"),
             
             tags$a("Ramas de la actividad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Ramas de la actividad')",
                    class="btn btn-primary btn-s")
             
           ),
           
           
           titlePanel('Tasas básicas por grupo de edad'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('indicador'),label = 'Elegir indicador',
                           choices = tasas_edad,
                           selected = tasas_edad[1],
                           multiple = FALSE),
               selectInput(ns('g_edad'),label = 'Grupo de edad:',
                           choices = grupos_edad,
                           selected = grupos_edad[1],
                           multiple = TRUE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata'))), 
               hr(),
               h5(textOutput(ns('metadata_pea'))),
               hr(),
               h5(textOutput(ns('metadata_edad')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_ted",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 800)%>% withSpinner(type = 5, color ="#e5616e"),
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
                        value = "t_ted",
                        
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
