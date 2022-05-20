library(plotly)
library(shinyWidgets)
library(shinydashboard)

tasas_sexo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    colores = c("#FE1764", "#00BDD6")
    
    armar_tabla <- function(dataframe,
                              variable = "indicador", 
                              valores_filter = c("Tasa de Actividad"),
                              periodo_i,
                              periodo_f
    ){
      datagraf1 <- tabla_resultados[[dataframe]] %>%                          
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        filter(eval(parse(text=variable)) %in% valores_filter) 
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Indicador" = "indicador", "Sexo", "Valor" = "valor")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f, ". Población de 14 años y más.")
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
                                periodo_f
                                ){
      
      datagraf1 <- tabla_resultados[[dataframe]] %>%                           # Daraframe para 2016-19
        mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",              # Identifico periodos
                                 TRUE ~ "2016-2019"),
               grp = paste0(Sexo, dummy),                                           # Grupos por Sexo y Período (4 grupos)
               # periodo = as.yearqtr(paste0(ANO4,".",TRIMESTRE), format="%Y.%q")), # Para trabajar con formato fecha 
               periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         # Periodo como factor y con formato 
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) 
      
      datagraf <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
       
      
      if (filtro) {                                    # Por si tengo que filtrar la base antes
        datagraf <- datagraf %>% 
          filter(eval(parse(text=variable)) %in% valores_filter)
      }                                                  
      
      grafico <- ggplot(datagraf, aes(periodo, valor, color = Sexo, group = grp
                                      ,text=paste0('</br>',Sexo,'</br>Tasa: ',valor,'%', '</br>Período: ',periodo)
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
        scale_color_manual(values = colores) +
        labs(title = titulo,
             subtitle = subtitulo,
             x = eje_x,
             y = eje_y,
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
      #scale_x_yearqtr(format = "%yQ%q", n = 19)               # Para trabajar con formato fecha
      
      if(porcentaje){
        grafico <- grafico + 
          scale_y_continuous(labels = function(x) paste0(x, "%"))    # Para que se peque el valor y el signo de %
      }
      
      grafico <- ggplotly(grafico, tooltip = c("text"))
      
      return(grafico)
    }
    
    # generar_titulo <- function(variables, periodo_i, periodo_f){
    #   nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
    #   titulo <- paste0(variables ," desde ", periodo_i, " hasta ", periodo_f)
    # }
    
    
    
    output$plot <- renderPlotly({graficos_series(dataframe= "tasas_por_sexo_df",
                                                 filtro = TRUE, 
                                                 variable = "indicador", 
                                                 valores_filter = input$indicador,
                                                 eje_x = "Período",
                                                 eje_y = "",
                                                 titulo = "",#input$indicador, 
                                                 subtitulo = "Población de 14 años y más. Por sexo y período. Total 31 aglomerados urbanos.",
                                                 periodo_i = input$id_periodo[1],
                                                 periodo_f = input$id_periodo[2]
                                                 )})
    
    output$tabla <- renderTable({
      armar_tabla(dataframe= "tasas_por_sexo_df",
                  variable = "indicador", 
                  valores_filter = input$indicador,
                  input$id_periodo[1],input$id_periodo[2]
                  )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$indicador,
                                                 input$id_periodo[1],input$id_periodo[2])})
    
  })
}


tasas <- tabla_resultados[["tasas_por_sexo_df"]]$indicador %>% unique()

tasas <- tasas[grepl("Tasa",tasas)]

trimestres <- tabla_resultados[["tasas_por_sexo_df"]] %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  select(periodo) %>% unique()

trimestres <- trimestres$periodo

tasas_sexo_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Tasas básicas',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('indicador'),label = 'Elegir indicador',
                           choices = tasas,
                           selected = tasas[1],
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T4"))
            
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_tb",
                        
                        br(),
                        box(width = NULL, textOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 500),
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))
                            ),
                       
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_tb",
                        
                        br(),
                        box(width = NULL, textOutput(ns('titulo2'))), 
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
