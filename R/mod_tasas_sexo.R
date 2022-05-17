library(plotly)

tasas_sexo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    colores = c("#FE1764", "#00BDD6")
    
    graficos_series <- function(dataframe, 
                                filtro = FALSE, 
                                variable = "indicador", 
                                valores_filter = c("Tasa Actividad"),
                                eje_x = "Período",
                                eje_y = "",
                                titulo = "Titulo", 
                                subtitulo = "Subtitulo",
                                porcentaje = TRUE){
      
      datagraf <- tabla_resultados[[dataframe]] %>%                           # Daraframe para 2016-19
        mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",              # Identifico periodos
                                 TRUE ~ "2016-2019"),
               grp = paste0(Sexo, dummy),                                           # Grupos por Sexo y Período (4 grupos)
               # periodo = as.yearqtr(paste0(ANO4,".",TRIMESTRE), format="%Y.%q")), # Para trabajar con formato fecha 
               periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         # Periodo como factor y con formato 
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE))))
      
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
    
    
    
    output$plot <- renderPlotly({graficos_series(dataframe= "tasas_por_sexo_df",
                                                 filtro = TRUE, 
                                                 variable = "indicador", 
                                                 valores_filter = input$indicador,
                                                 eje_x = "Período",
                                                 eje_y = "",
                                                 titulo = input$indicador, 
                                                 subtitulo = "Población de 14 años y más. Por sexo y período. Total 31 aglomerados urbanos.")})
  })
}


tasas <- tabla_resultados[["tasas_por_sexo_df"]]$indicador %>% unique()

tasas <- tasas[grepl("Tasa",tasas)]


tasas_sexo_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Mercado de Trabajo',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('indicador'),label = 'Elegir indicador',
                           choices = tasas,
                           selected = tasas[1],
                           multiple = FALSE)
             ),
             mainPanel(plotlyOutput(ns('plot'))
             )
           )
  )
}
