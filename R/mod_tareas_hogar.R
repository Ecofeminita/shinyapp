

library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)

horas_no_remunerado_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    colores = c("#FE1764", "#00BDD6")
    
    armar_tabla <- function(dataframe,
                            periodo_i,
                            periodo_f
    ){
      
      datagraf1 <- dataframe %>% 
        
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) 
      
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        
        select(-periodo) %>% 
        
      
        
        select("Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               "Sexo",
               "Población que realiza las tareas domésticas del hogar (%)" = "proporcion")
      
     
      
      return(datagraf)
      
      
      
    }
    
    
    
    
    generar_titulo <- function(periodo_i, periodo_f){
      titulo <- paste0('</br>',"<b>","<font size='+2'>","Personas de cada sexo que realizan las tareas domésticas del hogar.","</font>", '</br>',"Desde ", periodo_i, " hasta ", periodo_f,"</b>")
      titulo
    }
    
    
    
    grafico <- function(base,
                        
                        periodo_i, periodo_f
                        
    ){
      
      
      
      datagraf1 <- base%>%                           
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         # Periodo como factor y con formato 
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4))))
      
      tabla <-datagraf1 %>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      grafico <- tabla %>% 
        ggplot(.,aes(x = periodo, y = proporcion, fill = Sexo, group = Sexo
                     ,text=paste0('</br><b>',Sexo, '</b>', '</br>Proporción: ', proporcion, '%', '</br>Período: ',periodo
                     )))+
        geom_col(size = 1, alpha = 0.75, postion = "stack")+ 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "none",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_fill_manual(values = colores) +
        labs(title = "",
             x = "Periodo",
             y = paste0("Personas de cada sexo que realizan las tareas domésticas del hogar"),
             color = "")+
        guides(color = "none",
               alpha="none")+ 
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) 
      
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
    }
    
    
    
    proyeccion <- function(){
      df <- tabla_resultados$tareas_domesticas_sexo_df %>%
        mutate(date = paste(ANO4,TRIMESTRE, sep = '-'),
               date = yq(date))
        # filter(Sexo=='Mujeres')
      
      df %>% 
        ggplot(aes(x=date,y=proporcion, color = Sexo)) + 
        geom_point() + 
        geom_hline(yintercept = 50)+
        scale_x_date(limits = c(yq('2016-2'),yq('2045-1')),date_breaks = '5 years')+
        stat_smooth(method="lm",fullrange=TRUE)+
        theme_minimal()
      
      
    }

    
    output$plot <- renderPlotly({grafico(base = tabla_resultados$tareas_domesticas_sexo_df,
                                         
                                         periodo_i = input$id_periodo[1],
                                         periodo_f = input$id_periodo[2]
    )})
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados$tareas_domesticas_sexo_df,
                  
                  
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Distribución de las tareas del hogar")]})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Distribucion_tareas_hogar','.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados$tareas_domesticas_sexo_df,
                               
                               
                               input$id_periodo[1],
                               input$id_periodo[2]
        ), 
        file)    }
    )
    
  })
}





horas_no_remunerado_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Trabajo no remunerado',
           titlePanel('Distribución de las tareas domésticas entre los varones y mujeres'),
           sidebarLayout(
             sidebarPanel(
               
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_horas_n_rem",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600)%>% withSpinner(type = 5, color ="#e5616e")
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_horas_n_rem",
                        
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
