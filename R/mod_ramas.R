

a <- tabla_resultados$ramas_sexo_df


#colores <- c("#e5616e","#e9c1d0","#fbd17e","#8594c6","#8cddd3")


library(plotly)
library(shinyWidgets)
library(shinydashboard)


#armar_tabla(tabla_tipo_insercion, jerarqs[1], trimestres[1], trimestres[4])
#plot(tabla_tipo_insercion, "Período", jerarqs[1], trimestres[1], trimestres[4])

ramas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    windowsFonts(A = windowsFont("Times New Roman"))
    
    armar_tabla <- function(dataframe,
                            valores_filter,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- dataframe %>% 
        filter(`Rama de la ocupación` %in% valores_filter) %>%                          
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) 
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Rama de la ocupación", "Tasa de feminización", "Ingreso mensual promedio", "Ingreso horario")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
    plot <- function(base,
                     vary,
                     eje_x,
                     valores_filter,
                     periodo_i,
                     periodo_f){
      
      datagraf1 <- base %>%         
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        rename("ingreso" = vary)
      
      datagraf2 <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        filter(`Rama de la ocupación` %in% valores_filter)
       
      
      grafico <- ggplot(datagraf2, aes(x=periodo, y=ingreso, color=`Rama de la ocupación`, size =`Tasa de feminización`
                                       ,text=paste0('</br><b>',`Rama de la ocupación`,'</b></br>Período: ',periodo, '</br>', vary,': $',ingreso, '</br>Tasa de feminización: ', round(`Tasa de feminización`,1),'%')
                                       
      )) + 
        geom_point(alpha = .5)+
        scale_size(range = c(1, 15))+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 0, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        
        labs(x = eje_x,
             y = vary,
             fill = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")+
        scale_y_continuous(labels = function(x) (paste0("$",x)))
      
      #grafico
      ggplotly(grafico, tooltip = c("text")) %>%
        layout(showlegend = F,
               font = list(family = "A"))
    }
    
    
    generar_titulo <- function(periodo_i, periodo_f){
      titulo <- paste0("<b>","<font size='+2'>","Tasa de feminización e ingresos por rama de actividad desde ", periodo_i, " hasta ", periodo_f,"</b>","</font>")
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      plot(tabla_resultados[["ramas_sexo_df"]],
           
           eje_x = "Período",
           vary = input$ingreso_id,
           valores_filter = input$ramas_id,
           input$id_periodo[1],
           input$id_periodo[2]) 
    })
    
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[["ramas_sexo_df"]],
                  valores_filter = input$ramas_id,
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    
  })
}


ramas <- tabla_resultados[["ramas_sexo_df"]] %>% ungroup() %>%  select(`Rama de la ocupación`) %>% unique() %>% drop_na()

ramas <- ramas$`Rama de la ocupación`

trimestres <- tabla_resultados[["ramas_sexo_df"]] %>% ungroup() %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  select(periodo) %>% unique()

trimestres <- trimestres$periodo

ramas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Ramas de la ocupación',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ramas_id'),label = 'Elegir ramas de actividad',
                           choices = ramas,
                           selected = ramas[c(2,4,11)],
                           multiple = TRUE),
               selectInput(ns('ingreso_id'),label = 'Elegir variable de ingreso',
                           choices = c("Ingreso mensual promedio", "Ingreso horario"),
                           selected = "Ingreso mensual promedio",
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T4"))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_ramas",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600),
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))
                        ),
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_ramas",
                        
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