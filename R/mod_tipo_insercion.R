
library(plotly)
library(shinyWidgets)
library(shinydashboard)




tipo_insercion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    ###funciones
    
    
    armar_tabla <- function(dataframe,
                            valores_filter,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- dataframe %>% 
        filter(JERARQUIA %in% valores_filter) %>%                          
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) 
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))))%>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))%>% 
        
        select(-periodo,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Tipo de inserción" = "JERARQUIA", "Sexo", "Valor" = "tasa")
      
      datagraf
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    plot <- function(base,
                     eje_x,
                     jerarquias,
                     periodo_i,
                     periodo_f){
      
      datagraf1 <- base %>%         
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        
        mutate(JERARQUIA = factor(JERARQUIA, levels = c("Jefes","Dirección","Cuentapropia","Trabajadores Asalariados Registrados","Trabajadores Asalariados No Registrados"))) 
      
      datagraf2 <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) %>% 
        filter(JERARQUIA %in% jerarquias)
        
        grafico <- ggplot(datagraf2, aes(x=periodo, y=tasa, fill=JERARQUIA
                      ,text=paste0('</br>',Sexo,'</br><b>',JERARQUIA,'</b></br>Tasa: ',tasa,'%', '</br>Período: ',periodo)
                      
        )) + 
        geom_col(position = "stack")+
        facet_wrap(~Sexo)+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_fill_manual(values = colores5) +
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
    
    
    generar_titulo <- function(periodo_i, periodo_f){
      titulo <- paste0("</br><b>","<font size='+2'>","Personas ocupadas según tipo de inserción laboral",".</b></font>","<font size='+1'>", "</br> Desde ", periodo_i, " hasta ", periodo_f,"</font>")
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      plot(tabla_tipo_insercion,
                                      
                                      eje_x = "Período",
                                      jerarquias = input$jerarqs_id,
                                      input$id_periodo[1],
                                      input$id_periodo[2]) 
      })
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_tipo_insercion,
                  valores_filter = input$jerarqs_id,
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    }#,
    #width="600px"
    )
    
    output$metadata <- renderText({paste0("</br>",tabla_metadata$metadata[tabla_metadata$indicador == paste0(input$jerarqs_id)], "</br>")})
    
    output$titulo1 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Insercion_Laboral.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_tipo_insercion,
                               valores_filter = input$jerarqs_id,
                               input$id_periodo[1],
                               input$id_periodo[2]
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


tipo_insercion_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Tipo de inserción laboral',
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Tasas básicas",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Tasas básicas')",
                    class="btn btn-primary btn-s"),
             tags$a("Tasas básicas por grupos de edad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Por grupos de edad')",
                    class="btn btn-primary btn-s"),
             
             
             tags$a("Ramas de la actividad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;"),
                    onclick="fakeClick('Ramas de la actividad')",
                    class="btn btn-primary btn-s")
             
           ),
           
           titlePanel('Tipo de inserción laboral'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('jerarqs_id'),label = 'Elegir tipo de inserción laboral',
                           choices = jerarqs,
                           selected = jerarqs[1:2],
                           multiple = TRUE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(htmlOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_tip_ins",
                        
                        br(),
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        br(),
                        plotlyOutput(ns('plot'), height = 600)%>% withSpinner(type = 5, color ="#e5616e"),
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
                        value = "t_tip_ins",
                        
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