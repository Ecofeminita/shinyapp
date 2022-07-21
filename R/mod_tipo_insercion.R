




library(plotly)
library(shinyWidgets)
library(shinydashboard)


#armar_tabla(tabla_tipo_insercion, jerarqs[1], trimestres[1], trimestres[4])
#plot(tabla_tipo_insercion, "Período", jerarqs[1], trimestres[1], trimestres[4])


#tabla_resultados <- readRDS("www/tabla_resultados.RDS")

# t3_acomodo <- tabla_resultados[["tasas_no_registro_df"]] %>% 
#   mutate(JERARQUIA = "Trabajadores Asalariados")
# 
# tabla_tipo_insercion <- tabla_resultados[["sexo_segun_jerarquias_df"]] %>% 
#   left_join(.,t3_acomodo, by = c("ANO4", "TRIMESTRE","Sexo", "JERARQUIA"))
# 
# tabla_tipo_insercion_asal <- tabla_tipo_insercion %>% 
#   filter(JERARQUIA == "Trabajadores Asalariados") %>% 
#   mutate("Trabajadores Asalariados No Registrados" = round(tasa*(`Proporción de no Registrados`)/100,1),
#          "Trabajadores Asalariados Registrados" = round(tasa*(100-`Proporción de no Registrados`)/100,1)) %>% 
#   select(-`Proporción de no Registrados`,-"JERARQUIA",-tasa) %>% 
#   gather(., key ="JERARQUIA", value = "tasa",-Sexo,-ANO4,-TRIMESTRE)
# 
# tabla_tipo_insercion <- bind_rows((tabla_tipo_insercion %>% select(-`Proporción de no Registrados`)),tabla_tipo_insercion_asal) %>%
#   filter(JERARQUIA != "Trabajadores Asalariados")

tipo_insercion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    #colores5 <- c("#e5616e","#e9c1d0","#fbd17e","#8594c6","#8cddd3")
    
    
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
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
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
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        filter(JERARQUIA %in% jerarquias)# %>% 
        #filter(periodo != "16T4") %>% 
        
        grafico <- ggplot(datagraf2, aes(x=periodo, y=tasa, fill=JERARQUIA
                      ,text=paste0('</br>',Sexo,'</br><b>',JERARQUIA,'</b></br>Tasa: ',tasa,'%', '</br>Período: ',periodo)
                      
        )) + 
        geom_col(position = "stack")+
        facet_wrap(~Sexo)+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              #plot.background = element_rect(fill="gray99", color = NA),
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
    },
    width="600px")
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0(input$jerarqs_id)]})
    
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
                   file)    }
    )
    
  })
}


# jerarqs <- tabla_tipo_insercion %>% ungroup() %>%  select(JERARQUIA) %>% unique()
# 
# jerarqs <- jerarqs$JERARQUIA
# 
# trimestres <- tabla_tipo_insercion %>% ungroup() %>% 
#   mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
#                           levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
#   select(periodo) %>% unique()
# 
# trimestres <- trimestres$periodo

tipo_insercion_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Tipo de inserción laboral',
           
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
                        
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_tip_ins",
                        
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