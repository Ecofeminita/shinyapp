
library(plotly)
library(shinyWidgets)
library(shinydashboard)


#armar_tabla(tabla_tipo_insercion, jerarqs[1], trimestres[1], trimestres[4])
#plot(tabla_tipo_insercion, "Período", jerarqs[1], trimestres[1], trimestres[4])

#tabla_resultados$deciles_IPCF_sexo_df
#tabla_resultados$deciles_ITI_sexo_df

# nombres_deciles <- data.frame("tabla" =c("deciles_IPCF_sexo_df",
#                                          "deciles_ITI_sexo_df"),
#                               
#                               "cod" =c("DECCFR",
#                                        "DECINDR"),
#                               
#                               "nombre"= c("Ingreso per cápita Familiar",
#                                           "Ingreso Total Individual"
#                               ))

deciles_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    colores = c("#FE1764", "#00BDD6")
    
    #armar_tabla(tabla_resultados[["deciles_ITI_sexo_df"]],"DECINDR","16T2","16T3")
    
    armar_tabla <- function(dataframe,
                            tipo_ingreso,
                            periodo_i,
                            periodo_f
    ){
      datagraf1 <- dataframe %>%                        
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("Decil" = tipo_ingreso) %>% 
        filter(Sexo == "Mujeres") %>% 
        rename("Porcentaje de población femenina del decil" = Prop)
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
        select(-periodo, -Pob,"Año" = "ANO4", "Trimestre" = "TRIMESTRE", "Decil","Porcentaje de población femenina del decil")
      
      datagraf
    }
    
    # generar_titulo <- function(variables, periodo_i, periodo_f){
    #   nombre_variable <-  paste0(variables, collapse = ", ")
    #   nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
    #   titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    # }
    
    
    
    plot <- function(base,
                     tipo_ingreso,
                     periodo_i,
                     periodo_f){
      
      datagraf1 <- base %>%         
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("Decil" = tipo_ingreso) %>% 
        filter(Sexo == "Mujeres")
        
      
      datagraf2 <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      grafico <- ggplot(datagraf2, aes(x=periodo, y=Decil, fill=Prop
                                       ,text=paste0('</br><b>Decíl: ',Decil,'</b></br>Población femenina del decil: ',Prop,'%', '</br>Período: ',periodo)
                                       
      )) + 
        geom_tile()+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "none",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
             
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_fill_gradient(low=colores[2], high=colores[1]) +
    
        labs(x = "Período",
             y = "Decil")
      
      #grafico
      ggplotly(grafico, tooltip = c("text")) %>%
        layout(showlegend = F,
               font = list(family = "Times New Roman"))
    }
    
    
    #plot(tabla_resultados[["deciles_ITI_sexo_df"]],"DECINDR","16T2","16T3")
    
    generar_titulo <- function(tipo_ingreso,periodo_i, periodo_f){
      titulo <- paste0("</br><b>","<font size='+2'>","Distribución de la población según decil de ",tipo_ingreso,".","</font>","</b>", "<font size='+1'>","</br> Desde ", periodo_i, " hasta ", periodo_f,".","</font>","</br> Población perceptora de ingresos.")
      titulo
    }
    
    
    output$plot <- renderPlotly({

      plot(tabla_resultados[[(nombres_deciles$tabla[nombres_deciles$nombre == input$ingreso_id])]],

           tipo_ingreso = nombres_deciles$cod[nombres_deciles$nombre == input$ingreso_id],
           input$id_periodo[1],
           input$id_periodo[2])
    })

    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_deciles$tabla[nombres_deciles$nombre == input$ingreso_id])]],
                  tipo_ingreso =  nombres_deciles$cod[nombres_deciles$nombre == input$ingreso_id],
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    },
    width="600px")
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0(input$ingreso_id)]})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id,input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Decil',input$ingreso_id,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados[[(nombres_deciles$tabla[nombres_deciles$nombre == input$ingreso_id])]],
                               tipo_ingreso =  nombres_deciles$cod[nombres_deciles$nombre == input$ingreso_id],
                               input$id_periodo[1],
                               input$id_periodo[2]
        ), 
                   file)    }
    )
    
  })
}


# ingresos <- nombres_deciles %>% pull(nombre)
# 
# 
# trimestres <- tabla_resultados[["deciles_ITI_sexo_df"]] %>% ungroup() %>% 
#   mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
#                           levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
#   pull(periodo) 

deciles_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Deciles de ingreso',
           
           titlePanel('Deciles de ingreso'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso',
                           choices = ingresos,
                           selected = ingresos[2],
                           multiple = F),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_deciles",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        plotlyOutput(ns('plot'), height = 600)%>% withSpinner(type = 5, color ="#e5616e")
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_deciles",
                        
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





####guardo el anterior


# grafico <- ggplot(datagraf2, aes(x=periodo, y=Prop, fill=Decil
#                                  ,text=paste0('</br>',Sexo,'</br><b>Decíl: ',Decil,'</b></br>Población: ',Prop,'%', '</br>Período: ',periodo)
#                                  
# )) + 
#   geom_col(position = "stack")+
#   facet_wrap(~Sexo)+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
#         legend.position = "bottom",
#         panel.background = element_rect(fill = "gray99", color = "gray90"),
#         #plot.background = element_rect(fill="gray99", color = NA),
#         strip.text.y = element_text(angle = 0),
#         panel.grid.minor.y = element_blank()) +
#   scale_fill_viridis_d(option = "B", alpha = .7, end = .7)+
#   #scale_fill_manual(values = colores) +
#   labs(x = eje_x,
#        y = "",
#        fill = "",
#        caption = "Fuente: Elaboración propia en base a EPH-INDEC")+
#   scale_y_continuous(labels = function(x) (paste0(x,"%")))