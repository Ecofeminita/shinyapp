library(tidyverse)
library(plotly)

options(scipen = 9999)


brechas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    colores <-  c(colores2, "black")
    
   
    
    armar_tabla <- function(dataframe,
                            brecha,
                            periodo_i,
                            periodo_f,
                            valuacion
    ){
      datagraf1 <- dataframe %>% 
                                  
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("brecha" = brecha)
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))%>% 
        
        select(-periodo,
             
               "Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               "Brecha (%)" = "brecha", 
               "Mujeres (Ingreso medio - precios corrientes)"="media.mujeres",
               "Varones (Ingreso medio - precios corrientes)"="media.varones", 
               "Mujeres (Ingreso medio - precios constantes)"="cte_media.mujeres",
               "Varones (Ingreso medio - precios constantes)"="cte_media.varones")
      
      if(valuacion =="Precios corrientes"){
        
        datagraff <- datagraf %>% 
          select(-c("Varones (Ingreso medio - precios constantes)","Mujeres (Ingreso medio - precios constantes)"))
        
        return(datagraff)
        
      } else if(valuacion ==paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        datagraff <- datagraf %>% 
          select(-c("Mujeres (Ingreso medio - precios corrientes)","Varones (Ingreso medio - precios corrientes)"))
        
        return(datagraff)
      }
      
      
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
    plot_constante <- function(base,var,nombre,periodo_i, periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        mutate(x = (cte_media.mujeres+cte_media.varones)/2) %>% 
        rename("brecha" = var)
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      fig <- plot_ly(tabla, color = I("gray80"))
      fig <- fig %>% add_segments(x = ~cte_media.mujeres, xend = ~cte_media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
      fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0(round(brecha,1),"%"), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) 
      fig <- fig %>% add_markers(x = ~cte_media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Mujeres</b>','</br>$',round(cte_media.mujeres,0)))
      fig <- fig %>% add_markers(x = ~cte_media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Varones</b>','</br>$',round(cte_media.varones,0)))
      
      fig <- fig %>% layout(
        title = "",
        xaxis = list(title = nombre),
        yaxis = list(title = "Período"),
        margin = list(l = 65),
        showlegend = T,
        font = list(family = "Times New Roman")
      )
      
      fig
      
    }
    
    plot_corriente <- function(base,nombre,periodo_i, periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        mutate(x = (media.mujeres+media.varones)/2) %>% 
        mutate(var = media.varones-media.mujeres) %>% 
        rename("brecha" = var)
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      fig <- plot_ly(tabla, color = I("gray80"))
      fig <- fig %>% add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
      fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0("$",round(brecha,2)), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) 
      fig <- fig %>% add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Mujeres</b>','</br>$',round(media.mujeres,0)))
      fig <- fig %>% add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Varones</b>','</br>$',round(media.varones,0)))
      
      fig <- fig %>% layout(
        title = "",
        xaxis = list(title = nombre),
        yaxis = list(title = "Período"),
        margin = list(l = 65),
        showlegend = T,
        font = list(family = "Times New Roman")
      )
      
      fig
      
    }
    
    
    generar_titulo <- function(nombre,periodo_i, periodo_f,valuacion){
      titulo <- paste0("</br><b>","<font size='+2'>","Brechas de ", nombre ,".","</b>","</font>","<font size='+1'>", "</br>Desde ", periodo_i, " hasta ", periodo_f,"</font>", "</br>",valuacion)
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      if(input$precios_id == "Precios corrientes"){
        
        plot_corriente(base = tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                       
                       
                       nombre =paste0(input$ingreso_id, " - ", input$precios_id),
                       input$id_periodo[1],
                       input$id_periodo[2]) 
        
      } else if(input$precios_id == paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        plot_constante(base = tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                       
                       
                       var = nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                       nombre =paste0(input$ingreso_id, " - ", input$precios_id),
                       input$id_periodo[1],
                       input$id_periodo[2]) 
        
      }
      
        
    })
    
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                  brecha =nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                  input$id_periodo[1],
                  input$id_periodo[2],
                  input$precios_id
      )
    }#,
    #width="630px"
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("B-",input$ingreso_id)]})
    output$metadata_ingresos <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Valuación")]})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2],input$precios_id)})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2],input$precios_id)})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Brecha_',input$ingreso_id,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                               brecha =nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                               input$id_periodo[1],
                               input$id_periodo[2],
                               input$precios_id
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




brechas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Brechas de ingresos - general',
           
           
           tags$div(p("También puede interesarte: ", style= "text-align: right") ),
           
           tags$div( style = "display: flex;padding: 1px 0;justify-content: flex-end;",
             tags$a("Brechas de ingresos - desagregado", style=paste0(btn_style,"background:#687aad;border-color: #687aad;"),
                    onclick="fakeClick('Brechas de ingresos - desagregado')",
                    class="btn btn-primary btn-s"),
             tags$a("Deciles de ingreso", style=paste0(btn_style,"background:#687aad;border-color: #687aad;"),
                    onclick="fakeClick('Deciles de ingreso')",
                    class="btn btn-primary btn-s")
             
           ),
           
           
           titlePanel('Brechas de ingresos - general'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso',
                           choices = nombres_brechas$nombre,
                           selected = nombres_brechas$nombre[1],
                           multiple = FALSE),
               selectInput(ns('precios_id'),label = 'Valuación:',
                           choices = c("Precios corrientes", paste0("Precios constantes (",nombre_trimestre_base,")")),
                           selected = paste0("Precios constantes (",nombre_trimestre_base,")"),
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata'))), 
               hr(),
               h5(textOutput(ns('metadata_ingresos')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_br_gen",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
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
                        value = "t_br_gen",
                        
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


