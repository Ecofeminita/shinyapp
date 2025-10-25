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
        rename("brecha" = brecha) %>% 
        rename("brecha_corriente" = names(dataframe)[grepl("corr",names(dataframe))])%>% 
        mutate(ANO4 = as.character(round(as.numeric(ANO4),0)),
               TRIMESTRE = as.character(round(as.numeric(TRIMESTRE),0)))
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))))%>% 
        
        select(-periodo,
             
               "Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               "Brecha (%) - precios constantes" = "brecha", 
               "Brecha (%) - precios corrientes" = "brecha_corriente", 
               "Mujeres (Ingreso medio - precios corrientes)"="media.mujeres",
               "Varones (Ingreso medio - precios corrientes)"="media.varones", 
               "Mujeres (Ingreso medio - precios constantes)"="cte_media.mujeres",
               "Varones (Ingreso medio - precios constantes)"="cte_media.varones")
      
      if(valuacion =="Precios corrientes"){
        
        datagraff <- datagraf %>% 
          select(-c("Varones (Ingreso medio - precios constantes)","Mujeres (Ingreso medio - precios constantes)","Brecha (%) - precios constantes" ))
        
        return(datagraff)
        
      } else if(valuacion ==paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        datagraff <- datagraf %>% 
          select(-c("Mujeres (Ingreso medio - precios corrientes)","Varones (Ingreso medio - precios corrientes)","Brecha (%) - precios corrientes"))
        
        return(datagraff)
      }
      
      
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
    plot_ingresos <- function(base,var,nombre,eje_x = "Período",eje_y = "Ingreso Medio",titulo = "",subtitulo = "",periodo_i,periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("brecha" = var) %>% 
        pivot_longer(
          cols = c(cte_media.mujeres, cte_media.varones),
          names_to = "Sexo",
          values_to = "valor"
        ) %>%
        mutate(
          Sexo = case_when(
            Sexo == "cte_media.mujeres" ~ "Mujeres",
            Sexo == "cte_media.varones" ~ "Varones"
          )
        ) %>% 
        mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",           
                                 TRUE ~ "2016-2019"),
               grp = paste0(Sexo, dummy)) 
      
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) 
      
      
      grafico <- ggplot(tabla, aes(periodo, valor, color = Sexo, group = grp
                                       ,text=paste0('</br>',Sexo,'</br>Media de Ingresos: ',label_number(accuracy = 1,big.mark = ".", decimal.mark = ",", prefix = "$")(valor),'</br>Brecha [(V-M)/V]: ',brecha, '%', '</br>Período: ',periodo)
      )) +
        geom_line(linewidth = 1, alpha = 0.75) +
        geom_point(size = 1) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_color_manual(values = colores2) +
        scale_y_continuous(
          labels = label_number(scale = 1e-3, suffix = "K", prefix = "$")
        ) +
        labs(title = titulo,
             subtitle = subtitulo,
             x = eje_x,
             y = eje_y,
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
      
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
      
    }
    
    
    plot_brechas <- function(base,nombre,eje_x = "Período",eje_y = "Brecha",titulo = "",subtitulo = "",periodo_i,periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("brecha" = contains("corrientes")) %>% 
        mutate(dummy = case_when(ANO4 %in% c(2004:2006) ~ "2004-2006",           
                                 TRUE ~ "2016-2019"),
               grp = paste0(dummy))
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i])):unique(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))) 
      
      grafico <- ggplot(tabla, aes(periodo, brecha,group = grp,
                                   text=paste0('</br>Brecha [(V-M)/V]: ',brecha, '%', '</br>Período: ',periodo)
      )) +
        geom_line(linewidth = 1, alpha = 0.75, color = colores4[3]) +
        geom_point(size = 1, color = colores4[3]) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 35, vjust = 0.7),
              legend.position = "bottom",
              panel.background = element_rect(fill = "gray99", color = "gray90"),
              strip.text.y = element_text(angle = 0),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(
          labels = label_number(suffix = "%")
        ) +
        labs(title = titulo,
             subtitle = subtitulo,
             x = eje_x,
             y = eje_y,
             color = "",
             caption = "Fuente: Elaboración propia en base a EPH-INDEC")
      
      
      grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
      
      return(grafico)
      
    }
    
    
    
    generar_titulo <- function(nombre,periodo_i,periodo_f,indicador_id){
      if(indicador_id=="Evolución Brecha"){
        indicador <- " Brecha "
        valuacion <- "Precios corrientes"
      }else{
        indicador <- " "
        valuacion <- paste0("Precios constantes (",nombre_trimestre_base,")")
        }
      titulo <- paste0("</br><b>","<font size='+2'>","Evolución", indicador, "del ", nombre ,".","</b>","</font>","<font size='+1'>", "</br>Desde ", periodo_i, " hasta ", periodo_f, "</font>", "</br>", valuacion)
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      if(input$indicador_id == "Evolución Brecha"){
        
        plot_brechas(base = tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                       
                       
                       nombre =paste0(input$ingreso_id, " - ", input$indicador_id),
                       eje_x = "Período",
                       eje_y = "Brecha",
                       titulo = "",
                       subtitulo = "",
                       periodo_i = input$id_periodo[1],
                       periodo_f = input$id_periodo[2]) 
        
        
      } else if(input$indicador_id == "Evolución Ingresos"){
        
        plot_ingresos(base = tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                       
                       
                       var = nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                       nombre =paste0(input$ingreso_id, " - ", input$indicador_id),
                       eje_x = "Período",
                       eje_y = "Ingreso Medio",
                       titulo = "",
                       subtitulo = "",
                       periodo_i = input$id_periodo[1],
                       periodo_f = input$id_periodo[2]) 
        
        
      }
      
        
    })
    
    
    
    
    
    output$tabla <- renderTable({
      if(input$indicador_id=="Evolución Brecha"){valuacion <- "Precios corrientes"}else{valuacion <- paste0("Precios constantes (",nombre_trimestre_base,")")}
      armar_tabla(tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                  brecha =nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                  input$id_periodo[1],
                  input$id_periodo[2],
                  valuacion
      )
    }
    )
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("B-",input$ingreso_id)]})
    output$metadata_ingresos <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Valuación")]})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2],input$indicador_id)})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id, input$id_periodo[1],input$id_periodo[2],input$indicador_id)})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Brecha_',input$ingreso_id,'.xlsx',sep='')},
      content = function(file){
        
        if(input$indicador_id=="Evolución Brecha"){valuacion <- "Precios corrientes"}else{valuacion <- paste0("Precios constantes (",nombre_trimestre_base,")")}
        write.xlsx(armar_tabla(tabla_resultados[[(nombres_brechas$tabla[nombres_brechas$nombre == input$ingreso_id])]],
                               brecha =nombres_brechas$cod[nombres_brechas$nombre == input$ingreso_id],
                               input$id_periodo[1],
                               input$id_periodo[2],
                               valuacion
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
               selectInput(ns('indicador_id'),label = 'Indicador:',
                           choices = c("Evolución Brecha", "Evolución Ingresos"),
                           selected = "Evolución Brecha",
                           multiple = FALSE),
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata')))
               
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


