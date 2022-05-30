library(tidyverse)
library(plotly)

options(scipen = 9999)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")

# tabla_resultados$brecha_IOP_calif_df
# tabla_resultados$brecha_IOP_hr_calif_df
# tabla_resultados$brecha_IOP_nivel_educ_df
# tabla_resultados$brecha_IOP_hr_nivel_educ_df

#respetar orden!!!!!
nombres_brechas_desag <- data.frame("tabla" =c("brecha_IOP_calif_df",
                                         "brecha_IOP_hr_calif_df",
                                         "brecha_IOP_nivel_educ_df",
                                         "brecha_IOP_hr_nivel_educ_df"),
                              
                              "cod" =c("brecha.IOP.calif",
                                       "brecha.IOP.hr.calif",
                                       "brecha.IOP.nivel.educ",
                                       "brecha.IOP.hr.nivel.educ"),
                              
                              "nombre"= c("Ingreso mensual de la Ocupación Principal",
                                          "Ingreso horario de la Ocupación Principal",
                                          "Ingreso mensual de la Ocupación Principal",
                                          "Ingreso horario de la Ocupación Principal"),
                              
                              "variable_desag" = c("CALIFICACION", "CALIFICACION", "NIVEL_EDUCATIVO","NIVEL_EDUCATIVO"),
                              "variable_desag_nombre" = c("Calificación", "Calificación", "Nivel educativo","Nivel educativo"))


v1 <- as.character(unique(tabla_resultados[["brecha_IOP_calif_df"]]$CALIFICACION))
v2 <- as.character(unique(tabla_resultados[["brecha_IOP_nivel_educ_df"]]$NIVEL_EDUCATIVO))


opciones_actualizacion<- data.frame("Calificación" = v1,"Nivel educativo" = v2, "id" = c(1,2,3,4)
               ) %>% 
  pivot_longer(!id,names_to = "variable", values_to = "valores")

opciones_actualizacion$variable[opciones_actualizacion$variable =="Nivel.educativo"] <- "Nivel educativo"


trimestres <- tabla_resultados[[(nombres_brechas_desag$tabla[1])]] %>% #ungroup() %>% 
  mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                          levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
  select(periodo) %>% unique()

trimestres <- trimestres$periodo



brechas_desag_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
    
    colores <-  c("#FE1764", "#00BDD6", "black")
    
    
    
    #armar_tabla(tabla_resultados[["brecha_IOP_calif_df"]],"brecha.IOP.calif","CALIFICACION", nombre_facet="Calificación","16T2","19T2")
    
    armar_tabla <- function(dataframe,
                            brecha,
                            facet_var,
                            nombre_facet,
                            valores,
                            periodo_i,
                            periodo_f
    ){
      
      datagraf1 <- dataframe %>% 
        
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        rename("brecha" = brecha,
               "var_filtro" = facet_var) 
      
      
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        select(-periodo) %>% 
      
        filter(var_filtro %in% c(valores)) %>% 
        
        rename("Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               "Mujeres (Ingreso medio)"="media.mujeres",
               "Varones (Ingreso medio)"="media.varones", 
               "Brecha" = "brecha")
      
      str_nombre <- paste0(nombre_facet)
      
      colnames(datagraf)[3] <- str_nombre
      
      datagraf
      
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0(nombre_variable ," desde ", periodo_i, " hasta ", periodo_f)
    }
    
    
   
    
    
    
    #### versión sin los paneles
    
    plot <- function(base,var,facet_var,
                     valores, 
                     nombre,periodo_i, periodo_f){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        mutate(x = (media.mujeres+media.varones)/2) %>% 
        rename("brecha" = var) %>% 
        rename("var_facet" = facet_var)%>% 
        filter(var_facet %in% c(valores))
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      
      fig <- plot_ly(tabla, color = I("gray80"))
      fig <- fig %>% add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
      fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0(brecha,"%"), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) 
      fig <- fig %>% add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Mujeres</b>','</br>',var_facet,'</br>$',round(media.mujeres,0)))
      fig <- fig %>% add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                                 hoverinfo = 'text',
                                 text = ~paste0('</br><b>Varones</b>','</br>',var_facet,'</br>$',round(media.mujeres,0)))
      
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
    
    
    
    ####
    
    
    
    generar_titulo <- function(nombre,facet_var, valores,periodo_i, periodo_f){
      titulo <- paste0('</br>',"<b>","<font size='+2'>","Brechas de ", nombre , " por ", facet_var,".","</font>", '</br>',"Desde ", periodo_i, " hasta ", periodo_f,"</b>")
      titulo
    }
    
    generar_subtitulo <- function(valores){
      stitulo <- paste0("<b>","<font size='+1'>",valores,"</font>","</b>")
      stitulo
    }
    
    
    observe({
      x <- input$var_desag_id
      
      options = opciones_actualizacion %>%
        filter(variable %in% x) %>%
        pull(valores)
      
      
      updateSelectInput(session, 'valores_id',
                        choices = options,
                        selected = options[1])
    })
    
    
    
    
    observe({
      
      if(length(input$valores_id) == 1){
        output$st1 <- renderText({generar_subtitulo(input$valores_id[1])})
        output$plot1 <- renderPlotly({
          
          plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
               
               
               var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
               facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
               valores=input$valores_id[1],
               nombre =input$ingreso_id,
               input$id_periodo[1],
               input$id_periodo[2]) 
        })
        
        output$st2 <- NULL
        output$plot2 <- NULL
        
        output$st3 <- NULL
        output$plot3 <- NULL
        
        output$st4 <- NULL
        output$plot4 <- NULL
      }
      
    
    else if(length(input$valores_id) == 2){
      output$st1 <- renderText({generar_subtitulo(input$valores_id[1])})
      output$plot1 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[1],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st3 <- NULL
      output$plot3 <- NULL
      
      output$st4 <- NULL
      output$plot4 <- NULL
    } 
    
    else if(length(input$valores_id) == 3){
      
      output$st1 <- renderText({generar_subtitulo(input$valores_id[1])})
      output$plot1 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[1],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st3 <- renderText({generar_subtitulo(input$valores_id[3])})
      output$plot3 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[3],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st4 <- NULL
      output$plot4 <- NULL
    }
    
    else if(length(input$valores_id) == 4){
      
      output$st1 <- renderText({generar_subtitulo(input$valores_id[1])})
      output$plot1 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[1],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st3 <- renderText({generar_subtitulo(input$valores_id[3])})
      output$plot3 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[3],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
      
      output$st4 <- renderText({generar_subtitulo(input$valores_id[4])})
      output$plot4 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[4],
             nombre =input$ingreso_id,
             input$id_periodo[1],
             input$id_periodo[2]) 
      })
    }
    
    })
    
   
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
                  
                  brecha =unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id &nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                  
                  facet_var=unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                  
                  nombre_facet = input$var_desag_id,
                  valores = input$valores_id,
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    
    
    
    
    
  })
}






brechas_desag_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Brechas de ingresos - desagregado',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso:',
                           choices = unique(nombres_brechas_desag$nombre),
                           selected = unique(nombres_brechas_desag$nombre)[1],
                           multiple = FALSE),
               selectInput(ns('var_desag_id'),label = 'Elegir desagregación:',
                           choices = unique(nombres_brechas_desag$variable_desag_nombre),
                           selected = NULL,#unique(nombres_brechas_desag$variable_desag_nombre)[1],
                           multiple = FALSE),
               #conditionalPanel(condition = "input.var_desag_id=='Calificación'",
               selectInput(ns('valores_id'),label = 'Elegir valores:',
                           choices = NULL,
                           selected = NULL,
                           multiple = T)
              # )
               ,
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = c("16T2","19T2"))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico",
                        value = "g_br_des",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1')),
                        br(),
                        box(width = NULL, htmlOutput(ns('st1'))), 
                        br(),
                        plotlyOutput(ns('plot1'), height = 400),
                        br(),
                        box(width = NULL, htmlOutput(ns('st2'))), 
                        br(),
                        plotlyOutput(ns('plot2'), height = 400),
                        br(),
                        box(width = NULL, htmlOutput(ns('st3'))), 
                        br(),
                        plotlyOutput(ns('plot3'), height = 400),
                        br(),
                        box(width = NULL, htmlOutput(ns('st4'))), 
                        br(),
                        plotlyOutput(ns('plot4'), height = 400)
                        
                        ),
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_br_des",
                        
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





#Guardo código anterior por si me arrepiento


#plot(tabla_resultados[["brecha_IOP_calif_df"]],"brecha.IOP.calif","CALIFICACION", "Ingreso mensual de la Ocupación Principal","16T2","19T2")

###Versión con los paneles

# plot <- function(base,var, facet_var,
#                  valores, 
#                  nombre,periodo_i, periodo_f){
#   
#   
#   datagraf1 <- base %>% 
#     mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
#                             levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
#     mutate(x = (media.mujeres+media.varones)/2) %>%
#     rename("brecha" = var) %>% 
#     rename("var_facet" = facet_var)
#   
#   
#   tabla <- datagraf1%>% 
#     filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
#     filter(var_facet %in% c(valores))
#   
#   
#   tabla%>%   
#     
#     group_by(var_facet) %>% 
#     do(p=plot_ly(., color = I("gray80")) %>% 
#          add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)%>% 
#          add_text(x = ~x, y = ~periodo, text =~paste0(brecha,"%"), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) %>% 
#          add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
#                      hoverinfo = 'text',
#                      text = ~paste0('</br><b>Mujeres</b>','</br>',var_facet,'</br>$',round(media.mujeres,0))) %>% 
#          add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
#                      hoverinfo = 'text',
#                      text = ~paste0('</br><b>Varones</b>','</br>',var_facet,'</br>$',round(media.varones,0))) %>% 
#          add_text(#xref='x domain',
#            #yref='y domain',
#            x=15000,
#            y=trimestres[(as.integer(unique(datagraf1$periodo[datagraf1$periodo == periodo_f])) + 1)],
#            color = I(colores[3]),
#            text=~paste0('<b>',var_facet,'</b>'), 
#            showlegend = FALSE, 
#            hoverinfo='skip'
#            #showarrow=F,
#            #row=3, col=1
#          )%>% 
#          add_text(x=5000,
#                   y=trimestres[(as.integer(unique(datagraf1$periodo[datagraf1$periodo == periodo_f])) + 2)],
#                   text="", 
#                   showlegend = FALSE, 
#                   hoverinfo='skip'
#                   
#          )
#        
#        
#        %>% 
#          layout(
#            title = "",
#            xaxis = list(title = str_wrap(nombre,20)),
#            yaxis = list(title = "Período"
#            ),
#            margin = list(l = 65),
#            showlegend = F,
#            font = list(family = "Times New Roman")
#          )
#        
#     ) %>% 
#     subplot(nrows = length(unique(datagraf1$var_facet)), 
#             shareX = TRUE, shareY = TRUE)
#   
# }



