library(tidyverse)
library(plotly)

options(scipen = 9999)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")

# tabla_resultados$brecha_IOP_calif_df
# tabla_resultados$brecha_IOP_hr_calif_df
# tabla_resultados$brecha_IOP_nivel_educ_df
# tabla_resultados$brecha_IOP_hr_nivel_educ_df

# #respetar orden!!!!!
# nombres_brechas_desag <- data.frame("tabla" =c("brecha_IOP_calif_df",
#                                          "brecha_IOP_hr_calif_df",
#                                          "brecha_IOP_nivel_educ_df",
#                                          "brecha_IOP_hr_nivel_educ_df"),
#                               
#                               "cod" =c("brecha.IOP.calif",
#                                        "brecha.IOP.hr.calif",
#                                        "brecha.IOP.nivel.educ",
#                                        "brecha.IOP.hr.nivel.educ"),
#                               
#                               "nombre"= c("Ingreso mensual de la Ocupación Principal",
#                                           "Ingreso horario de la Ocupación Principal",
#                                           "Ingreso mensual de la Ocupación Principal",
#                                           "Ingreso horario de la Ocupación Principal"),
#                               
#                               "variable_desag" = c("CALIFICACION", "CALIFICACION", "NIVEL_EDUCATIVO","NIVEL_EDUCATIVO"),
#                               "variable_desag_nombre" = c("Calificación", "Calificación", "Nivel educativo","Nivel educativo"))
# 
# 
# v1 <- as.character(unique(tabla_resultados[["brecha_IOP_calif_df"]]$CALIFICACION))
# v2 <- as.character(unique(tabla_resultados[["brecha_IOP_nivel_educ_df"]]$NIVEL_EDUCATIVO))
# 
# 
# opciones_actualizacion<- data.frame("Calificación" = v1,"Nivel educativo" = v2, "id" = c(1,2,3,4)
#                ) %>% 
#   pivot_longer(!id,names_to = "variable", values_to = "valores")
# 
# opciones_actualizacion$variable[opciones_actualizacion$variable =="Nivel.educativo"] <- "Nivel educativo"
# 
# 
# trimestres <- tabla_resultados[[(nombres_brechas_desag$tabla[1])]] %>% #ungroup() %>% 
#   mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
#                           levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
#   select(periodo) %>% unique()
# 
# trimestres <- trimestres$periodo



brechas_desag_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    colores_b <-  c("#FE1764", "#00BDD6", "black")
    
    colores_g <- c("#e5616e","#e9c1d0","#fbd17e","#8594c6","#8cddd3")
    
    
    
    #armar_tabla(tabla_resultados[["brecha_IOP_calif_df"]],"brecha.IOP.calif","CALIFICACION", nombre_facet="Calificación","16T2","19T2")
    
    armar_tabla <- function(dataframe,
                            brecha,
                            facet_var,
                            nombre_facet,
                            valores,
                            periodo_i,
                            periodo_f,
                            valuacion
    ){
      
      datagraf1 <- dataframe %>% 
        
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        rename("brecha" = brecha,
               "var_filtro" = facet_var) 
      
      
      
      datagraf <- datagraf1%>% 
        
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) %>% 
        select(-periodo#,-nombre_trim_base
               ) %>% 
      
        filter(var_filtro %in% c(valores)) %>% 
        
        rename("Año" = "ANO4", 
               "Trimestre" = "TRIMESTRE", 
               "Brecha (%)" = "brecha",
               "Mujeres (Ingreso medio - precios corrientes)"="media.mujeres",
               "Varones (Ingreso medio - precios corrientes)"="media.varones", 
               "Mujeres (Ingreso medio - precios constantes)"="cte_media.mujeres",
               "Varones (Ingreso medio - precios constantes)"="cte_media.varones")
      
      str_nombre <- paste0(nombre_facet)
      
      colnames(datagraf)[3] <- str_nombre
      
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
   
    
   plot_general <- function(base,var,facet_var,
                            valores, 
                            nombre,
                            porcentaje,
                            periodo_i, periodo_f){
     
     datagraf1 <- base %>% 
       mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                               levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
       rename("brecha" = var) %>% 
       rename("var_facet" = facet_var)%>% 
       filter(var_facet %in% c(valores))
     
     tabla <- datagraf1%>% 
       filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
     
     
     grafico <- ggplot(tabla, aes(periodo, brecha, color = var_facet, group = var_facet
                                     ,text=paste0('</br>',var_facet,'</br>Brecha: ',brecha,'%', '</br>Período: ',periodo)
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
       scale_color_manual(values = colores_g) +
       labs(#title = titulo,
            #subtitle = subtitulo,
            x = "Período",
            y = paste0("Brecha de ",nombre),
            color = "",
            #caption = "Fuente: Elaboración propia en base a EPH-INDEC"
            )
     
     
     if(porcentaje){
       grafico <- grafico + 
         scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,60))    
     }
     
     grafico <- ggplotly(grafico, tooltip = c("text")) %>% layout(font = list(family = "Times New Roman"))
     
     return(grafico)
     
     
     
   }
    
    
    plot <- function(base,var,facet_var,
                     valores, 
                     nombre,periodo_i, periodo_f,
                     valuacion){
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(TRIMESTRE, "°T ",ANO4),         
                                levels = unique(paste0(TRIMESTRE, "°T ",ANO4)))) %>% 
        mutate(x = (media.mujeres+media.varones)/2) %>% 
        mutate(cte_x = (cte_media.mujeres+cte_media.varones)/2) %>% 
        mutate(z = media.varones-media.mujeres) %>% 
        rename("brecha" = var) %>% 
        rename("var_facet" = facet_var)%>% 
        filter(var_facet %in% c(valores))
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f]))) 
      
      if(valuacion =="Precios corrientes"){
        
        fig <- plot_ly(tabla, color = I("gray80"))
        fig <- fig %>% add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
        fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0("$",round(z,2)), name = "Brecha", color = I(colores_b[3]), hoverinfo='skip', showlegend = F) 
        fig <- fig %>% add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores_b[1]),
                                   hoverinfo = 'text',
                                   text = ~paste0('</br><b>Mujeres</b>','</br>',var_facet,'</br>$',round(media.mujeres,0)))
        fig <- fig %>% add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores_b[2]),
                                   hoverinfo = 'text',
                                   text = ~paste0('</br><b>Varones</b>','</br>',var_facet,'</br>$',round(media.varones,0)))
        
        fig <- fig %>% layout(
          title = "",
          xaxis = list(title = nombre),
          yaxis = list(title = "Período"),
          margin = list(l = 65),
          showlegend = T,
          font = list(family = "Times New Roman")
        )
        
        return(fig)
        
      } else if(valuacion ==paste0("Precios constantes (",nombre_trimestre_base,")")){
        
        
        fig <- plot_ly(tabla, color = I("gray80"))
        fig <- fig %>% add_segments(x = ~cte_media.mujeres, xend = ~cte_media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)
        fig <- fig %>% add_text(x = ~cte_x, y = ~periodo, text =~paste0(brecha,"%"), name = "Brecha", color = I(colores_b[3]), hoverinfo='skip', showlegend = F) 
        fig <- fig %>% add_markers(x = ~cte_media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores_b[1]),
                                   hoverinfo = 'text',
                                   text = ~paste0('</br><b>Mujeres</b>','</br>',var_facet,'</br>$',round(cte_media.mujeres,0)))
        fig <- fig %>% add_markers(x = ~cte_media.varones, y = ~periodo, name = "Varones", color = I(colores_b[2]),
                                   hoverinfo = 'text',
                                   text = ~paste0('</br><b>Varones</b>','</br>',var_facet,'</br>$',round(cte_media.varones,0)))
        
        fig <- fig %>% layout(
          title = "",
          xaxis = list(title = nombre),
          yaxis = list(title = "Período"),
          margin = list(l = 65),
          showlegend = T,
          font = list(family = "Times New Roman")
        )
        
        return(fig)
      }
      
      
      
      
    }
    
    
    
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
                        selected = options#[1]
                        )
    })
    
    
    output$plot_gral <- renderPlotly({
      
      plot_general(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
           
           
           var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
           facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
           valores=input$valores_id,
           nombre =input$ingreso_id,
           porcentaje = T, #para constantes!
           input$id_periodo[1],
           input$id_periodo[2]) 
    })
    
    observe({
      
      if(length(input$valores_id) == 1){
        output$st1 <- renderText({generar_subtitulo(input$valores_id[1])})
        output$plot1 <- renderPlotly({
          
          plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
               
               
               var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
               facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
               valores=input$valores_id[1],
               nombre =paste0(input$ingreso_id," - ",input$precios_id),
               input$id_periodo[1],
               input$id_periodo[2],
               input$precios_id) 
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
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
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
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st3 <- renderText({generar_subtitulo(input$valores_id[3])})
      output$plot3 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[3],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
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
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st2 <- renderText({generar_subtitulo(input$valores_id[2])})
      output$plot2 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[2],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st3 <- renderText({generar_subtitulo(input$valores_id[3])})
      output$plot3 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[3],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
      })
      
      output$st4 <- renderText({generar_subtitulo(input$valores_id[4])})
      output$plot4 <- renderPlotly({
        
        plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
             
             
             var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
             valores=input$valores_id[4],
             nombre =paste0(input$ingreso_id," - ",input$precios_id),
             input$id_periodo[1],
             input$id_periodo[2],
             input$precios_id) 
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
                  input$id_periodo[2],
                  input$precios_id
      )
    },
    width="650px")
    
    output$metadata <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("B-",input$ingreso_id)]})
    output$metadata_desag <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == input$var_desag_id]})
    output$metadata_ingresos <- renderText({tabla_metadata$metadata[tabla_metadata$indicador == paste0("Valuación")]})
    
    # output$interpretacion_horas <- renderText({paste0("<font size='+1'>Para interpretar estos resultados, estudiemos el <b>Uso del tiempo</b> de cada segmento de la población.</font>")})
    # output$interpretacion_horas_1 <- renderText({paste0("<font size='+1'>Para interpretar estos resultados, estudiemos el <b>Uso del tiempo</b> de cada segmento de la población.</font>")})
    
    output$titulo0 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, valores = input$valores_id,input$id_periodo[1],input$id_periodo[2])})
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste('Brecha_',input$ingreso_id,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
                               
                               brecha =unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id &nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                               
                               facet_var=unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                               
                               nombre_facet = input$var_desag_id,
                               valores = input$valores_id,
                               input$id_periodo[1],
                               input$id_periodo[2],
                               input$precios_id
        )
      , 
                   file)    }
    )
    
    
    
  })
}






brechas_desag_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = 'Brechas de ingresos - desagregado',
           
           titlePanel('Brechas de ingresos - desagregado'),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('ingreso_id'),label = 'Elegir tipo de ingreso:',
                           choices = unique(nombres_brechas_desag$nombre),
                           selected = unique(nombres_brechas_desag$nombre)[1],
                           multiple = FALSE),
               selectInput(ns('precios_id'),label = 'Valuación:',
                           choices = c("Precios corrientes", paste0("Precios constantes (",nombre_trimestre_base,")")),
                           selected = paste0("Precios constantes (",nombre_trimestre_base,")"),
                           multiple = FALSE),
               selectInput(ns('var_desag_id'),label = 'Elegir desagregación:',
                           choices = unique(nombres_brechas_desag$variable_desag_nombre),
                           selected = NULL,
                           multiple = FALSE),
               selectInput(ns('valores_id'),label = 'Elegir valores:',
                           choices = NULL,
                           selected = NULL,
                           multiple = T)
              
               ,
               sliderTextInput(ns('id_periodo'), "Trimestre:", choices = trimestres, selected = trimestres[c(1,length(trimestres))]),
               
               br(), 
               hr(), 
               h4("Sobre el indicador"), 
               h5(textOutput(ns('metadata'))), 
               hr(),
               h5(textOutput(ns('metadata_desag'))), 
               hr(),
               h5(textOutput(ns('metadata_ingresos')))
               
             ),
             mainPanel( tabsetPanel(
               
               tabPanel("Gráfico-Comparación",
                        value = "g_br_des_gral",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo0'))), 
                        br(),
                       
                        
                        tags$div( style="display: inline-flex;",  id = "div_1",
                                  tags$p('Para interpretar estos resultados, estudiemos el '), 
                                  HTML('&nbsp;'),
                                  tags$a(" Uso del tiempo", 
                                         id = "bd_1",
                                         onclick="fakeClick('Trabajo no remunerado')"#,
                                         
                                  ),
                                  HTML('&nbsp;'),
                                  tags$p(' de cada segmento de la población')
                        ),
                        br(),
                        br(),
                        box(width = NULL, plotlyOutput(ns('plot_gral'), height = 500)%>% withSpinner(type = 5, color ="#e5616e"),
                            
                        )
                        
                        
                        
                        
               ),
               
               
               tabPanel("Gráfico-Desagregado",
                        value = "g_br_des",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo1'))), 
                        br(),
                       
                        box(width = NULL, htmlOutput(ns('st1'))), 
                        br(),
                        plotlyOutput(ns('plot1'), height = 400)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        box(width = NULL, htmlOutput(ns('st2'))), 
                        br(),
                        plotlyOutput(ns('plot2'), height = 400)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        box(width = NULL, htmlOutput(ns('st3'))), 
                        br(),
                        plotlyOutput(ns('plot3'), height = 400)%>% withSpinner(type = 5, color ="#e5616e"),
                        br(),
                        box(width = NULL, htmlOutput(ns('st4'))), 
                        br(),
                        plotlyOutput(ns('plot4'), height = 400)%>% withSpinner(type = 5, color ="#e5616e")
                        
                        
                        
                        
               ),
               
               tabPanel("Tabla",
                        value = "t_br_des",
                        
                        br(),
                        box(width = NULL, htmlOutput(ns('titulo2'))), 
                        br(),
                       
                        
                        
                        tags$div( style="display: inline-flex;",  id = "div_2",
                                  tags$p('Para interpretar estos resultados, estudiemos el '), 
                                  HTML('&nbsp;'),
                                  tags$a(" Uso del tiempo", 
                                         id = "bd_2",
                                         onclick="fakeClick('Trabajo no remunerado')"#,
                                         
                                  ),
                                  HTML('&nbsp;'),
                                  tags$p(' de cada segmento de la población')
                        ),
                        
                        
                        
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





