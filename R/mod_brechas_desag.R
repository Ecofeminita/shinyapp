library(tidyverse)
library(plotly)

options(scipen = 9999)



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


trimestres <- tabla_resultados[[(nombres_brechas_desag$tabla[1])]] %>% ungroup() %>% 
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
                            #valores,
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
      #%>% 
        #filter(var_filtro %in% c(valores))
        
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
    
    
    #plot(tabla_resultados[["brecha_IOP_calif_df"]],"brecha.IOP.calif","CALIFICACION", "Ingreso mensual de la Ocupación Principal","16T2","19T2")
    
    
    plot <- function(base,var, facet_var,
                     #valores, 
                     nombre,periodo_i, periodo_f){
      
      
      datagraf1 <- base %>% 
        mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                                levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
        mutate(x = (media.mujeres+media.varones)/2) %>%
        rename("brecha" = var) %>% 
        rename("var_facet" = facet_var)
      
      
      tabla <- datagraf1%>% 
        filter(as.integer(periodo) %in% c(as.integer(datagraf1$periodo[datagraf1$periodo == periodo_i]):as.integer(datagraf1$periodo[datagraf1$periodo == periodo_f])))# %>% 
       # filter(var_facet %in% c(valores))
      
      
      tabla%>%   
        
        group_by(var_facet) %>% 
        do(p=plot_ly(., color = I("gray80")) %>% 
             add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, alpha = .3, showlegend = FALSE)%>% 
             add_text(x = ~x, y = ~periodo, text =~paste0(brecha,"%"), name = "Brecha", color = I(colores[3]), hoverinfo='skip', showlegend = F) %>% 
             add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                         hoverinfo = 'text',
                         text = ~paste0('</br><b>Mujeres</b>','</br>',var_facet,'</br>$',round(media.mujeres,0))) %>% 
             add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                         hoverinfo = 'text',
                         text = ~paste0('</br><b>Varones</b>','</br>',var_facet,'</br>$',round(media.varones,0))) %>% 
             add_text(#xref='x domain',
                      #yref='y domain',
                      x=15000,
                      y=trimestres[(as.integer(unique(datagraf1$periodo[datagraf1$periodo == periodo_f])) + 1)],
                      color = I(colores[3]),
                      text=~paste0('<b>',var_facet,'</b>'), 
                      showlegend = FALSE, 
                      hoverinfo='skip'
                      #showarrow=F,
                      #row=3, col=1
             )%>% 
             add_text(#xref='x domain',
                      #yref='y domain',
                      x=5000,
                      y=trimestres[(as.integer(unique(datagraf1$periodo[datagraf1$periodo == periodo_f])) + 2)],
                      text="", 
                      showlegend = FALSE, 
                      hoverinfo='skip'
                      #showarrow=F,
                      #row=3, col=1
             )
           
           
           %>% 
             layout(
               title = "",
               xaxis = list(title = str_wrap(nombre,20)),
               yaxis = list(title = "Período"
                            ),
               margin = list(l = 65),
               showlegend = F,
               font = list(family = "Times New Roman")
             )
           
        ) %>% 
        subplot(nrows = length(unique(datagraf1$var_facet)), shareX = TRUE, shareY = TRUE)
      
    }
    
    
    
    generar_titulo <- function(nombre,facet_var,periodo_i, periodo_f){
      titulo <- paste0("<b>","<font size='+2'>","Brechas de ", nombre , " por ", facet_var, ". Desde ", periodo_i, " hasta ", periodo_f,"</b>","</font>")
      titulo
    }
    
    
    output$plot <- renderPlotly({
      
      plot(base = tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
           
           
           var = unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id & nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
           facet_var =unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
           #valores=input$valores_id,
           nombre =input$ingreso_id,
           input$id_periodo[1],
           input$id_periodo[2]) 
    })
    
    
    
    
    
    output$tabla <- renderTable({
      armar_tabla(tabla_resultados[[(nombres_brechas_desag$tabla[nombres_brechas_desag$nombre == input$ingreso_id&nombres_brechas_desag$variable_desag_nombre == input$var_desag_id])]],
                  
                  brecha =unique(nombres_brechas_desag$cod[nombres_brechas_desag$nombre == input$ingreso_id &nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                  
                  facet_var=unique(nombres_brechas_desag$variable_desag[nombres_brechas_desag$variable_desag_nombre == input$var_desag_id]),
                  
                  nombre_facet = input$var_desag_id,
                 # valores = input$valores_id,
                  input$id_periodo[1],
                  input$id_periodo[2]
      )
    })
    
    output$metadata1 <- renderText({"blabla"})
    output$metadata2 <- renderText({"blabla"})
    
    output$titulo1 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, input$id_periodo[1],input$id_periodo[2])})
    output$titulo2 <- renderText({generar_titulo(input$ingreso_id,input$var_desag_id, input$id_periodo[1],input$id_periodo[2])})
    
    
    
    # observeEvent(input$var_desag_id,{
    #   updateSelectInput(session,'valores_id',choices = get(a[input$var_desag_id]))
    # })
    
    
  })
}


v1 <- as.character(unique(tabla_resultados[["brecha_IOP_calif_df"]]$CALIFICACION))
v2 <- as.character(unique(tabla_resultados[["brecha_IOP_nivel_educ_df"]]$NIVEL_EDUCATIVO))

a <- list()
a$"Calificación" <- v1
a$"Nivel educativo" <- v2
#a<- data.frame("Calificación" = v1,"Nivel educativo" = v2)



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
                           selected = unique(nombres_brechas_desag$variable_desag_nombre)[1],
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
                        plotlyOutput(ns('plot'), height = 600),
                        br(),
                        box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))
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


