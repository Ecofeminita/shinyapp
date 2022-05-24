library(tidyverse)
library(ggalt)
library(plotly)

options(scipen = 9999)

colores = c("#FE1764", "#00BDD6")

tabla_resultados$brecha_ITI_df

plot <- function(base){
  
  tabla <- base %>% 
    mutate(periodo = factor(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE),         
                            levels = unique(paste0(substr(ANO4, 3, 4), "T", TRIMESTRE)))) %>% 
    mutate(x = (media.mujeres+media.varones)/2)
  
  
  fig <- plot_ly(tabla, color = I("gray80"))
  fig <- fig %>% add_segments(x = ~media.mujeres, xend = ~media.varones, y = ~periodo, yend = ~periodo, showlegend = FALSE)
  fig <- fig %>% add_text(x = ~x, y = ~periodo, text =~paste0(brecha.ITI,"%"), name = "Brecha", hoverinfo='skip', showlegend = F) 
  fig <- fig %>% add_markers(x = ~media.mujeres, y = ~periodo, name = "Mujeres", color = I(colores[1]),
                             hoverinfo = 'text',
                             text = ~paste0("$",round(media.mujeres,0)))
  fig <- fig %>% add_markers(x = ~media.varones, y = ~periodo, name = "Varones", color = I(colores[2]),
                             hoverinfo = 'text',
                             text = ~paste0("$",round(media.varones,0)))
  
  fig <- fig %>% layout(
    title = "",
    xaxis = list(title = "Ingreso Total Individual"),
    yaxis = list(title = ""),
    margin = list(l = 65)
  )
  
  fig
  
}






